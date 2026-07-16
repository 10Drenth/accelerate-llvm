#include "types.h"
#include "util.h"

struct NodeWriteOutputParams{
    void *done_mvar;
    void *read_adress;
    void *write_adress;
    size_t bytesize;
};

struct KernelArguments{
    uint32_t *argument_indices;
    size_t argument_count;
};

void CUDA_CB node_write_output(void *arguments){
    struct NodeWriteOutputParams *params = (struct NodeWriteOutputParams *)arguments;
    memcpy(params->write_adress, params->read_adress, params->bytesize);
    printf("\n\nWriting output from the host node (with bytesize %ld).\n", params->bytesize);
    uint32_t output_size = params->bytesize / 8;
    printf("\nOuput value: size = %d, value = ", output_size);
    for (uint32_t c = 0; c < output_size; c++){
        printf("%lu", ((uint64_t* )params->write_adress)[c]);
    }
    hs_try_putmvar(-1, params->done_mvar);
    printf("\nSuccesfully put the mvar");
}

void run_graph
    ( uint32_t node_count
    , uint32_t *node_dependency_counts
    , uint32_t **node_dependencies
    , struct NodeContent *node_contents
    , uint32_t max_sizes
    , uint32_t *size_indices
    , uint32_t allocation_count
    , uint32_t mem_bytesize
    , uint32_t *mem_offsets
    , int8_t *mem_types
    , uint32_t *input_bytesizes
    , char **input_data
    , char **output_data
    , void **output_mvars
    , void *done_mvar )
{
    printf("Hello from c! The nodecount is %d\n", node_count);
    printf("\nSize of a pointer %lu\n", sizeof(char*));
    printf("\nSize of a device pointer %lu\n", sizeof(CUdeviceptr));
    printf("\nSome nodecontent info: size %lu, alignment: %lu\n", sizeof(struct NodeContent), _Alignof(struct NodeContent));
    printf("\nSome content content info: size %lu, alignment: %lu\n", sizeof(node_contents[0].content), _Alignof(sizeof(node_contents[0].content)));
    printf("\nSome kernel content info: size %lu, alignment: %lu\n", sizeof(node_contents[0].content.kernel), _Alignof(sizeof(node_contents[0].content.kernel)));
    printf("\nSome kernelphase info: size %lu, alignment: %lu\n", sizeof(struct KernelPhase), _Alignof(struct KernelPhase));
    printf("\nSome AllocData info: size %lu, alignment: %lu\n", sizeof(struct AllocData), _Alignof(struct AllocData));

    printf("\nSome mem info: total size: %d, devpointer alignment: %lu \n", mem_bytesize, _Alignof(CUdeviceptr));


    printf("\n\nStarting sort: ");
    // Topological sort (naive implementation)
    bool all_sorted = false;
    size_t* ordering = malloc(node_count * sizeof(size_t));
    bool* visited = malloc(node_count * sizeof(bool));
    for (size_t i = 0; i < node_count; i++)
    {
        ordering[i] = -1;
        visited[i] = false;
    }
    uint32_t cursor = 0;

    while (!all_sorted){

        for (size_t i = 0; i < node_count; i++)
        {
            if (visited[i]) continue;

            uint32_t dep_count = node_dependency_counts[i];
            uint32_t* deps = node_dependencies[i];

            bool any_unvisited_dependency = false;
            for (size_t d = 0; d < dep_count; d++)
            {
                if (!visited[deps[d]]) {
                    any_unvisited_dependency = true;
                    break;
                }
            }
            if (any_unvisited_dependency) continue;

            visited[i] = true;
            ordering[cursor] = i;
            cursor += 1;
            printf("%ld, ", i);
            if (cursor == node_count){
                all_sorted = true;
                break;
            }
        }
    }
    printf("\nDone!");
    

    // Start driver interactions
    CU_CHECK(cuInit(0));
    int cuda_version = 0;
    CU_CHECK(cuDriverGetVersion(&cuda_version));
    printf("\nDriver version is %d", cuda_version);
    // Get number of devices supporting CUDA
    int deviceCount = 0;
    CU_CHECK(cuDeviceGetCount(&deviceCount));
    if (deviceCount == 0) {
        printf("There is no device supporting CUDA.\n");
        exit (0);
    }

    // Get handle for device 0
    CUdevice cuDevice;
    CU_CHECK(cuDeviceGet(&cuDevice, 0));

    // Create context
    CUcontext cuContext;
    CU_CHECK(cuCtxCreate(&cuContext, 0, cuDevice));
    // CU_CHECK(cuDevicePrimaryCtxRetain(&cuContext, cuDevice));
    

    CUgraph graph;
    CU_CHECK(cuGraphCreate(&graph, 0));

    CUdeviceptr mem_ptr_d;
    CU_CHECK(cuMemAlloc(&mem_ptr_d, mem_bytesize));
    char *mem = (char *)mem_ptr_d;

    printf("\nAllocating device pointers");
    CUdeviceptr *dev_pointers = (CUdeviceptr *)malloc(allocation_count * sizeof(CUdeviceptr));

    printf("\nAllocating sizes array, max sizes: %u", max_sizes);
    CUdeviceptr sizes_d;
    CU_CHECK(cuMemAlloc(&sizes_d, (max_sizes + 1) * sizeof(CUdeviceptr)));
    uint32_t *sizes = (uint32_t *)sizes_d;

    struct BuildState state;
    memset(&state, 0, sizeof(state));
    state.graph = graph;
    state.ctx = cuContext;
    state.sizes = sizes;
    state.mem = mem;
    state.mem_offsets = mem_offsets;
    state.size_indices = size_indices;
    state.bytesizes = input_bytesizes;

    printf("\nDone allocating device pointers");

    size_t output_node_count = 0;
    for (size_t i = 0; i < node_count; i++){
        size_t node_index = ordering[i];
        struct NodeContent content = node_contents[node_index];
        if (content.node_type == 2){
            output_node_count += 1;
        }
    }

    struct NodeWriteOutputParams *outputParams = malloc(output_node_count * sizeof(struct NodeWriteOutputParams));

    size_t output_node_index = 0;
    CUgraphNode *nodes = malloc(node_count * sizeof(CUgraphNode));

        
    for (size_t i = 0; i < node_count; i++)
    {
        printf("\n\n");
        printf("\nNode %ld: ", ordering[i]);
        size_t node_index = ordering[i];
        struct NodeContent content = node_contents[node_index];
        state.current_node = malloc(sizeof(CUgraphNode));
        state.dependency_count = (size_t)node_dependency_counts[node_index];
        state.dependencies = malloc(state.dependency_count * sizeof(CUgraphNode));

        printf("\nDependencies: ");
        for (uint32_t j = 0; j < state.dependency_count; j++){
            uint32_t dependency_index = node_dependencies[node_index][j];
            printf("%d, ", dependency_index);
            state.dependencies[j] = nodes[dependency_index];
        }



        switch (content.node_type)
        {
        case NODE_COPY:
            printf("\nCopy nodes are deprecated");
            exit(1);
            CUDA_MEMCPY3D cpy_params_cpy;
            uint32_t cp_id1 = content.content.general.alloc_1;
            uint32_t cp_id2 = content.content.general.alloc_2;
            memset(&cpy_params_cpy, 0, sizeof(cpy_params_cpy));
            printf("Copy from %d to %d", cp_id1, cp_id2);
            cpy_params_cpy.srcMemoryType = CU_MEMORYTYPE_DEVICE;
            
            // cpy_params_cpy.srcDevice = (CUdeviceptr)(state.mem + state.mem_offsets[cp_id1]);
            // if (mem_types[cp_id1] == MEM_BUFFER){
            //     cpy_params_cpy.srcDevice = state.dev_pointers[cp_id1];
            // }
            // cpy_params_cpy.dstDevice = (CUdeviceptr)(state.mem + state.mem_offsets[cp_id2]);
            // if (mem_types[cp_id2] == MEM_BUFFER){
            //     cpy_params_cpy.dstDevice = state.dev_pointers[cp_id2];
            // }

            cpy_params_cpy.srcPitch = (size_t)state.bytesizes[cp_id1];
            cpy_params_cpy.srcHeight = 1;
            cpy_params_cpy.dstMemoryType = CU_MEMORYTYPE_DEVICE;
            cpy_params_cpy.dstPitch = (size_t)state.bytesizes[cp_id2];
            cpy_params_cpy.dstHeight = 1;
            cpy_params_cpy.WidthInBytes = (size_t)state.bytesizes[cp_id2];
            cpy_params_cpy.Height = 1;
            cpy_params_cpy.Depth = 1;
            CU_CHECK(cuGraphAddMemcpyNode(state.current_node, state.graph, state.dependencies, state.dependency_count, &cpy_params_cpy, state.ctx));
            break;
        
        case NODE_INPUT:
            CUDA_MEMCPY3D cpy_params_input;
            memset(&cpy_params_input, 0, sizeof(cpy_params_input));
            uint32_t input_alloc = content.content.general.alloc_1;

            
            uint32_t input_size = state.bytesizes[input_alloc] / 8;
            printf("Input to %d", input_alloc);
            printf("\nInput value: size = %d, value = ", input_size);
            for (uint32_t c = 0; c < input_size; c++){
                printf("%lu", ((uint64_t* )input_data[input_alloc])[c]);
            }
            
            CUdeviceptr init_dest_ptr = (CUdeviceptr)(state.mem + state.mem_offsets[input_alloc]);

            printf("\n PreAllocating device pointer %u", input_alloc);
            if (mem_types[input_alloc] == MEM_BUFFER){
                printf("\n%u Is a buffer of size %u", input_alloc, state.bytesizes[input_alloc]);
                CUdeviceptr alloc_ptr;
                CU_CHECK(cuMemAlloc(&alloc_ptr, state.bytesizes[input_alloc]));
                printf("\nMem adress (device): %llu", mem_ptr_d);
                printf("\nOffset: %d", state.mem_offsets[input_alloc]);
                printf("\nDestination adress: %llu", init_dest_ptr);
                CU_CHECK(cuMemcpyHtoD((CUdeviceptr)(state.mem + state.mem_offsets[input_alloc]), (void *)&alloc_ptr, sizeof(CUdeviceptr)));
                
                printf("\nSize index: %u", state.size_indices[input_alloc]);
                CUdeviceptr size_d = (CUdeviceptr)(state.sizes + state.size_indices[input_alloc] * sizeof(uint32_t));
                CU_CHECK(cuMemcpyHtoD(size_d, (void *)&state.bytesizes[input_alloc], sizeof(uint32_t)));
            }   

            cpy_params_input.srcMemoryType = CU_MEMORYTYPE_HOST;
            cpy_params_input.srcHost = (void *)input_data[input_alloc];
            cpy_params_input.srcPitch = (size_t)state.bytesizes[input_alloc];
            cpy_params_input.srcHeight = 1;
            cpy_params_input.dstMemoryType = CU_MEMORYTYPE_DEVICE;
            cpy_params_input.dstDevice = init_dest_ptr;
            cpy_params_input.dstPitch = (size_t)state.bytesizes[input_alloc];
            cpy_params_input.dstHeight = 1;
            cpy_params_input.WidthInBytes = (size_t)state.bytesizes[input_alloc];
            cpy_params_input.Height = 1;
            cpy_params_input.Depth = 1;
            CU_CHECK(cuGraphAddMemcpyNode(state.current_node, state.graph, state.dependencies, state.dependency_count, &cpy_params_input, state.ctx));
            printf("\nInput End");
            break;
        case NODE_OUTPUT:
            // CUgraphNode copy_node;

            // uint32_t output_alloc = content.content.general.alloc_1;

            // state.bytesizes[output_alloc] = 8;
            // void *output_adress;
            // CU_CHECK(cuMemAllocHost(&output_adress, state.bytesizes[output_alloc]));
            
            // CUdeviceptr output_device_src = (CUdeviceptr)(state.mem + state.mem_offsets[output_alloc]);
            // if (mem_types[output_alloc] == MEM_BUFFER){
            //     output_device_src = state.dev_pointers[output_alloc];
            // }

            // CUDA_MEMCPY3D cpy_params_output;
            // memset(&cpy_params_output, 0, sizeof(cpy_params_output));
            // printf("Output from %d, size is %d", output_alloc, state.bytesizes[output_alloc]);
            // cpy_params_output.srcMemoryType = CU_MEMORYTYPE_DEVICE;
            // cpy_params_output.srcDevice = output_device_src;
            // cpy_params_output.srcPitch = (size_t)state.bytesizes[output_alloc];
            // cpy_params_output.srcHeight = 1;
            // cpy_params_output.dstMemoryType = CU_MEMORYTYPE_HOST;
            // cpy_params_output.dstHost = output_adress;
            // cpy_params_output.dstPitch = (size_t)state.bytesizes[output_alloc];
            // cpy_params_output.dstHeight = 1;
            // cpy_params_output.WidthInBytes = (size_t)state.bytesizes[output_alloc];
            // cpy_params_output.Height = 1;
            // cpy_params_output.Depth = 1;
            // CU_CHECK(cuGraphAddMemcpyNode(&copy_node, state.graph, state.dependencies, state.dependency_count, &cpy_params_output, state.ctx));

            // CUDA_HOST_NODE_PARAMS hostParams;
            // memset(&hostParams, 0, sizeof(hostParams));

            // memset(&outputParams[output_node_index], 0, sizeof(struct NodeWriteOutputParams));
            // outputParams[output_node_index].bytesize = (size_t)state.bytesizes[output_alloc];
            // outputParams[output_node_index].done_mvar = output_mvars[output_alloc];
            // outputParams[output_node_index].write_adress = (void *)output_data[output_alloc];
            // outputParams[output_node_index].read_adress = output_adress;

            // hostParams.userData = (void *)(&outputParams[output_node_index]);
            // hostParams.fn = node_write_output;

            // CU_CHECK(cuGraphAddHostNode(state.current_node, state.graph, &copy_node, 1, &hostParams));
            // output_node_index += 1;
            // break;
        case NODE_EMPTY:
            printf("Empty");
            CU_CHECK(cuGraphAddEmptyNode(state.current_node, state.graph, state.dependencies, state.dependency_count));
            break;
        case NODE_ALLOC: // TODO: Remove placeholder
            printf("\nAlloc:");
            struct AllocData alloc_data = content.content.alloc_data;
            uint32_t a_arg_count = alloc_data.arg_count;
            uint32_t *a_arg_indices = alloc_data.arg_indices;
            printf("\nIterating over %d parameters: ", a_arg_count);
            for (size_t a_idx = 0; a_idx < a_arg_count; a_idx++){
                printf("%d, ", a_arg_indices[a_idx]);
            }
            printf("\nwrite destination: %u", alloc_data.write_index);
            struct KernelPhase data = content.content.alloc_data.alloc_kernel;
            printf("\nKernel module: %s", data.module_path);
            printf("\nKernel symbol: %s", data.symbol);

            printf("\n\nLoading alloc kernel");
            CUDA_KERNEL_NODE_PARAMS alloc_node_params = load_ptx_kernel(state, &data);
    
            printf("\nAllocating input dims struct");
            size_t dims_ptr_struct_size = a_arg_count * sizeof(CUdeviceptr);
            CUdeviceptr dims_idxs_d;
            if (a_arg_count > 0){
                CU_CHECK(cuMemAlloc(&dims_idxs_d, dims_ptr_struct_size));
                CUdeviceptr *dims_idxs_h = malloc(dims_ptr_struct_size);

                for (size_t a_idx = 0; a_idx < a_arg_count; a_idx++)
                {   // TODO: This should account for alignment
                    dims_idxs_h[a_idx] = (CUdeviceptr)(state.mem + state.mem_offsets[a_arg_indices[a_idx]]);
                }
                CU_CHECK(cuMemcpyHtoD(dims_idxs_d, dims_idxs_h, dims_ptr_struct_size));
            }
            else{
                CU_CHECK(cuMemAlloc(&dims_idxs_d, sizeof(CUdeviceptr)));
            }
            CUdeviceptr alloc_dest = (CUdeviceptr)(state.mem + state.mem_offsets[alloc_data.write_index]);
            CUdeviceptr alloc_sz_dest = (CUdeviceptr)(state.sizes + state.size_indices[alloc_data.write_index] * sizeof(uint32_t));
            printf("\nAlloc destination: %llu", alloc_dest);

            void *alloc_args[] = {&dims_idxs_d, &alloc_dest, &alloc_sz_dest};
            alloc_node_params.kernelParams = alloc_args;

            CUdevice all_ptr;
            CU_CHECK(cuMemAlloc(&all_ptr, 8));
            CU_CHECK(cuMemcpyHtoD(alloc_dest, (void *)&all_ptr, sizeof(CUdeviceptr)));

            CU_CHECK(cuGraphAddEmptyNode(state.current_node, state.graph, state.dependencies, state.dependency_count));

            // CU_CHECK(cuGraphAddKernelNode(state.current_node, state.graph, state.dependencies, state.dependency_count, &alloc_node_params));
            break;
        case NODE_KERNEL:
            // CU_CHECK(cuGraphAddEmptyNode(state.current_node, state.graph, state.dependencies, state.dependency_count));

            add_kernel_node(state, content.content.kernel);
            break;
        default:
            break;
        }

        nodes[node_index] = *state.current_node;
    }

    printf("\n\nInstantiating graph");
    CUDA_GRAPH_INSTANTIATE_PARAMS instantiation_params;
    memset(&instantiation_params, 0, sizeof(instantiation_params));


    CUgraphExec executable_graph;
    CUresult e = cuGraphInstantiateWithParams(&executable_graph, graph, &instantiation_params);
    if (e != CUDA_SUCCESS){
        printf("\n\nGraph instantiation not succesfull:\n");
        CUgraphNodeType violating_node_type;
        switch (instantiation_params.result_out)
        {
        case CUDA_GRAPH_INSTANTIATE_ERROR:
            printf("CUDA_GRAPH_INSTANTIATE_ERROR");
            break;
        case CUDA_GRAPH_INSTANTIATE_INVALID_STRUCTURE:
            cuGraphNodeGetType(instantiation_params.hErrNode_out, &violating_node_type);
            printf("CUDA_GRAPH_INSTANTIATE_INVALID_STRUCTURE, offending node: %d", violating_node_type);
            break;
        case CUDA_GRAPH_INSTANTIATE_NODE_OPERATION_NOT_SUPPORTED:
            cuGraphNodeGetType(instantiation_params.hErrNode_out, &violating_node_type);
            printf("CUDA_GRAPH_INSTANTIATE_NODE_OPERATION_NOT_SUPPORTED, offending node: %d", violating_node_type);
            break;
        case CUDA_GRAPH_INSTANTIATE_MULTIPLE_CTXS_NOT_SUPPORTED:
            cuGraphNodeGetType(instantiation_params.hErrNode_out, &violating_node_type);
            printf("CUDA_GRAPH_INSTANTIATE_MULTIPLE_CTXS_NOT_SUPPORTED, offending node: %d", violating_node_type);
            break;
        default:
            break;
        }
        exit(1);
    }
    printf("\nInstantiation succesfull.");
    

    printf("\n\nLaunching graph.");
    CU_CHECK(cuGraphLaunch(executable_graph, 0));

    printf("\n\nSynchronizing.\n");
    CU_CHECK(cuStreamSynchronize(0));
    printf("\n\nGraph Done.\n");

    // TODO: Copy back output values

    hs_try_putmvar(-1, done_mvar);
}
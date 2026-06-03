#include "graph.h"

// Credits: https://forums.developer.nvidia.com/t/cudevicegetattribute-shows-i-can-use-fabric-handle-but-actually-i-cannot/336426
#ifndef CU_CHECK
#define CU_CHECK(cmd) \
do { \
    CUresult e = (cmd); \
    if (e != CUDA_SUCCESS) { \
        const char *error_str = NULL; \
        cuGetErrorName(e, &error_str); \
        printf("\nCUDA error: %s\n", error_str); \
        exit(1); \
    } \
} while (0)
#endif


typedef enum {BUFFER, SCALAR} AllocType;

struct ProgramMemory{
    CUdeviceptr *allocations;
    uint32_t *byte_sizes;
    AllocType *alloc_types; //TODO
    uint32_t allocation_count;
};

struct NodeWriteOutputParams{
    void *done_mvar;
    void *read_adress;
    void *write_adress;
    size_t bytesize;
};

struct KernelArguments{
    uint32_t *argument_indeces;
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


struct KernelStagingNodeParams{
    struct ProgramMemory *program_memory;
    struct KernelArguments kernel_args;
    CUdeviceptr kernel_args_obj; // TODO: voidpointer and fix alignment for different bytesizes
};

void CUDA_CB kernel_staging_node(void *arguments){
    struct KernelStagingNodeParams *params = (struct KernelStagingNodeParams *)arguments;
    CUdeviceptr *arg_obj = malloc(2 * sizeof(CUdeviceptr));

    for (uint32_t i = 0; i < params->kernel_args.argument_count; i++){
        uint32_t index = params->kernel_args.argument_indeces[i];
        //Only works for buffers
        arg_obj[i] = params->program_memory->allocations[index];
    }
    cuMemcpyHtoD(params->kernel_args_obj, (void *)arg_obj, sizeof(2 * sizeof(CUdeviceptr)));
}
void CUDA_CB kernel_commit_node(void * arguments){
    struct KernelStagingNodeParams *params = (struct KernelStagingNodeParams *)arguments;
    CUdeviceptr *arg_obj = malloc(2 * sizeof(CUdeviceptr));
    cuMemcpyDtoH(arg_obj, params->kernel_args_obj, sizeof(2 * sizeof(CUdeviceptr)));

    for (uint32_t i = 0; i < params->kernel_args.argument_count; i++){
        uint32_t index = params->kernel_args.argument_indeces[i];
        //Only works for buffers
        params->program_memory->allocations[index] = arg_obj[i];
    }
}

void run_graph
    ( uint32_t node_count
    , uint32_t *node_dependency_counts
    , uint32_t **node_dependencies
    , struct NodeContent *node_contents
    , uint32_t allocation_count
    , uint32_t *input_bytesizes
    , char **input_data
    , char **output_data
    , void **output_mvars
    , void *done_mvar )
{
    printf("Hello from c! The nodecount is %d\n", node_count);
    printf("\nSize of a pointer %lu\n", sizeof(char*));
    printf("\nSome nodecontent info: size %lu, alignment: %lu\n", sizeof(struct NodeContent), _Alignof(struct NodeContent));
    printf("\nSome content content info: size %lu, alignment: %lu\n", sizeof(node_contents[0].content), _Alignof(sizeof(node_contents[0].content)));
    printf("\nSome kernel content info: size %lu, alignment: %lu\n", sizeof(node_contents[0].content.kernel), _Alignof(sizeof(node_contents[0].content.kernel)));


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

    printf("\nAllocating device pointers");
    CUdeviceptr *dev_pointers = (CUdeviceptr *)malloc(allocation_count * sizeof(CUdeviceptr));
    for (size_t i = 0; i < allocation_count; i++)
    {
        CUdeviceptr ptr;
        CU_CHECK(cuMemAlloc(&ptr, input_bytesizes[i]));
        dev_pointers[i] = ptr;
    }
    struct ProgramMemory program_memory;
    program_memory.allocation_count = allocation_count;
    program_memory.allocations = dev_pointers;
    program_memory.byte_sizes = input_bytesizes;

    printf("\nDone allocating device pointers");

    CUmodule mod_alloc;
    printf("\nLoading module");
    CU_CHECK(cuModuleLoad(&mod_alloc, "/home/mdrenth/remote/cuda_id/alloc_kernel.ptx"));
    printf("\n Succesfully loaded alloc module");
    printf("\nGetting Function");
    CUfunction alloc_kernel_func;
    CU_CHECK(cuModuleGetFunction(&alloc_kernel_func, mod_alloc, "id_kern"));
    printf("\nSetting up parameters");

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
        CUgraphNode new_node;

        size_t dependency_count = (size_t)node_dependency_counts[node_index];
        CUgraphNode *dependencies = malloc(dependency_count * sizeof(CUgraphNode));
        printf("\nDependencies: ");
        for (uint32_t j = 0; j < dependency_count; j++){
            uint32_t dependency_index = node_dependencies[node_index][j];
            printf("%d, ", dependency_index);
            dependencies[j] = nodes[dependency_index];
        }

        switch (content.node_type)
        {
        case NODE_COPY:
            CUDA_MEMCPY3D cpy_params_cpy;
            memset(&cpy_params_cpy, 0, sizeof(cpy_params_cpy));
            printf("Copy from %d to %d", content.content.general.alloc_1, content.content.general.alloc_2);
            cpy_params_cpy.srcMemoryType = CU_MEMORYTYPE_DEVICE;
            cpy_params_cpy.srcDevice = dev_pointers[content.content.general.alloc_1];
            cpy_params_cpy.srcPitch = (size_t)input_bytesizes[content.content.general.alloc_1];
            cpy_params_cpy.srcHeight = 1;
            cpy_params_cpy.dstMemoryType = CU_MEMORYTYPE_DEVICE;
            cpy_params_cpy.dstDevice = dev_pointers[content.content.general.alloc_2];
            cpy_params_cpy.dstPitch = (size_t)input_bytesizes[content.content.general.alloc_2];
            cpy_params_cpy.dstHeight = 1;
            cpy_params_cpy.WidthInBytes = (size_t)input_bytesizes[content.content.general.alloc_2];
            cpy_params_cpy.Height = 1;
            cpy_params_cpy.Depth = 1;
            CU_CHECK(cuGraphAddMemcpyNode(&new_node, graph, dependencies, dependency_count, &cpy_params_cpy, cuContext));
            break;
        
        case NODE_INPUT:
            CUDA_MEMCPY3D cpy_params_input;
            memset(&cpy_params_input, 0, sizeof(cpy_params_input));
            uint32_t input_size = input_bytesizes[content.content.general.alloc_1] / 8;
            printf("Input to %d", content.content.general.alloc_1);
            printf("\nInput value: size = %d, value = ", input_size);
            for (uint32_t c = 0; c < input_size; c++){
                printf("%lu", ((uint64_t* )input_data[content.content.general.alloc_1])[c]);
            }
            cpy_params_input.srcMemoryType = CU_MEMORYTYPE_HOST;
            cpy_params_input.srcHost = (void *)input_data[content.content.general.alloc_1];
            cpy_params_input.srcPitch = (size_t)input_bytesizes[content.content.general.alloc_1];
            cpy_params_input.srcHeight = 1;
            cpy_params_input.dstMemoryType = CU_MEMORYTYPE_DEVICE;
            cpy_params_input.dstDevice = dev_pointers[content.content.general.alloc_1];
            cpy_params_input.dstPitch = (size_t)input_bytesizes[content.content.general.alloc_1];
            cpy_params_input.dstHeight = 1;
            cpy_params_input.WidthInBytes = (size_t)input_bytesizes[content.content.general.alloc_1];
            cpy_params_input.Height = 1;
            cpy_params_input.Depth = 1;
            CU_CHECK(cuGraphAddMemcpyNode(&new_node, graph, dependencies, dependency_count, &cpy_params_input, cuContext));
            break;
        case NODE_OUTPUT:
            CUgraphNode copy_node;
            void *output_adress;
            CU_CHECK(cuMemAllocHost(&output_adress, input_bytesizes[content.content.general.alloc_1]));
            CUDA_MEMCPY3D cpy_params_output;
            memset(&cpy_params_output, 0, sizeof(cpy_params_output));
            printf("Output from %d, size is %d", content.content.general.alloc_1, input_bytesizes[content.content.general.alloc_1]);
            cpy_params_output.srcMemoryType = CU_MEMORYTYPE_DEVICE;
            cpy_params_output.srcDevice = dev_pointers[content.content.general.alloc_1];
            cpy_params_output.srcPitch = (size_t)input_bytesizes[content.content.general.alloc_1];
            cpy_params_output.srcHeight = 1;
            cpy_params_output.dstMemoryType = CU_MEMORYTYPE_HOST;
            cpy_params_output.dstHost = output_adress;
            // cpy_params_output.dstHost = (void *)output_data[content.content.general.alloc_1];
            cpy_params_output.dstPitch = (size_t)input_bytesizes[content.content.general.alloc_1];
            cpy_params_output.dstHeight = 1;
            cpy_params_output.WidthInBytes = (size_t)input_bytesizes[content.content.general.alloc_1];
            cpy_params_output.Height = 1;
            cpy_params_output.Depth = 1;
            CU_CHECK(cuGraphAddMemcpyNode(&copy_node, graph, dependencies, dependency_count, &cpy_params_output, cuContext));

            CUDA_HOST_NODE_PARAMS hostParams;
            memset(&hostParams, 0, sizeof(hostParams));

            memset(&outputParams[output_node_index], 0, sizeof(struct NodeWriteOutputParams));
            outputParams[output_node_index].bytesize = (size_t)input_bytesizes[content.content.general.alloc_1];
            outputParams[output_node_index].done_mvar = output_mvars[content.content.general.alloc_1];
            outputParams[output_node_index].write_adress = (void *)output_data[content.content.general.alloc_1];
            outputParams[output_node_index].read_adress = output_adress;

            hostParams.userData = (void *)(&outputParams[output_node_index]);
            hostParams.fn = node_write_output;

            CU_CHECK(cuGraphAddHostNode(&new_node, graph, &copy_node, 1, &hostParams));
            output_node_index += 1;
            break;
        case NODE_EMPTY:
            printf("Empty");
            CU_CHECK(cuGraphAddEmptyNode(&new_node, graph, dependencies, dependency_count));
            break;
        case NODE_ALLOC: // TODO: Remove placeholder
            printf("Alloc");
            CU_CHECK(cuGraphAddEmptyNode(&new_node, graph, dependencies, dependency_count));
            break;
        case NODE_KERNEL: // TODO: Remove placeholder
            printf("Kernel from %d to %d", content.content.kernel.alloc_1, content.content.kernel.alloc_2);
            printf("\nKernel module: %s", content.content.kernel.module_path);
            printf("\nKernel symbol: %s", content.content.kernel.symbol);
            
            CUmodule mod;
            printf("\nLoading module");
            CU_CHECK(cuModuleLoad(&mod, content.content.kernel.module_path));

            printf("\nGetting Function");
            CUfunction kernel_func;
            CU_CHECK(cuModuleGetFunction(&kernel_func, mod, content.content.kernel.symbol));
            printf("\nSetting up parameters");
            size_t poffset;
            size_t psize;
            CU_CHECK(cuFuncGetParamInfo(kernel_func, 0, &poffset, &psize));
            CU_CHECK(cuFuncGetParamInfo(kernel_func, 1, &poffset, &psize));
            printf("\nDone getting param info");
            
            CUDA_KERNEL_NODE_PARAMS kernel_params;
            memset(&kernel_params, 0, sizeof(kernel_params));
            kernel_params.blockDimX = kernel_params.blockDimY = kernel_params.blockDimZ = 1;
            kernel_params.gridDimX = kernel_params.gridDimY = kernel_params.gridDimZ = 1;
            kernel_params.ctx = cuContext;

            struct KernelArguments kArgs;
            kArgs.argument_count = 2;
            uint32_t kernel_arg_indeces[2] = {content.content.kernel.alloc_1, content.content.kernel.alloc_2};
            kArgs.argument_indeces;
            
            CUdeviceptr npointer; 
            // CU_CHECK(cuMemAlloc(&npointer, 8));
            CUdeviceptr input_ptr = dev_pointers[content.content.kernel.alloc_1];
            CUdeviceptr output_ptr = dev_pointers[content.content.kernel.alloc_2];
            // struct ST {
            //     CUdeviceptr outp;
            //     CUdeviceptr inp;
            // };
            // struct ST st;
            // st.inp = input_ptr;
            // st.outp = output_ptr;
            CUdeviceptr params_ptr;
            // CUdeviceptr arr[2] = {output_ptr, input_ptr};
            printf("\nAllocating param struct");
            CU_CHECK(cuMemAlloc(&params_ptr, 2 * sizeof(CUdeviceptr))); //acquire actual bytesize of argument struct
            // CU_CHECK(cuMemAlloc(&params_ptr, sizeof(arr)));
            // CU_CHECK(cuMemAlloc(&params_ptr, sizeof(struct ST)));

            printf("\nCopying param struct to device");
            // CU_CHECK(cuMemcpyHtoD(params_ptr, (void *)arr, sizeof(arr)));
            // CU_CHECK(cuMemcpyHtoD(params_ptr, (void *)&st, sizeof(struct ST)));

            void *args[] = {&npointer, &params_ptr};
            // void *args[] = {&npointer, &input_ptr, &output_ptr};
            kernel_params.kernelParams = args;
            kernel_params.func = kernel_func;
            // kernel_params.func = fs[0];


            CUDA_HOST_NODE_PARAMS commit_node_params;
            memset(&commit_node_params, 0, sizeof(commit_node_params));

            CUDA_HOST_NODE_PARAMS staging_node_params;
            memset(&staging_node_params, 0, sizeof(staging_node_params));


            struct KernelStagingNodeParams commit_params;
            commit_params.program_memory = &program_memory;
            commit_params.kernel_args_obj = params_ptr;
            commit_params.kernel_args = kArgs;

            commit_node_params.userData = (void *)(&commit_params);
            commit_node_params.fn = kernel_commit_node;

            staging_node_params.userData = (void *)(&commit_params);
            staging_node_params.fn = kernel_commit_node;

            CUgraphNode staging_node;
            CU_CHECK(cuGraphAddHostNode(&staging_node, graph, dependencies, dependency_count, &staging_node_params));

            CUgraphNode kernel_node;
            CU_CHECK(cuGraphAddKernelNode(&kernel_node, graph, &staging_node, 1, &kernel_params));

            CU_CHECK(cuGraphAddHostNode(&new_node, graph, &kernel_node, 1, &commit_node_params));
            break;
        default:
            break;
        }

        nodes[node_index] = new_node;
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

    hs_try_putmvar(-1, done_mvar);
}
#include "types.h"
#include "util.h"

CUDA_KERNEL_NODE_PARAMS load_ptx_kernel(struct BuildState state, struct KernelPhase *data) {
    printf("\nKernel module: %s", data->module_path);
    printf("\nKernel symbol: %s", data->symbol);
    
    CUmodule mod;
    printf("\nLoading module");
    CU_CHECK(cuModuleLoad(&mod, data->module_path));

    CUfunction kernel_func;
    printf("\nGetting Function");
    CU_CHECK(cuModuleGetFunction(&kernel_func, mod, data->symbol));

    CUDA_KERNEL_NODE_PARAMS kernel_params;
    memset(&kernel_params, 0, sizeof(kernel_params));

    kernel_params.blockDimX = data->thread_block_size;
    kernel_params.blockDimY = kernel_params.blockDimZ = 1;
    kernel_params.gridDimX = data->grid_size;
    kernel_params.gridDimY = kernel_params.gridDimZ = 1;

    kernel_params.ctx = state.ctx;

    kernel_params.func = kernel_func;

    return kernel_params;
}

void add_kernel_node(struct BuildState state, struct KernelData data) {

    uint32_t k_arg_count = data.arg_count;
    uint32_t *k_arg_indices = data.arg_indices;
    // printf("Kernel from %d to %d", k_a1, k_a2);
    printf("\nIterating over %d parameters: ", k_arg_count);
    for (size_t a_idx = 0; a_idx < k_arg_count; a_idx++){
        printf("%d, ", k_arg_indices[a_idx]);
    }
    
    printf("\n\nLoading prep kernel");
    CUDA_KERNEL_NODE_PARAMS prep_node_params = load_ptx_kernel(state, &data.prep_phase);
    printf("\n\nLoading main phase kernel");
    CUDA_KERNEL_NODE_PARAMS node_params = load_ptx_kernel(state, &data.main_phase);


    printf("\nAllocating input param struct");
    size_t params_ptr_struct_size = k_arg_count * sizeof(CUdeviceptr);
    CUdeviceptr params_idxs_d;
    CU_CHECK(cuMemAlloc(&params_idxs_d, params_ptr_struct_size));
    CUdeviceptr *params_idxs_h = malloc(params_ptr_struct_size);

    size_t params_struct_size = 0;

    for (size_t a_idx = 0; a_idx < k_arg_count; a_idx++)
    {   // TODO: This should account for alignment
        // params_struct_size += state.bytesizes[a_idx];
        params_struct_size += sizeof(CUdeviceptr);
        params_idxs_h[a_idx] = (CUdeviceptr)(state.mem + state.mem_offsets[k_arg_indices[a_idx]]);
        printf("\nKernel param value: %llu", params_idxs_h[a_idx]);
    }
    CU_CHECK(cuMemcpyHtoD(params_idxs_d, params_idxs_h, params_ptr_struct_size));

    CUdeviceptr params_ptr;
    CU_CHECK(cuMemAlloc(&params_ptr, params_struct_size));

    CUdeviceptr npointer; 
    void *args[] = {&npointer, &params_ptr};
    void *prep_args[] = {&params_idxs_d, &params_ptr};

    node_params.kernelParams = args;
    prep_node_params.kernelParams = prep_args;

    CUgraphNode prep_node;

    // CU_CHECK(cuGraphAddKernelNode(state.current_node, state.graph, state.dependencies, state.dependency_count, &prep_node_params));
    CU_CHECK(cuGraphAddKernelNode(&prep_node, state.graph, state.dependencies, state.dependency_count, &prep_node_params));

    CU_CHECK(cuGraphAddKernelNode(state.current_node, state.graph, &prep_node, 1, &node_params));
}
#include "/usr/local/cuda/include/cuda.h"
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

struct NodeContent;
struct KernelPhase;
struct GraphProgram;
struct KernelData;
struct AllocData;
struct BuildState;
typedef enum {NODE_COPY, NODE_INPUT, NODE_OUTPUT, NODE_EMPTY, NODE_ALLOC, NODE_KERNEL} NodeType;
typedef enum {MEM_SCALAR, MEM_BUFFER} MemType;


struct BuildState {
    CUgraph graph;
    CUcontext ctx;

    char *mem;
    uint32_t *mem_offsets;
    uint32_t *size_indices;

    uint32_t *bytesizes;
    uint32_t *sizes;

    CUgraphNode *current_node;
    size_t dependency_count;
    CUgraphNode *dependencies;
};

struct KernelPhase {
    char *module_path; // Size 8, alignment 8
    char *symbol; // Size 8, alignment 8
    int32_t thread_block_size; // Size 4, alignment 4
    int32_t grid_size; // Size 4, alignment 4
    int32_t shared_memory_bytes; // Size 4, alignment 4
}; // Size 32, alignment 8

struct KernelData{
    uint32_t arg_count; // Size 4, alignment 4
    uint32_t *arg_indices; // Size 8, alignment 8
    struct KernelPhase main_phase; // Size 32, alignment 8
    struct KernelPhase prep_phase; // Size 32, alignment 8
}; // Size 80, alignment 8

struct AllocData{
    uint32_t write_index; // Size 4, alignment 4
    uint32_t arg_count; // Size 4, alignment 4
    uint32_t *arg_indices; // Size 8, alignment 8
    struct KernelPhase alloc_kernel; // Size 32, alignment 8
}; // Size 48, alignment 8

struct NodeContent {
    int32_t node_type; // Size 1, alignment 1
    union {
        struct KernelData kernel; 
        struct AllocData alloc_data;
        struct {
            uint32_t alloc_1;
            uint32_t alloc_2;
        } copy;
        struct {
            uint32_t alloc_1;
            uint32_t alloc_2;
        } general; // Size 8, alignment 8
    } content; // Size 40, alignment 8
}; // Size 56, alignment 8


void hs_try_putmvar(int32_t, void*);

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
    , void *done_mvar
    );


CUDA_KERNEL_NODE_PARAMS load_ptx_kernel(struct BuildState state, struct KernelPhase *data);
void add_kernel_node(struct BuildState state, struct KernelData data);
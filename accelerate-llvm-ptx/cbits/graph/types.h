#include "/usr/local/cuda/include/cuda.h"
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

struct NodeContent;
struct KernelPhase;
struct GraphProgram;
struct KernelData;
struct BuildState;
typedef enum {NODE_COPY, NODE_INPUT, NODE_OUTPUT, NODE_EMPTY, NODE_ALLOC, NODE_KERNEL} NodeType;
typedef enum {MEM_SCALAR, MEM_BUFFER} MemType;


struct BuildState {
    CUgraph graph;
    CUcontext ctx;

    char *mem;
    uint32_t *mem_offsets;
    CUdeviceptr *dev_pointers;

    uint32_t *bytesizes;

    CUgraphNode *current_node;
    size_t dependency_count;
    CUgraphNode *dependencies;
};

struct KernelPhase {
    char *module_path; // Size 8, alignment 8
    char *symbol; // Size 8, alignment 8
    int32_t thread_block_size;
    int32_t grid_size;
    int32_t shared_memory_bytes;
};

struct KernelData{
    uint32_t arg_count; // Size 4, alignment 4
    uint32_t *arg_indices; // Size 4, alignment 4
    struct KernelPhase main_phase;
    struct KernelPhase prep_phase;
}; // Size 48, alignment 8

struct NodeContent {
    int32_t node_type; // Size 1, alignment 1
    union {
        struct KernelData kernel; 
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


struct GraphProgram {
    // Graph definition
    uint32_t node_count;
    uint32_t *node_dependency_counts;
    uint32_t **node_dependencies;
    struct NodeContent *node_contents;

    uint32_t allocation_count;
    uint32_t *allocation_sizes;
    
    // Graph input
    // uint8_t *input_data;
    void **input_data;
    
    // TODO
    // Graph output
    void **output_mvars;
    void **output_data;

    void *done_mvar;
};

void hs_try_putmvar(int32_t, void*);

void run_graph
    ( uint32_t node_count
    , uint32_t *node_dependency_counts
    , uint32_t **node_dependencies
    , struct NodeContent *node_contents
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

void add_kernel_node(struct BuildState state, struct KernelData data);
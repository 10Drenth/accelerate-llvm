#include "/usr/local/cuda/include/cuda.h"
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

struct NodeContent;
struct GraphProgram;
typedef enum {NODE_COPY, NODE_INPUT, NODE_OUTPUT, NODE_EMPTY, NODE_ALLOC, NODE_KERNEL} NodeType;
typedef enum {MEM_SCALAR, MEM_BUFFER} MemType;


struct NodeContent {
    int32_t node_type; // Size 1, alignment 1
    union {
        struct {
            uint32_t arg_count; // Size 4, alignment 4
            uint32_t *arg_indices; // Size 4, alignment 4
            char *module_path; // Size 8, alignment 8
            char *symbol; // Size 8, alignment 8
            char *prep_module_path; // Size 8, aligment 8
            char *prep_symbol; // Size 8, alignment 8
        } kernel; // Size 48, alignment 8
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
    , void *done_mvar)
    ;
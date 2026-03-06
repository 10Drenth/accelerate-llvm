#include "/usr/local/cuda/include/cuda.h"
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

struct NodeContent;
struct GraphProgram;
typedef enum {NODE_COPY, NODE_INPUT, NODE_OUTPUT, NODE_EMPTY} NodeType;

struct NodeContent {
    uint32_t node_type;
    uint32_t alloc_1;
    uint32_t alloc_2;
    uint32_t padding;
};

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
    , uint32_t *allocation_sizes
    , char **input_data
    , char **output_data
    , void **output_mvars
    , void *done_mvar)
    ;
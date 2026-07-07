#include "/usr/local/cuda/include/cuda.h"

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
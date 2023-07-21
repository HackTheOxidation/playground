#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef unsigned int u32;
typedef unsigned long long u64;

#define FUNC_PTR(VAR, RET, ...) \
    RET (*VAR)(__VA_ARGS__) 

typedef FUNC_PTR(MathFunc, u64, u32, u32);

u64 add(u32 a, u32 b) {
    return a + b;
}

u64 mul(u32 a, u32 b) {
    return a * b;
}

void exec(MathFunc fun, u32 a, u32 b) {
    printf("%lld\n", fun(a, b));
}

int main() {
    MathFunc fun;

    fun = &add;
    exec(fun, 5, 7);

    fun = &mul;
    exec(fun, 5, 7);
    
    return 0;
}

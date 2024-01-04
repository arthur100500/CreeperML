// Copyright 2023-2024, Arthur Alekseev and Starcev Matvey

// SPDX-License-Identifier: LGPL-3.0-or-later

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define cmptr int64_t // CreeperML pointer

typedef struct str_str
{
    int len;
    int *data;
} strr;

typedef struct function_str
{
    cmptr (*fn)(); // The pointer to function itself
    cmptr *argv;   // Arguments to be applied
    cmptr argc;    // How many arguments there are already
    cmptr arity;   // How many argument does function take at all
} function;

extern void print_int(const int i)
{
    printf("%d\n", i);
}

extern void print_string(const char* s)
{
    printf("%s\n", s);
}

extern cmptr get_arity(cmptr fn_ptr);

cmptr create_function(cmptr fn, cmptr argc, cmptr argv);

extern cmptr cm_malloc(size_t size)
{
    return malloc(size);
}

cmptr get_arity_wrapper(cmptr fn_ptr)
{
    cmptr res = get_arity(fn_ptr);
    return res;
}

extern cmptr apply_closure(function *closure, cmptr N)
{
    cmptr *a = closure->argv;

    switch (N)
    {
    case 0:
        return closure->fn();
    case 1:
        return closure->fn(a[0]);
    case 2:
        return closure->fn(a[0], a[1]);
    case 3:
        return closure->fn(a[0], a[1], a[2]);
    case 4:
        return closure->fn(a[0], a[1], a[2], a[3]);
    case 5:
        return closure->fn(a[0], a[1], a[2], a[3], a[4]);
    case 6:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    case 8:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    case 9:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
    case 10:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
    case 11:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10]);
    case 12:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11]);
    case 13:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12]);
    case 14:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13]);
    case 15:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14]);
    case 16:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15]);
    }
}

cmptr apply_args(function *clsr, cmptr argc, cmptr *argv)
{
    cmptr arity = get_arity((cmptr)clsr->fn);

    argc = clsr->argc + argc;

    // Merge arguments
    cmptr *clsr_argv = clsr->argv;

    clsr->argv = cm_malloc(argc * sizeof(cmptr));

    for (int i = 0; i < clsr->argc; i++)
        clsr->argv[i] = clsr_argv[i];

    for (int i = clsr->argc; i < argc; i++)
        clsr->argv[i] = argv[i - clsr->argc];

    // Apply arguments
    if (argc > arity)
    {
        cmptr res = apply_closure(clsr, arity);

        for (int i = arity; i < argc; i++)
        {
            argv[i - arity] = argv[i];
        }

        cmptr clsr_res = create_function(res, 0, NULL);

        cmptr app_res = apply_args(clsr_res, argc - arity, argv);

        return app_res;
    }

    if (argc == arity)
    {
        cmptr res = apply_closure(clsr, arity);

        if (get_arity(res) != 0)
        {
            cmptr clsr_res = create_function(res, 0, NULL);
            return res;
        }

        return res;
    }

    return clsr;
}

cmptr create_function(cmptr fn, cmptr argc, cmptr argv)
{
    cmptr arity = get_arity(fn);

    function *clsr = (function *)cm_malloc(sizeof(function));
    clsr->arity = arity; // Summary size of all arguments
    clsr->fn = fn;
    clsr->argv = argv;
    clsr->argc = argc;

    return clsr;
}

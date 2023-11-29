// Copyright 2023-2024, Arthur Alekseev and Starcev Matvey 

// SPDX-License-Identifier: LGPL-3.0-or-later

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct str_str
{
    int len;
    int* data;
} strr;

extern void print_int(const int i)
{
    printf("%d", i);
}

extern void print_string(const strr* str)
{
    int len = str->len;

    for (int i = 0; i < len; i++)
    {
        printf("%c", str->data[i]);
    }
}
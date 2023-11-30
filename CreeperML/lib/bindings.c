// Copyright 2023-2024, Arthur Alekseev and Starcev Matvey 

// SPDX-License-Identifier: LGPL-3.0-or-later

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void print_int(const int i)
{
    printf("%d\n", i);
}

extern void print_string(const char* s)
{
    printf("%s\n", s);
}
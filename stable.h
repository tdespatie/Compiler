/*
 *   File name: stable.h
 *   Compiler: GNU GCC Compiler
 *   Author: [Tyler Despatie, 040-694-672]
 *   Course: CST 8152 – Compilers, Lab Section: 401
 *   Assignment: 3
 *   Date: 15 November 2013
 *   Professor: Sv. Ranev
 *   Purpose: Prototypes for stable.c
 *   Function list: st_create(), st_install(), st_lookup(), st_update_type(),
 *                st_update_value(), st_get_type(),st_destroy(), st_print(),
 *                st_store(), st_sort()
 */
#ifndef STABLE_H
#define STABLE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "buffer.h"

#define _CRT_SECURE_NO_WARNINGS
#define INTEGER  0x0002  /* 0000 0000 0000 0010 */
#define FLOAT    0x0004  /* 0000 0000 0000 0100 */
#define STRING   0x0006  /* 0000 0000 0000 0110 */
#define RESET    0xFFF8  /* 1111 1111 1111 1000 */
#define URESET   0xFFF9  /* 1111 1111 1111 1001 */
#define INITIAL  0x0000  /* 0000 0000 0000 0000 */
#define LSB      0x0001  /* 0000 0000 0000 0001 */

#define SIZE_ZERO sym_table.st_size == 0
#define DEFAULT_CAPACITY   100
#define DEFAULT_INC_FACTOR 15

typedef union InitialValue {
    int int_val; /* integer variable initial value */
    float fpl_val; /* floating-point variable initial value */
    int str_offset; /* string variable initial value (offset) */
} InitialValue;
typedef struct SymbolTableVidRecord {
    unsigned short status_field; /* variable record status field*/
    char *plex; /* pointer to lexeme (VID name) in CA */
    int o_line; /* line of first occurrence */
    InitialValue i_value; /* variable initial value */
    size_t ds_offset;/*offset from the beginning of data segment*/
}STVR;
typedef struct SymbolTableDescriptor {
    STVR *pstvr; /* pointer to array of STVR */
    int st_size; /* size in number of STVR elements */
    int st_offset; /*offset in number of STVR elements */
    Buffer *plsBD; /* pointer to the lexeme storage buffer descriptor */
} STD;

STD st_create(int);
int st_install(STD,char*,int);
int st_lookup(STD,char*);
int st_update_type(STD,int,char);
int st_update_value(STD, int,InitialValue);
char st_get_type(STD,int);
void st_destroy(STD);
int st_print(STD);
int st_store(STD);
int st_sort(STD,char);

#endif

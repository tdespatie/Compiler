/*
 *   File name: parser.h
 *   Compiler: GNU GCC Compiler
 *   Author: [Tyler Despatie, 040-694-672]
 *   Course: CST 8152 Compilers, Lab Section: 401
 *   Assignment: 4
 *   Date: 6 December 2013
 *   Professor: Sv. Ranev
 *   Purpose: Prototypes for parser.c
 */
#ifndef PARSER_H
#define PARSER_H

#include "buffer.h"
#include "token.h"
#include "stable.h"

/* Global Variables */
static Token lookahead_token;
static Buffer *sc_buf;
int synerrno;

/* Constants */
#define _CRT_SECURE_NO_WARNINGS
#define NO_ATTR -1
#define ELSE     0
#define IF       1
#define INPUT    2
#define OUTPUT   3
#define PLATYPUS 4
#define REPEAT   5
#define THEN     6
#define USING    7
#define TOKENS_MATCH lookahead_token.code == pr_token_code
#define CHECK_ATTRS if (lookahead_token.attribute.get_int != pr_token_attribute) break;
#define CHECK_SEOF if (lookahead_token.code == SEOF_T) return;
#define ERROR_TOKEN lookahead_token.code == ERR_T

/* Externals */
extern Token mlwpar_next_token(Buffer *);
extern Buffer *str_LTBL;
extern STD sym_table;
extern char *kw_table[];
extern int line;

/* Core Functions */
void parser(Buffer*);
void match(int,int);
void syn_eh(int);
void syn_printe(void);
void gen_incode(char*);

/* Grammar Functions */
void program(void);
void opt_statements(void);
void statements(void);
void statements_1(void);
void statement(void);
void assign_statement(void);
void assign_expression(void);
void select_statement(void);
void iteration_statement(void);
void input_statement(void);
void var_identifier(void);
void var_list(void);
void var_list_1(void);
void opt_var_list(void);
void output_statement(void);
void output_list(void);
void arith_expression(void);
void unary_arith_expression(void);
void add_arith_expression(void);
void add_arith_expression_1(void);
void multi_arith_expression(void);
void multi_arith_expression_1(void);
void primary_arith_expression(void);
void string_expression(void);
void string_expression_1(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_or_expression(void);
void logical_or_expression_1(void);
void logical_and_expression(void);
void logical_and_expression_1(void);
void relational_expression(void);
void relational_expression_1(void);
void relational_expression_2(void);
void primary_a_relational(void);
void primary_s_relational(void);

#endif

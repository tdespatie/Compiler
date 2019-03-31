/*
 *   File name: parser.c
 *   Compiler: GNU GCC Compiler
 *   Author: [Tyler Despatie, 040-694-672]
 *   Course: CST 8152 – Compilers, Lab Section: 401
 *   Assignment: 4
 *   Date: 6 December 2013
 *   Professor: Sv. Ranev
 *   Purpose: Recursive Descent Predictive Parser
 *   Function list: mlwpar_next_token, program(), gen_incode() parser(), match(), syn_eh(), syn_printe(),
 *                  gen_incode(), opt_statements(), statements(), statements_1(), statement(),
 *                  assign_statement(), assign_expression(), select_statement(), iteration_statement(),
 *                  input_statement(), var_identifier(), var_list(), var_list_1(), opt_var_list(),
 *                  output_statement(), output_list, arith_expression(), unary_arith_expression(),
 *                  add_arith_expression(), add_arith_expression_1(), multi_arith_expression(),
 *                  multi_arith_expression_1(), primary_arith_expression(), string_expression(),
 *                  primary_string_expression(), conditional_expression(), logical_or_expression(),
 *                  logical_or_expression_1(), logical_and_expression(), logical_and_expression_1(),
 *                  relational_expression(), relational_expression_1(), relational_expression_2(),
 *                  primary_a_relational(), primary_s_relational()
 */
#include <stdio.h>
#include <stdlib.h>
#include "parser.h"

#define DEBUG
#undef  DEBUG

/*
 * Purpose: Sets up the parser
 * Author: Svillen Ranev
 * History/Versions: 1.0
 * Called functions: mlwpar_next_token(): match a lexeme with a token
 *                   program(): run the parser
 *                     match(): match the lookahead token with the required token
 *                gen_incode(): used as a printf statement with a \n
 * Parameters: Buffer*: buffer to parse
 */
void parser(Buffer *in_buf) {
    sc_buf = in_buf;
    lookahead_token = mlwpar_next_token(sc_buf);
    program(); match(SEOF_T,NO_ATTR);
    gen_incode("PLATY: Source file parsed");
}

/*
 * Purpose: Match the lookahead token with the required token
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: syn_eh(): Panic mode, get tokens till we hit a syncronizing token
 *        mlwpar_next_token(): match a lexeme with a recognized token
 *               syn_printe(): display the token code and attribute as a syntax error
 * Parameters: int pr_token_code: required token code to match
 *        int pr_token_attribute: required attribute code to match
 */
void match(int pr_token_code,int pr_token_attribute) {
    if (TOKENS_MATCH)       /* Compares token code to expected code */
        switch (pr_token_code) {
            case KW_T:
            case LOG_OP_T:
            case ART_OP_T:
            case REL_OP_T:
                CHECK_ATTRS; /* Compares token attribute to expected attribute, break if no match */
            default:
                CHECK_SEOF; /* Checks if token code is SEOF, returns if it is */
                lookahead_token = mlwpar_next_token(sc_buf);
                if (ERROR_TOKEN) { /* Checks if token code is ERR_T */
                    syn_printe();
                    lookahead_token = mlwpar_next_token(sc_buf);
                    ++synerrno; /* Increment syntax error number */
                }
            return;
        }
    syn_eh(pr_token_code); /* Call error handler if no match */
}

/*
 * Purpose: (Panic mode) Error handler for the parser
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: syn_printe(): allocate memory
 *            mlwpar_next_token(): match a lexeme with a recognized token
 *                         exit(): call the garbage collector
 * Parameters: int sync_token_code: synchronization token code to discard other tokens up to
 * Algorithm: We have found an error, advance to a safe place (sync_token_code) so that we can
 *            continue to parse the file.
 */
void syn_eh(int sync_token_code) {
    syn_printe();
    ++synerrno;

    while(lookahead_token.code != sync_token_code) /* Advance until Synchronization token */
        if (lookahead_token.code != SEOF_T)
            lookahead_token = mlwpar_next_token(sc_buf);
        else
            break;

    if (lookahead_token.code != SEOF_T) { /* Advance one more before leaving */
        lookahead_token = mlwpar_next_token(sc_buf);
        return;
    }

    if (sync_token_code != SEOF_T)
        exit(synerrno);
}

/*
 * Purpose: Display the token code and attribute as a syntax error
 * Author: Svillen Ranev
 * History/Versions: 1.0
 * Called functions:   printf(): print to the standard output
 *             b_get_chmemloc(): get the characters memory location
 */
void syn_printe(void) {
    Token t = lookahead_token;
    printf("PLATY: Syntax error:  Line:%3d\n",line);
    printf("*****  Token code:%3d Attribute: ", t.code);

    switch(t.code) {
	    case ERR_T: /* ERR_T     0   Error token */
	    	 printf("%s\n",t.attribute.err_lex);
	    break;
    	case SEOF_T: /*SEOF_T    1   Source end-of-file token */
	    	 printf("NA\n" );
	    break;
	    case AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	    case SVID_T: /* SVID_T    3  String Variable identifier token */
		     printf("%s\n",sym_table.pstvr[t.attribute.get_int].plex);
	    break;
	    case FPL_T: /* FPL_T     4  Floating point literal token */
		     printf("%5.1f\n",t.attribute.flt_value);
	    break;
	    case INL_T: /* INL_T      5   Integer literal token */
	         printf("%d\n",t.attribute.get_int);
	    break;
	    case STR_T: /* STR_T     6   String literal token */
	         printf("%s\n",b_get_chmemloc(str_LTBL,(short)t.attribute.get_int));
	    break;
        case SCC_OP_T: /* 7   String concatenation operator token */
	         printf("NA\n" );
	    break;
	    case ASS_OP_T: /* ASS_OP_T  8   Assignment operator token */
		     printf("NA\n" );
	    break;
	    case ART_OP_T: /* ART_OP_T  9   Arithmetic operator token */
		     printf("%d\n",t.attribute.get_int);
        break;
	    case REL_OP_T: /*REL_OP_T  10   Relational operator token */
		     printf("%d\n",t.attribute.get_int);
        break;
	    case LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		     printf("%d\n",t.attribute.get_int);
	    break;
	    case LPR_T: /*LPR_T    12  Left parenthesis token */
		     printf("NA\n" );
	    break;
	    case RPR_T: /*RPR_T    13  Right parenthesis token */
	         printf("NA\n" );
	    break;
	    case LBR_T: /*    14   Left brace token */
	         printf("NA\n" );
	    break;
	    case RBR_T: /*    15  Right brace token */
	         printf("NA\n" );
	    break;
	    case KW_T: /*     16   Keyword token */
	         printf("%s\n",kw_table [t.attribute.get_int]);
	    break;
    	case COM_T: /* 17   Comma token */
	         printf("NA\n");
	    break;
	    case EOS_T: /*    18  End of statement *(semi - colon) */
	         printf("NA\n" );
	    break;
	    default:
	         printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
    }
}
/*
 * Purpose: Match "PLATYPUS" as the program starting point
 * Author: Svillen Ranev
 * History/Versions: 1.0
 * Called functions: match(): match the lookahead token with the expected token
 *              gen_incode(): output the parsed line to standard output
 */
void program(void){
    match(KW_T,PLATYPUS);match(LBR_T,NO_ATTR);opt_statements();
    match(RBR_T,NO_ATTR);
    gen_incode("PLATY: Program parsed");
}

/*
 * Purpose: Print to the standard output the given parameters with a line terminator
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: printf(): print to the standard output
 */
void gen_incode(char *arg) {
    printf("%s\n", arg); /* Print out the line with a line terminator */
}

/*
 * Purpose: Handle optional statements
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: statements(): go to the <statements> production
 *                   gen_incode(): output the parsed line to standard output
 *
 * <opt_statements> -> <statements> | E
 * FIRST(opt_statements) = { AVID_T | SVID_T | IF | USING | INPUT | OUTPUT | E }
 */
void opt_statements(void) {
    switch (lookahead_token.code) {
        case AVID_T:
        case SVID_T: /* Check for AVID or SVID */
            statements();
            return; /* We're done here */
        case KW_T: /* Check for a keyword */
            switch(lookahead_token.attribute.get_int) {
                case IF:
                case USING:
                case INPUT:
                case OUTPUT: /* Process only IF, USING, INPUT, OUTPUT */
                    statements();
                    return;
            }
    }
    /* We do have the possibility of empty */
    gen_incode("PLATY: Opt_statements parsed");
}

/*
 * Purpose: <statements> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: statement(): go to the <statement> production
 *                statements_1(): go to the <statements^1> production
 *
 * <statements> -> <statement> | <statements> <statement>
 * Transformed: <statements> -> <statement> <statements^1>
 * FIRST(statements) = { AVID_T | SVID_T | IF | USING | INPUT | OUTPUT }
 */
void statements(void) {
    statement();
    statements_1();
}

/*
 * Purpose: <statements_1> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: statement(): go to the <statement> production
 *                statements_1(): recursive call of <statements^1> production
 *
 * <statements^1> -> <statements> | E
 * FIRST(statements) = { AVID_T | SVID_T | IF | USING | INPUT | OUTPUT }
 * FIRST(statements^1) = { FIRST(statements) | E }
 */
void statements_1(void) {
    switch (lookahead_token.code) {
        case AVID_T:
        case SVID_T: /* Check for AVID or SVID */
            statement();
            statements_1();
            break;
        case KW_T: /* Check for Keyword token */
            switch (lookahead_token.attribute.get_int) {
                case IF:
                case USING:
                case INPUT:
                case OUTPUT: /* Check for IF, USING, INPUT, OUTPUT */
                    statement();
                    statements_1();
                    break;
            }
    }
    /* We can have empty here */
}

/*
 * Purpose: <statement> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: assign_statement(): go to the <assignment statement> production
 *                   select_statement(): go to the <selection statement> production
 *                iteration_statement(): go to the <iteration statement> production
 *                    input_statement(): go to the <input statement> production
 *                   output_statement(): go to the <output statement> production
 *                         syn_printe(): display the token code and attribute as a syntax error
 *
 * <statement> -> <assignment statement> | <selection statement> | <iteration statement>
 *                   | <input statement> | <output statement>
 * FIRST(statement) = { AVID_T | SVID_T | IF | USING | INPUT | OUTPUT }
 */
void statement(void) {
    switch (lookahead_token.code) {
        case AVID_T:
        case SVID_T: /* Check for AVID or SVID */
            assign_statement();
            return;
        case KW_T: /* Check for Keyword token */
            switch (lookahead_token.attribute.get_int) {
                case IF:
                    select_statement(); /* Process an IF statement */
                    return;
                case USING:
                    iteration_statement(); /* Process an interation statement */
                    return;
                case INPUT:
                    input_statement(); /* Process an input statement */
                    return;
                case OUTPUT:
                    output_statement(); /* Process an output statement */
                    return;
            }
    }
    /* We can't have empty */
    syn_printe();
}

/*
 * Purpose: <assignment statement> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: assign_expression(): go to the <assignment expression> production
 *                               match(): match the lookahead token with the expected token
 *                          gen_incode(): output the parsed line to standard output
 *
 * <assignment statement> -> <assignment expression>;
 * FIRST(assignment statement) = { AVID_T | SVID_T }
 */
void assign_statement(void) {
    assign_expression();
    match(EOS_T,NO_ATTR);
    gen_incode("PLATY: Assignment statement parsed");
}

/*
 * Purpose: <assignment expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: arith_expression(): go to the <arithmetic expression> production
 *                  string_expression(): go to the <string expression> production
 *                              match(): match the lookahead token with the expected token
 *                         gen_incode(): output the parsed line to standard output
 *                         syn_printe(): display the token code and attribute as a syntax error
 *
 * <assignment expression> -> <arithmetic expression> | <string expression>
 * FIRST(assignment expression) = { AVID_T | SVID_T }
 */
void assign_expression(void) {
    /* Check for AVID */
    if (lookahead_token.code == AVID_T) {
        match(AVID_T, NO_ATTR);
        match(ASS_OP_T, NO_ATTR); /* Check for an = sign */
        arith_expression(); /* process the assignment */
        gen_incode("PLATY: Assignment expression (arithmetic) parsed");
    } else if (lookahead_token.code == SVID_T) { /* Check for SVID */
        match(SVID_T, NO_ATTR);
        match(ASS_OP_T, NO_ATTR); /* Check for an = sign */
        string_expression(); /* process the assignment */
        gen_incode("PLATY: Assignment expression (string) parsed");
    } else
        syn_printe(); /* We don't have anything else in the first set */
}

/*
 * Purpose: <assignment expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: opt_statements(): call the <opt_statements> production
 *           conditional_expression(): call the <conditional expression> production
 *                            match(): match the lookahead token with the expected token
 *                       gen_incode(): output the parsed line to standard output
 *
 * <selection statement> -> IF (<conditional expression>) THEN <opt_statements>
 *                          ELSE { <opt_statements> } ;
 * FIRST(selection statement) = { IF }
 */
void select_statement(void) {
    match(KW_T, IF);
    match(LPR_T, NO_ATTR);
    conditional_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, THEN);
    opt_statements();
    match(KW_T, ELSE);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: IF statement parsed");
}

/*
 * Purpose: <iteration statement> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: assign_expression(): call the <assignment expression> production
 *                      opt_statements(): call the <opt_statements> production
 *              conditional_expression(): call the <conditional expression> production
 *                               match(): match the lookahead token with the expected token
 *                          gen_incode(): output the parsed line to standard output
 *
 * <iteration statement> -> USING (<assignment expression>, <conditional expression>,
 *                                 <assignment expression> )
 *                          REPEAT { < opt_statements> };
 * FIRST(iteration statement) = { USING }
 */
void iteration_statement(void) {
    match(KW_T, USING);
    match(LPR_T, NO_ATTR);
    assign_expression();
    match(COM_T, NO_ATTR);
    conditional_expression();
    match(COM_T, NO_ATTR);
    assign_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, REPEAT);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: USING statement parsed");
}
/*
 * Purpose: <input statement> production
 * Author: Svillen Ranev
 * History/Versions: 1.0
 * Called functions: match(): match the lookahead token with the expected token
 *              gen_incode(): output the parsed line to standard output
 *                var_list(): call the <variable list> production
 *
 * <input statement> -> INPUT (<variable list>);
 * FIRST(input statement) = { INPUT }
 */
void input_statement(void) {
    match(KW_T, INPUT);match(LPR_T, NO_ATTR);var_list();
    match(RPR_T, NO_ATTR);match(EOS_T, NO_ATTR);
    gen_incode("PLATY: INPUT statement parsed");

}

/*
 * Purpose: <variable identifier> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: match(): match the lookahead token with the expected token
 *              syn_printe(): display the token code and attribute as a syntax error
 *
 * <variable identifier> -> <arithmetic variable identifier> | <string variable identifier>
 * FIRST(variable identifier) = { AVID_T | SVID_T }
 */
void var_identifier(void) { /* A Variable identifier is either an AVID or SVID */
    if (lookahead_token.code == AVID_T || lookahead_token.code == SVID_T)
        match(lookahead_token.code, lookahead_token.attribute.arr_op);
   else
        syn_printe(); /* No possibility of anything else */
}

/*
 * Purpose: <variable list> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: var_identifier(): go to the <variable identifier> production
 *                       var_list_1(): go to the <variable list^1> production
 *                       gen_incode(): output the parsed line to standard output
 *
 * <variable list> -> <variable identifier> | <variable list>,<variable identifier>
 * Transformed: <variable list> -> <variable identifier> <variable list^1>
 * FIRST(variable list) = { AVID_T, SVID_T }
 */
void var_list(void) {
    var_identifier();
    var_list_1();
    gen_incode("PLATY: Variable list parsed");
}

/*
 * Purpose: <variable list^1> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: var_identifier(): go to the <variable identifier> production
 *                       var_list_1(): go to the <variable list^1> production
 *                       match(): match the lookahead token with the expected token
 *
 * <variable list^1> -> ,<variable identifier> <variable list^1> | E
 * FIRST(variable list^1) = { , , E }
 */
void var_list_1(void) { /* Check for comma */
    if (lookahead_token.code == COM_T) {
        match(COM_T, NO_ATTR);
        var_identifier();
        var_list_1();
    }
  /* We can have empty too */
}

/*
 * Purpose: <opt_variable list> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: var_list(): go to the <variable list> production
 *                 gen_incode(): output the parsed line to standard output
 *
 * <opt_variable list> -> <variable list> | E
 * FIRST(opt_variable list) = { AVID_T, SVID_T, E }
 */
void opt_var_list(void) { /* Check for AVID or SVID */
    if (lookahead_token.code == AVID_T || lookahead_token.code == SVID_T)
        var_list();
    else /* We have the possibility of empty */
        gen_incode("PLATY: Optional Variable list parsed");

}

/*
 * Purpose: <output statement> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: match(): match the lookahead token with the expected token
 *             output_list(): go to the <output list> production
 *              gen_incode(): output the parsed line to standard output
 *
 * <output statement> -> OUTPUT (<opt_variable list>); | OUTPUT (STR_T);
 * Transformed: <output statement> -> OUTPUT(<ouptut list>);
 * FIRST(output statement) = { OUTPUT }
 */
void output_statement(void) {
    match(KW_T, OUTPUT);
    match(LPR_T, NO_ATTR);
    output_list();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: OUTPUT statement parsed");
}

/*
 * Purpose: <output list> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: match(): match the lookahead token with the expected token
 *            opt_var_list(): go to the <output variable list> production
 *              gen_incode(): output the parsed line to standard output
 *
 * <output list> -> <opt_variable list> | STR_T
 * FIRST(output list) = { AVID_T, SVID_T, STR_T, E }
 */
void output_list(void) {
    switch (lookahead_token.code) {
        case AVID_T:
        case SVID_T: /* AVID or SVID */
            opt_var_list();
            break;
        case STR_T:
            match(STR_T, NO_ATTR); /* Process STRING token */
            gen_incode("PLATY: Output list (string literal) parsed");
            break;
        default: /* We can have empty */
            gen_incode("PLATY: Output list (empty) parsed");
    }
}

/*
 * Purpose: <arithmetic expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: syn_printe(): display the token code and attribute as a syntax error
 *       unary_arith_expression(): go to the <unary arithmetic expression> production
 *         add_arith_expression(): go to the <additive arithmetic expression> production
 *                   gen_incode(): output the parsed line to standard output
 *
 * <arithmetic expression> -> <unary arithmetic expression> | <additive arithmetic expression>
 * FIRST(arithmetic expression) = { -, +, AVID_T, FPL_T, INL_T, ( }
 */
void arith_expression(void) {
    switch (lookahead_token.code) {
        case ART_OP_T:
            if (lookahead_token.attribute.arr_op != MINUS && /* Check for MINUS or PLUS */
                lookahead_token.attribute.arr_op != PLUS) {
                syn_printe();
                return;
            }
            unary_arith_expression(); /* If minus or plus go here */
            break;
        case AVID_T:
        case FPL_T:
        case INL_T:
        case LPR_T:
            add_arith_expression(); /* If it's not an operator, process here */
            break;
        default:
            syn_printe();   /* If it's not part of the first set, it's an error */
            return;
    }
    gen_incode("PLATY: Arithmetic expression parsed");
}

/*
 * Purpose: <unary arithmetic expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: syn_printe(): display the token code and attribute as a syntax error
 *     primary_arith_expression(): go to the <primary arithmetic expression> production
 *                   gen_incode(): output the parsed line to standard output
 *                        match(): match the lookahead token with the expected token
 *
 * <unary arithmetic expression> -> - <primary arithmetic expression>
 *                                 | + <primary arithmetic expression>
 * FIRST(unary arithmetic expression) = { -, + }
 */
void unary_arith_expression(void) {
    if (lookahead_token.code == ART_OP_T)
        if (lookahead_token.attribute.arr_op == MINUS || /* Check for MINUS OR PLUS */
            lookahead_token.attribute.arr_op == PLUS) {
            match(lookahead_token.code, lookahead_token.attribute.arr_op); /* Process either */
            primary_arith_expression();
            gen_incode("PLATY: Unary arithmetic expression parsed");
            return;
        }
    syn_printe(); /* If it's not in the first set, it's an error */
}

/*
 * Purpose: <additive arithmetic expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: multi_arith_expression(): go to the <multiplicative arithmetic expression> production
 *                   add_arith_expression_1(): go to the <additive arithmetic expression> production
 *
 * Transformed: <additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expression^1>
 * FIRST(additive arithmetic expression) = { AVID_T, FPL_T, INL_T, ( }
 */
void add_arith_expression(void) {
    multi_arith_expression();
    add_arith_expression_1();
}

/*
 * Purpose: <additive arithmetic expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: multi_arith_expression(): go to the <multiplicative arithmetic expression> production
 *                   add_arith_expression_1(): go to the <additive arithmetic expression^1> production
 *                              gen_incode(): output the parsed line to standard output
 *                                    match(): match the lookahead token with the expected token
 *
 * <additive arithmetic expression^1> ->
 *	 +  <multiplicative arithmetic expression><additive arithmetic expression^1>
 * | -  <multiplicative arithmetic expression><additive arithmetic expression^1> | E
 * FIRST(additive arithmetic expression^1) = { +, - , E }
 */
void add_arith_expression_1(void) {
   if (lookahead_token.code == ART_OP_T) /* Check if it's an arithmetic operator */
        if (lookahead_token.attribute.arr_op == PLUS || /* Check if it's plus or minus */
            lookahead_token.attribute.arr_op == MINUS) {
            match(lookahead_token.code, lookahead_token.attribute.arr_op); /* Process either */
            multi_arith_expression();
            add_arith_expression_1();
            gen_incode("PLATY: Additive arithmetic expression parsed");
        }
    /* It can be empty too */
}

/*
 * Purpose: <multiplicative arithmetic expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: multi_arith_expression_1(): go to the <multiplicative arithmetic expression^1> production
 *                   primary_arith_expression(): go to the <primary arithmetic expression> production
 *
 * Transformed: <multiplicative arithmetic expression> ->
 *              <primary arithmetic expression> <multiplicative arithmetic expression^1>
 * FIRST(multiplicative arithmetic expression) = { AVID_T, FPL_T, INL_T, ( }
 */
void multi_arith_expression(void) {
    primary_arith_expression();
    multi_arith_expression_1();
}

/*
 * Purpose: <multiplicative arithmetic expression^1> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: multi_arith_expression_1(): go to the <multiplicative arithmetic expression^1> production
 *                   primary_arith_expression(): go to the <primary arithmetic expression> production
 *                                 gen_incode(): output the parsed line to standard output
 *                                      match(): match the lookahead token with the expected token
 *
 * <multiplicative arithmetic expression^1> ->
 *   * <primary arithmetic expression><multiplicative arithmetic expression^1>
 * | / <primary arithmetic expression><multiplicative arithmetic expression^1> | E
 * FIRST(multiplicative arithmetic expression^1) = { *, / , E }
 */
void multi_arith_expression_1(void) {
    if (lookahead_token.code == ART_OP_T)
        if (lookahead_token.attribute.arr_op == MULT || /* Check if it's Multiplication or Division */
            lookahead_token.attribute.arr_op == DIV) {
            match(lookahead_token.code, lookahead_token.attribute.arr_op); /* Process either */
            primary_arith_expression();
            multi_arith_expression_1();
            gen_incode("PLATY: Multiplicative arithmetic expression parsed");
        }
    /* It could be empty too */
}

/*
 * Purpose: <primary arithmetic expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: syn_printe(): display the token code and attribute as a syntax error
 *             arith_expression(): go to the <arithmetic expression> production
 *                   gen_incode(): output the parsed line to standard output
 *                        match(): match the lookahead token with the expected token
 *
 * <primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
 * FIRST(primary arithmetic expression) = { AVID_T, FPL_T, INL_T, ( }
 */
void primary_arith_expression(void) {
    switch (lookahead_token.code) {
        case AVID_T:
        case FPL_T:
        case INL_T: /* Process either, AVID, FPL, or INL */
            match(lookahead_token.code, lookahead_token.attribute.arr_op);
            break;
        case LPR_T: /* Left parenthesis found, let's take a look inside */
            match(LPR_T, NO_ATTR);
            arith_expression();
            match(RPR_T, NO_ATTR);
            break;
        default:
            syn_printe(); /* We do not have the possibility of empty or anything else */
            return;
    }
    gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*
 * Purpose: <string expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: primary_string_expression(): go to the <primary string expression> production
 *                         string_expression_1(): go to the <string expression^1> production
 *
 * Transformed: <string expression> -> <primary string expression> <string expression^1>
 * FIRST(string expression) = { SVID_T, STR_T }
 */
void string_expression(void) {
    primary_string_expression();
    string_expression_1();
}

/*
 * Purpose: <string expression^1> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: primary_string_expression(): go to the <primary string expression> production
 *                         string_expression_1(): go to the <string expression^1> production
 *                   gen_incode(): output the parsed line to standard output
 *                        match(): match the lookahead token with the expected token
 *
 * <string expression^1> -> <> <primary string expression> <string expression^1> | E
 * FIRST(string expression^1) = { <>, E }
 */
void string_expression_1(void) {
    if (lookahead_token.code == SCC_OP_T) { /* Check for concatenation operator */
        match(SCC_OP_T, NO_ATTR); /* Process concatenation */
        primary_string_expression();
        string_expression_1();
    } else /* It can be empty too */
        gen_incode("PLATY: String expression parsed");
}

/*
 * Purpose: <primary string expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: syn_printe(): display the token code and attribute as a syntax error
 *                   gen_incode(): output the parsed line to standard output
 *                        match(): match the lookahead token with the expected token
 *
 * <primary string expression> -> SVID_T | STR_T
 * FIRST(primary string expression) = { SVID_T, STR_T }
 */
void primary_string_expression(void) {
    if (lookahead_token.code == SVID_T || lookahead_token.code == STR_T) /* Check for SVID or STRING */
        match(lookahead_token.code, lookahead_token.attribute.arr_op); /* Process either */
    else
        syn_printe(); /* It cannot be empty */
    gen_incode("PLATY: Primary string expression parsed");
}

/*
 * Purpose: <conditional expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: logical_or_expression(): go to the <logical or expression> production
 *                              gen_incode(): output the parsed line to standard output
 *
 * <conditional expression> -> <logical OR expression>
 * FIRST(conditional expression) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 */
void conditional_expression(void) {
    logical_or_expression();
    gen_incode("PLATY: Conditional expression parsed");
}

/*
 * Purpose: <logical or expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: logical_and_expression(): go to the <logical and expression> production
 *                  logical_or_expression_1(): go to the <logical or expression^1> production
 *
 * Transformed: <logical OR expression> -> <logical AND expression> <logical OR expression^1>
 * FIRST(logical OR expression) = {  AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 */
void logical_or_expression(void) {
    logical_and_expression();
    logical_or_expression_1();
}

/*
 * Purpose: <logical or expression^1> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: logical_and_expression(): go to the <logical and expression> production
 *                  logical_or_expression_1(): go to the <logical or expression^1> production
 *                               gen_incode(): output the parsed line to standard output
 *                                    match(): match the lookahead token with the expected token
 *
 * <logical OR expression^1> -> .OR. <logical AND expression> <logical OR expression^1> | E
 * FIRST(logical OR expression^1) = { .OR., E }
 */
void logical_or_expression_1(void) { /* Check for OR */
    if (lookahead_token.code == LOG_OP_T && lookahead_token.attribute.log_op == OR ) {
        match(LOG_OP_T, OR); /* Process OR */
        logical_and_expression();
        logical_or_expression_1();
        gen_incode("PLATY: Logical OR expression parsed");
    }
    /* It may be empty too */
}

/*
 * Purpose: <logical and expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: relatonal_expression(): go to the <relational expression> production
 *               logical_and_expression_1(): go to the <logical and expression^1> production
 *
 * Transformed: <logical AND expression> -> <relational expression> <logical AND expression^1>
 * FIRST(logical AND expression) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 */
void logical_and_expression(void) {
    relational_expression();
    logical_and_expression_1();
}

/*
 * Purpose: <logical and expression^1> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: relatonal_expression(): go to the <relational expression> production
 *               logical_and_expression_1(): go to the <logical and expression^1> production
 *                             gen_incode(): output the parsed line to standard output
 *                                  match(): match the lookahead token with the expected token
 *
 * <logical AND expression^1> -> .AND. <relational expression> <logical AND expression^1> | E
 * FIRST(logical AND expression^1) = { .AND., E }
 */
void logical_and_expression_1(void) { /* Check for AND */
    if (lookahead_token.code == LOG_OP_T && lookahead_token.attribute.log_op == AND) {
        match(LOG_OP_T, AND); /* Process AND */
        relational_expression();
        logical_and_expression_1();
        gen_incode("PLATY: Logical AND expression parsed");
    }
    /* It may be empty too */
}

/*
 * Purpose: <relational expression> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: relatonal_expression_1(): go to the <relational expression^1> production
 *                   relatonal_expression_2(): go to the <relational expression^2> production
 *                     primary_a_relational(): go to the <primary a relational> production
 *                     primary_s_relational(): go to the <primary s relational> production
 *                               syn_printe(): display the token code and attribute as a syntax error
 *                               gen_incode(): output the parsed line to standard output
 *
 * Transformed: <relational expression> ->
 *           <primary a_relational expression><relational expression^1>
 *         | <primary s_relational expression><relational expression^2>
 * FIRST(relational expression) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 */
void relational_expression(void) {
    switch (lookahead_token.code) {
        case AVID_T:
        case FPL_T:
        case INL_T: /* Check for AVID, FPL, INL */
            primary_a_relational();
            relational_expression_1();
            break;
        case SVID_T:
        case STR_T: /* Check for SVID or STR */
            primary_s_relational();
            relational_expression_2();
            break;
        default: /* It cannot be empty or anything else */
            syn_printe();
    }
    gen_incode("PLATY: Relational expression parsed");
}

/*
 * Purpose: <relational expression^1> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: primary_a_relational(): go to the <primary a relational> production
 *                             syn_printe(): display the token code and attribute as a syntax error
 *                                  match(): match the lookahead token with the expected token
 *
 * <relational expression^1> -> ==  <primary a_relational expression>
 *                         | !=  <primary a_relational expression>
 *                         | >   <primary a_relational expression>
 *                         | <   <primary a_relational expression>
 * FIRST(relational expression^1) = { ==, !=, >, < }
 */
void relational_expression_1(void) {
    if (lookahead_token.code == REL_OP_T) { /* Check for any relational operator */
        match(lookahead_token.code, lookahead_token.attribute.arr_op); /* Process any */
        primary_a_relational();
    } else
        syn_printe(); /* It cannot be anything other than a relational operator */
}

/*
 * Purpose: <relational expression^2> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: primary_s_relational(): go to the <primary s relational> production
 *                             syn_printe(): display the token code and attribute as a syntax error
 *                                  match(): match the lookahead token with the expected token
 *
 * <relational expression^2> -> ==  <primary s_relational expression>
 *                         | !=  <primary s_relational expression>
 *                         | >   <primary s_relational expression>
 *                         | <   <primary s_relational expression>
 * FIRST(relational expression^2) = { ==, !=, >, < }
 */
void relational_expression_2(void) {
    if (lookahead_token.code == REL_OP_T) { /* Check for any relational operator */
        match(lookahead_token.code, lookahead_token.attribute.arr_op); /* Process any */
        primary_s_relational();
    } else
        syn_printe(); /* It cannot be anything other than a relational operator */
}

/*
 * Purpose: <primary a relational> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: gen_incode(): output the parsed line to standard output
 *                   syn_printe(): display the token code and attribute as a syntax error
 *                        match(): match the lookahead token with the expected token
 *
 * <primary a_relational expression> -> AVID_T | FPL_T | INL_T
 * FIRST(primary a_relational expression) = { AVID_T, FPL_T, INL_T }
 */
void primary_a_relational(void) {
    switch (lookahead_token.code) {
        case AVID_T:
        case FPL_T:
        case INL_T: /* Check for AVID, FPL, or INL */
            match(lookahead_token.code, lookahead_token.attribute.arr_op);
            break;
        default:
            syn_printe(); /* It cannot be anything that isn't in the first set */
    }
    gen_incode("PLATY: Primary a_relational expression parsed");
}

/*
 * Purpose: <primary s relational> production
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: gen_incode(): output the parsed line to standard output
 *    primary_string_expression(): go to the <primary string expression> production
 *
 * <primary s_relational expression> -> <primary string expression>
 * FIRST(primary s_relational expression) = { SVID_T, STR_T }
 */
void primary_s_relational(void) {
    primary_string_expression();
    gen_incode("PLATY: Primary s_relational expression parsed");
}

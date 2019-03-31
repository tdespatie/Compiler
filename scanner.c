/*
*   File name: scanner.c
*   Compiler: GNU GCC Compiler
*   Author: [Tyler Despatie, 040-694-672]
*   Course: CST 8152 – Compilers, Lab Section: 401
*   Assignment: 2
*   Date: 25 October 2013
*   Professor: Sv. Ranev
*   Purpose: Scanner for the PLATYPUS language
*   Function list: b_create(), b_addc(), b_reset(), b_destroy(),
*                  b_getsize(), b_setmark(), b_getmark(), b_getc()
*                  b_retract(), b_get_getc_offset(),b_isempty(), strcat(),
*                  b_set_setc_offset(), b_get_chmemloc(), strncpy(),
*                  strcpy(), strcat(), strcpysize(), atool(), strtod(), strcmp(),
*                  strtol(), strstr(), strlen(), assert(), isalpha(),
*                  isdigit(), get_next_state(), char_class(), aa_func02(),
*                  aa_func03(), aa_func05(), aa_func08(), aa_func11(), aa_func12()
*                  scanner_init(), iskeyword(), mlwpar_next_token(), isalnum()
*/

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

#define NDEBUG       /* to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define RUNTIME_ERROR scerrnum = R_ONE; return aa_table[ES]("RUN TIME ERROR: "); /* RUNTIME ERROR */
#define CHECK_EOF c == (unsigned char)EOF || c == '\0'    /* Handle EOF */
#define CHECK_NOTEOF c != (unsigned char)EOF && c != '\0' /* Loop while c != EOF */
#define CHECK_NEWLINE  c == '\n' || c == '\r'             /* Handle a new line character */
#define CHECK_NOTNEWLINE c != '\n' && c != '\r'           /* Loop until we hit a new line */

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
extern STD sym_table;
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char ch); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
/*static int iskeyword(char * kw_lexeme); *//*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
void strcpysize(char *, char[], size_t);
int scanner_init(Buffer * sc_buf);
Token mlwpar_next_token(Buffer * sc_buf);

/*
*   Purpose: Ensure sc_buf has been created, set its' getc_offset to 0, reset str_LBL and line count
*   Author: Sv. Ranev
*   History/Versions: 1.0
*   Called functions: b_isempty(): check if buffer is empty,
*                     b_set_getc_offset(): set getc_offset of buffer specified and offset specified
*                     b_reset(): reset buffer structure's variables
*   Parameters: Buffer *: pointer to buffer structure
*   Return value: 0: if successful, 1: if failure occurs
*/
int scanner_init(Buffer * sc_buf) {
  	if(b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	b_set_getc_offset(sc_buf,0);/* in case the buffer has been read previously  */
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
*   Purpose: Recognizes characters in the buffer and handles them appropriately.
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions:b_create(), b_addc(), b_destroy(),
*                    b_getsize(), b_setmark(), b_getmark(), b_getc()
*                    b_retract(), b_get_getc_offset(), strcat(),
*                    b_set_getc_offset(), b_get_chmemloc(), strstr(),
*                    get_next_state(), isalnum()
*   Parameters: Buffer *: pointer to buffer structure
*   Return value: Token: Returns a Token that relates to a character in the buffer
*   Algorithm: Determine if a token is recognized and put it through the finite state machine.
*              If a token is not recognized, return an error token accordingly.
*/
Token mlwpar_next_token(Buffer * sc_buf)
{
   Token t;         /* token to return after recognition */
   unsigned char c; /* input symbol */
   int state = 0;   /* initial state of the FSM */
   short lexstart;  /*start offset of a lexeme in the input buffer */
   short lexend;    /*end   offset of a lexeme in the input buffer */
   short lexpos;    /*index position */
   int accept = NOAS; /* type of state - initially not accepting */

        while (1){ /* endless loop broken by token returns it WILL GENERATE A WARNING */

        c = (unsigned char)b_getc(sc_buf); /* GET THE NEXT SYMBOL FROM THE INPUT BUFFER */

        /* Stuff we don't want */
        if (CHECK_EOF) { t.code = SEOF_T; break; }                      /* End of file indication */
        if (c == ' '  || c == '\t' || c == '\v' || c == '\f') continue; /* Remove whitespace */
        if (CHECK_NEWLINE) { line++; continue; }                        /* New line */

        /* Operators */
        switch (c) {
            case '{': { t.code = LBR_T; /* no attribute */ return t; break; }
            case '}': { t.code = RBR_T; /* no attribute */ return t; break; }
            case '(': { t.code = LPR_T; /* no attribute */ return t; break; }
            case ')': { t.code = RPR_T; /* no attribute */ return t; break; }
            case ',': { t.code = COM_T; /* no attribute */ return t; break; }
            case ';': { t.code = EOS_T; /* no attribute */ return t; break; }
            case '>': { t.code = REL_OP_T; t.attribute.rel_op = GT;    return t; break; }
            case '+': { t.code = ART_OP_T; t.attribute.arr_op = PLUS;  return t; break; }
            case '-': { t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t; break; }
            case '*': { t.code = ART_OP_T; t.attribute.arr_op = MULT;  return t; break; }
            case '/': { t.code = ART_OP_T; t.attribute.arr_op = DIV;   return t; break; }
            default: break; /* If it's not recognized, move to the special cases below */
        }

        /* Special cases */
        if (c == '!'){ /* If the character is an exclamation mark, something else must follow */
            lexstart = (short)b_setmark(sc_buf, b_get_getc_offset(sc_buf)); /* Mark where we start to make sure we can return */
            c = (unsigned char)b_getc(sc_buf);

            if (c == '=') {                             /* Check if it's followed by an = sign */
                t.code = REL_OP_T;                       /* Set the Relational Operator flag */
                t.attribute.rel_op = NE;                 /* Set the Not Equal attribute */
                return t;
            }
             while (CHECK_NOTEOF && CHECK_NOTNEWLINE) {  /* Loop to discard the rest of the line */
                c = (unsigned char)b_getc(sc_buf);
                if (CHECK_NEWLINE) line++;               /* We've hit the end of the line; increment */
                if (CHECK_EOF) b_retract(sc_buf);        /* Retract because we've already written code above to handle it */
             }

             c = (unsigned char)*b_get_chmemloc(sc_buf,lexstart); /* See what follows the exclamation mark */
             if (c == '<') continue;               /* Check if it's a comment, then start at the beginning again */

             t.attribute.err_lex[0] = *b_get_chmemloc(sc_buf,lexstart-1);   /* Error character */
             t.attribute.err_lex[1] = *b_get_chmemloc(sc_buf,lexstart);     /* Error character */
             t.attribute.err_lex[2] = '\0';                                 /* Add string terminator */
             t.code = ERR_T;                                                /* Set error flag */
            return t;
        }
         if (c == '<') {                    /* Check for less than operator */
            c =(unsigned char)b_getc(sc_buf);/* Move to the next character */
            if (c == '>') {                 /* Check for concatenation operator */
                t.code = SCC_OP_T;
            } else {                        /* If it's not, it must be less than operator */
                t.code = REL_OP_T;          /* Set relational operator flag */
                t.attribute.rel_op = LT;    /* Set less than operator attribute */
                b_retract(sc_buf);          /* We've looked to far, it's only a less than operator */
            }
            break;
         }

         if (c == '=') {                    /* Check for assignment operator */
            c = (unsigned char)b_getc(sc_buf);             /* Move to the next character */
            if (c == '=') {                 /* Check for a boolean equals operator */
                t.code = REL_OP_T;          /* EQUALS found, set relational operator flag */
                t.attribute.rel_op = EQ;    /* Set Equals as an attribute */
            } else {
                t.code = ASS_OP_T;          /* It's just an assignment operator */
                b_retract(sc_buf);          /* We've gone too far; Retreat! */
            }
             break;
         }

         if (c == '.') {                         /* Check for period/decimal */
            char* logArr[] = { "AND.", "OR." };  /* We're checking for an .AND. or an .OR. */
            char cString[4];                     /* Temporary place to store the next 4 characters in our buffer */
            int temp_line = line;                /* Necessary due to the way I look ahead for .AND. or .OR. */

            lexstart = (short)b_setmark(sc_buf, b_get_getc_offset(sc_buf)); /* Mark where we started */

            for(lexpos=0; lexpos<4; lexpos++) {                /* Fill up our temporary string to compare */
                c = (unsigned char)*b_get_chmemloc(sc_buf,(lexstart+lexpos)); /* Grab the character at the next location */
                if (CHECK_NEWLINE) line++;                     /* Handle if we hit a new line by accident */
                if (CHECK_EOF) break;                          /* Handle if we hit the end of file */
                cString[lexpos] = (char)c;                     /* Add the character to our temporary string */
            }
            if (strstr(cString,logArr[0])) {                  /* Check if our temporary string contains AND. */
                t.code = LOG_OP_T;                            /* Set the operator flag */
                t.attribute.log_op = AND;                     /* Set the operator type */
                b_set_getc_offset(sc_buf, (lexstart+4));      /* Let's move ahead past the operator */
                return t;
            }
            if (strstr(cString,logArr[1])) {                  /* Check if our temporary string contains OR. */
                t.code = LOG_OP_T;                            /* Set the operator flag */
                t.attribute.log_op = OR;                      /* Set the operator type */
                b_set_getc_offset(sc_buf, (lexstart+3));      /* Let's move ahead past the operator */
                return t;
            }
            t.code = ERR_T;                        /* Nothing matched, we've an error amongst us */
            line = temp_line;                      /* We need the line number as it was before we looked ahead */
            t.attribute.err_lex[0] = *b_get_chmemloc(sc_buf, lexstart-1); /* What we found instead of what we want */
            t.attribute.err_lex[1] = '\0';
            return t;
        }

        if (c == '"') {  /* String Literal */
            int length_flag = 0;
            lexstart = b_get_getc_offset(sc_buf);             /* Get the current getc_offset */
            b_setmark(str_LTBL, b_getsize(str_LTBL));         /* Set a mark in the str_LTBL buffer */

            do {
                c = (unsigned char)b_getc(sc_buf);
                if (c == '"') t.code = STR_T;                 /* Loop until we hit '"' */
                if (CHECK_NEWLINE) line++;                    /* Handle if we hit a new line */
            } while (CHECK_NOTEOF && c != '"');               /* Break out if we hit EOF or '"' */

            lexend = b_get_getc_offset(sc_buf);               /* Get the position we end up at after the loop */

            if (CHECK_EOF) {                                  /* Handle EOF */
                for (lexpos=0;(lexpos+lexstart) < lexend; lexpos++) {  /* Loop from the start to where we ended up */
                    if (lexpos <= ERR_LEN)                    /* Check if the string is going to be too long */
                        t.attribute.err_lex[lexpos] = *b_get_chmemloc(sc_buf,(lexstart+lexpos)-1); /* Write the Illegal string literal into err_lex */
                    else {
                        length_flag = 1;
                        t.attribute.err_lex[ERR_LEN-3] = '\0';   /* Terminate the string because it's too long */
                        strcat(t.attribute.err_lex, "...");      /* Add "..." to the end of the string to show it's been truncated */
                        break;
                    }
                }

                if (length_flag == 0) 
                    t.attribute.err_lex[lexpos] = '\0';
                
                t.code = ERR_T;     /* Return an error, it's an illegal string */
                b_retract(sc_buf);  /* Retract so our code at the beginning handles SEOF */
                return t;
            }

            for (lexpos=lexstart; lexpos<lexend; lexpos++) {  /* loop through the string literal, adding each character */
                c = (unsigned char)*b_get_chmemloc(sc_buf,lexpos);
                if (c != '"')
                    if(!b_addc(str_LTBL, (char)c)) {             /* Add the string literal to the string literal buffer */
                       RUNTIME_ERROR                        /* If b_addc returns NULL, it's a runtime error */
                    }
            }

            if (!b_addc(str_LTBL, '\0')) {                 /* Add a null terminating character to the string literal buffer */
                RUNTIME_ERROR                               /* If b_addc returns NULL, it's a runtime error */
            }

            t.attribute.str_offset = b_getmark(str_LTBL);  /* Provide it's offset */
            return t;
        }

         if (isalnum(c)) {
              state = 0;                                                 /* Start with state 0 */
              lexstart = (short)b_setmark(sc_buf, b_get_getc_offset(sc_buf)-1); /* Mark the starting point */

              state = get_next_state(state, (char)c, &accept);  /* Go through the transition table to find the next state */
              while (accept == NOAS) {
                 c = (unsigned char)b_getc(sc_buf);
                 state = get_next_state(state, (char)c, &accept);  /* Go through the transition table to find the next state */
              }

              lexend = b_get_getc_offset(sc_buf);             /* Got the offset so I can properly size my buffer */

              lex_buf = b_create((lexend-lexstart)+1,0,'f');  /* Since we know how much space we need, I used fixed mode */
              if (!lex_buf) { RUNTIME_ERROR }                  /* Failed to create buffer */

              if (accept == ASWR)
                b_retract(sc_buf);                            /* RETRACT  getc_offset IF THE FINAL STATE IS A RETRACTING FINAL STATE */

              lexend = b_get_getc_offset(sc_buf);             /* Get the end of the lexeme */

              for (lexpos=lexstart;lexpos<lexend; lexpos++) { /* Loop through the lexeme */
                if (!b_addc(lex_buf, *b_get_chmemloc(sc_buf, lexpos))) { /* Check if b_addc can add the character */
                    RUNTIME_ERROR                                         /* If it returns NULL, it's a runtime error */
                }
              }

             if(!b_addc(lex_buf, '\0')) { /* add a null terminating character to the buffer */
                RUNTIME_ERROR              /* Handle if it cannot be added to the buffer */
              }
              /* call the appropriate function for the current state */
              t = aa_table[state]((char*)b_get_chmemloc(lex_buf,0)); 
              b_destroy(lex_buf);                    /* destroy the buffer as it is no longer needed */
            return t;
          }
        t.attribute.err_lex[0] = (char)c;     /* The character does not meet the grammar of the PLATYPUS Language */
        t.attribute.err_lex[1] = '\0';  /* Return the errornous character */
        t.code = ERR_T;
        return t;
    }
    return t;
}
/*
*   Purpose: Consult the transition table for the character at hand
*   Author: Sv. Ranev
*   History/Versions: 1.0
*   Called functions: assert()
*   Parameters: int state: current state
*                  char c: the current character to enter in our finite state machine
*             int *accept: an int pointer to the accepting state status
*   Return value: int: returns our next state
*/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];

#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif

assert(next != IS);

#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*
*   Purpose: Determine which column the character falls under in our transition table
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: isalpha(): check if the character is a letter
*                     isdigit(): check if the character is a digit
*   Parameters: char c: character to process
*   Return value: int: returns the column number in our table
*/
int char_class(char c)
{
    int column = 6;                       /* Other Column */

    if (c == '#')   { column = 5; }      /* # Column */
    if (c == '.')   { column = 4; }      /* Decimal Column */
    if (isalpha(c)) { column = 0; }      /* Alpha [A-Z a-z] */

    if (isdigit(c)) {            /* Digit [0-9] */
        int i = c - '0';         /* Convert ch to int */
        if (i <= 7) column = 2;      /* Octal [1-7] */
        if (i >= 8) column = 3;      /* Digit [8-9] */
        if (i == 0) column = 1;      /* 0 column */
    }

    return column;
}

/*
*   Purpose: Determine whether the lexeme is a keyword; if it isn't, make it an AVID.
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: iskeyword(): check if the lexeme is a keyword
*                       strncpy(): copy the VID into the vid_lex array
*                        strlen(): get the length of the lexeme
*   Parameters: char lexeme[]: lexeme to process
*   Return value: Token: Returns a Token that relates to the lexeme
*   Algorithm: Check if the lexeme is a keyword, if it isn't make it an AVID and make sure
*              it's of an appropriate length.
*/
Token aa_func02(char lexeme[]){ /* AVID/Keyword */
    Token t_temp;
    int flag;           /* We need to store the length and return value of isKeyword */
    size_t length;

    flag = iskeyword(lexeme);   /* Call iskeyword to check if the lexeme is a keyword */
    if (flag != R_FAIL_1) {     /* The function will return the keyword index, or -1 if it's not a k/w */
        t_temp.code = KW_T;                       /* Keyword Flag */
        t_temp.attribute.kwt_idx = flag;          /* Set keyword attribute */
        return t_temp;                            /* Return the token */
    }

    t_temp.code = AVID_T;                           /* If it's not a keyword it's an AVID */

    length = strlen(lexeme);                        /* Get the length of the lexeme */
    if (length > VID_LEN)                           
        lexeme[VID_LEN] = '\0';  
    else
        lexeme[length] = '\0';    /* Add a null terminator to the end of the lexeme */

    t_temp.attribute.vid_offset = st_install(sym_table, lexeme, line);

    if (t_temp.attribute.vid_offset == R_FAIL_1) {
        printf("\nError: The Symbol Table is full - install failed.\n");
        st_store(sym_table);
        exit(R_ONE);
    }

  return t_temp;
}

/*
*   Purpose: Make the lexeme an appropriate SVID.
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: strncpy(): copy the VID into the vid_lex array
*                      strlen(): get the length of the lexeme
*   Parameters: char lexeme[]: lexeme to process
*   Return value: Token: Returns a Token that relates to the lexeme
*/
Token aa_func03(char lexeme[]){ /* SVID */
    Token t_temp;
    size_t length;

    t_temp.code = SVID_T;                                  /* Set SVID Code */
    length = strlen(lexeme);                               /* Get the length of the lexeme */

    if (length > VID_LEN) {                                /* Check if the lexeme is longer than 8 characters */
        lexeme[VID_LEN-1] = '#';         /* Add a # to the array */
        lexeme[VID_LEN] = '\0';          /* Add a string terminator */
    } else {
        lexeme[length-1] = '#';          /* Add a # to the array */
        lexeme[length] = '\0';           /* Add a string terminator */
    }
    
    t_temp.attribute.vid_offset = st_install(sym_table, lexeme, line);

   if (t_temp.attribute.vid_offset == R_FAIL_1) {
        printf("\nError: The Symbol Table is full - install failed.\n");
        st_store(sym_table);
        exit(R_ONE);
    } 

  return t_temp;
}

/*
*   Purpose: Make the lexeme an appropriate DIL.
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: strncpysize(): copy a string into the err_lex array ensuring an appropriate length.
*                            strtol: convert a string into a long
*   Parameters: char lexeme[]: lexeme to process
*   Return value: Token: Returns a Token that relates to the lexeme
*/
Token aa_func05(char lexeme[]){ /* DIL */
    Token t_temp;
    long l_temp;                                             /* Used for proper conversion to avoid underflow or overflow*/
    char * cErrPtr = NULL;                                   /* Used to check if conversion caused an error */

    l_temp = strtol(lexeme, &cErrPtr, 10);                   /* Function converts String to long */

    if (cErrPtr == lexeme) {                                 /* Error checking */
        scerrnum = R_ONE;
        aa_table[ES]("RUN TIME ERROR: ");
    }

    if (l_temp > USHRT_MAX || l_temp < 0) {                  /* Check valid range (an unsigned short is 2 bytes) */
        strcpysize(t_temp.attribute.err_lex, lexeme, ERR_LEN); /* Provide error message in err_lex[] */
        t_temp.code = ERR_T;                                 /* Set error flag */
        return t_temp;                                       /* Return error token */
    } else {
        t_temp.code = INL_T;                                 /* Set Decimal Integer Literal flag */
        t_temp.attribute.int_value = (int)l_temp;            /* Assign int attribute the converted string */
    }

  return t_temp;
}

/*
*   Purpose: Make the lexeme an appropriate FPL.
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: strncpysize(): copy a string into the err_lex array ensuring an appropriate length.
*                            strtod: convert a string into a double
*   Parameters: char lexeme[]: lexeme to process
*   Return value: Token: Returns a Token that relates to the lexeme
*/
Token aa_func08(char lexeme[]){ /* FPL */
    Token t_temp;
    double d_temp;                                              /* Used for proper conversion to avoid underflow or overflow*/
    char * cErrPtr = NULL;                                      /* Used to check if conversion caused an error */

    d_temp = strtod(lexeme, &cErrPtr);                          /* Function converts String to double */

    if (cErrPtr == lexeme) {                                    /* Error checking */
        scerrnum = R_ONE;
        aa_table[ES]("RUN TIME ERROR: ");
    }
    if ((d_temp > 0 || d_temp < 0) && (d_temp > FLT_MAX || d_temp < FLT_MIN)) { /* Check if it's in a proper range */
        t_temp.code = ERR_T;                                    /* Set error flag */
        strcpysize(t_temp.attribute.err_lex, lexeme, ERR_LEN);  /* Provide error message in err_lex[] */
        return t_temp;                                          /* Return error token */
    } else {
        t_temp.code = FPL_T;                                    /* Set Floating Point Literal flag */
        t_temp.attribute.flt_value = (float)d_temp;             /* Assign float attribute the converted string */
    }

  return t_temp;
}

/*
*   Purpose: Make the lexeme an appropriate OIL.
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: strncpysize(): copy a string into the err_lex array ensuring an appropriate length.
*                             atool: convert a string into an octal
*   Parameters: char lexeme[]: lexeme to process
*   Return value: Token: Returns a Token that relates to the lexeme
*/
Token aa_func11(char lexeme[]){ /* OIL */
    Token t_temp;
    long l_temp;                                            /* Used for proper conversion to avoid underflow or overflow*/

    l_temp = atool(lexeme);                                 /* Convert to octal */

    if (l_temp > SHRT_MAX || l_temp < SHRT_MIN) {           /* Check valid range */
        strcpysize(t_temp.attribute.err_lex, lexeme, ERR_LEN); /* Provide error message in err_lex[] */
        t_temp.code = ERR_T;                                /* Set error flag */
        return t_temp;                                      /* Return error token */
    } else {
        t_temp.code = INL_T;                                /* Set Decimal Integer Literal flag */
        t_temp.attribute.int_value = (int)l_temp;           /* Assign int attribute the converted string */
    }

  return t_temp;
}

/*
*   Purpose: Return an error token
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: strncpysize(): copy a string into the err_lex array ensuring an appropriate length.
*   Parameters: char lexeme[]: lexeme to process
*   Return value: Token: Returns a Token that relates to the lexeme
*/
Token aa_func12(char lexeme[]){ /* Error state */
    Token t_temp;
    t_temp.code = ERR_T;                                   /* Set error token code */
    strcpysize(t_temp.attribute.err_lex, lexeme, ERR_LEN); /* Provide error message */
    return t_temp;
}

/*
*   Purpose: Convert a lexeme into base 8.
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: strtol: convert a string into a long
*   Parameters: char *lexeme: lexeme to process
*   Return value: long: base 8 value
*/
long atool(char * lexeme){
    char *cErrCheck = NULL; /* Used for error checking */
    long l_temp;            /* Used for proper conversion */

    if (!lexeme) {          /* Check if it's a valid pointer */
        scerrnum = R_ONE;
        aa_table[ES]("RUN TIME ERROR: ");
    }

    l_temp = strtol(lexeme, &cErrCheck, 8); /* Check for proper conversion */

    if (cErrCheck == lexeme) {
        scerrnum = R_ONE;
        aa_table[ES]("RUN TIME ERROR: ");
    }

    return l_temp;
}

/*
*   Purpose: Copy the contents of the source into the destination with a guarenteed length
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Parameters: char *dest: destination string
*            char source[]: source string (lexeme)
*               int length: length of string
*   Return value: void
*/
void strcpysize(char *dest, char source[], size_t length) {
    unsigned int i;

    if (!dest) { /* Check it it's a valid pointer */
        scerrnum = R_ONE;
        aa_table[ES]("RUN TIME ERROR: ");
    }
    for(i=0;i<length;i++) {
        dest[i] = source[i];           /* Copy contents of source into destination */
        if (source[i] == '\0') return; /* Check if we've reached the end of the string */
    }
    dest[i] = '\0'; /* If there wasn't a null terminator, add one */
    return;
}

/*
*   Purpose: Check if the lexeme is a keyword.
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: strcmp: compare lexeme with a keyword in the kw_lexeme array
*   Parameters: char *kw_lexeme: lexeme to process
*   Return value: int: index of the keyword found or -1 if no match was found
*/
int iskeyword(char * kw_lexeme){
    int i;
     for(i=0;i<KWT_SIZE;i++) {                    /* Loop through the Keyword table */
        if (strcmp(kw_lexeme, kw_table[i]) == 0)  /* Check for a match */
            return i;
     }
    return R_FAIL_1; /* No match was found, return -1 */
}

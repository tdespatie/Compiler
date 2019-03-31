/*
 *   File name: stable.c
 *   Compiler: GNU GCC Compiler
 *   Author: [Tyler Despatie, 040-694-672]
 *   Course: CST 8152 – Compilers, Lab Section: 401
 *   Assignment: 3
 *   Date: 15 November 2013
 *   Professor: Sv. Ranev
 *   Purpose: Prototypes for stable.c
 *   Function list: st_create(), st_install(), st_lookup(), st_update_type(),
 *                st_update_value(), st_get_type(),st_destroy(), st_print(),
 *                st_store(), st_sort(), malloc(), b_create(), free(), st_lookup(),
 *	              strlen(), b_get_chmemloc(), b_addc(), st_incoffset(), strcmp(),
 *		          b_destroy(), st_setsize(), printf(), fopen(), fprintf(),
 *                st_get_type(), fclose(), tolower()
 */
#include "stable.h"

static void st_setsize(void);
static void st_incoffset(void);
extern STD sym_table;

/*
 * Purpose: Creates a new empty symbol table
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions:   malloc(): allocate memory
 *                   b_create(): create a buffer
 *                       free(): free allocated memory
 * Parameters: int st_size: size of the symbol table
 * Return value: sym_table: return a pointer to the STD
 */
STD st_create(int st_size)
{
    STD sym_table; /* local sym_table variable for memory allocation */
    sym_table.pstvr = NULL;
    sym_table.plsBD = NULL;
    sym_table.st_size = 0;

    if (st_size <= 0) return sym_table;

    sym_table.pstvr = (STVR*)malloc((size_t)((int)sizeof(STVR) * st_size));
    if (!sym_table.pstvr) return sym_table; /* Check for successful allocation */

    /* Create a lexeme storage buffer */
    sym_table.plsBD = b_create(DEFAULT_CAPACITY, DEFAULT_INC_FACTOR, 'a');
    sym_table.st_offset = 0;

    if (!sym_table.plsBD) {    /* If the buffer fails to create */
        free(sym_table.pstvr); /* Free the memory it succesfully allocated */
        return sym_table;
    }

    sym_table.st_size = st_size; /* Function has been successful, set st_size */
    return sym_table;
}

/*
 * Purpose: This function installs a new entry (VID record) in the symbol table.
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: st_lookup(): lookup a lexeme in the STVR
 *                      strlen(): find the length of a string
 *              b_get_chmemloc(): Return a ptr to the location of a char in the buffer
 *                      b_addc(): add a char to the buffer
 *                st_incoffset(): increment the symbol table offset
 *                     tolower(): when checking the first index of the lexeme, make it lowercase
 * Parameters: STD sym_table: pointer to the symbol table
 *              char *lexeme: lexeme to lookup or add to the buffer
 *                  int line: line number the lexeme was on
 * Return value: int: return the symbol table offset of the VID
 * Algorithm: Get the pointer to the STVR, set the lexeme's location in the buffer, update its'
 *            status field, add each character of the lexeme to the symbol table
 */
int st_install(STD sym_table, char *lexeme, int line)
{

    STVR *temp_stvr = NULL;
    int lex_offset, length;
    int i = 0, r_flag = 0;
    short pos;

    if (!sym_table.plsBD) return R_FAIL_1;

    lex_offset = st_lookup(sym_table, lexeme); /* Check the symbol table for an existing lexeme */
    if (lex_offset != R_FAIL_1) return lex_offset; /* Lexeme exists in the symbol table */
    if (sym_table.st_size <= sym_table.st_offset) return R_FAIL_1; /* Check for space */

    length = (int)strlen(lexeme)+1;
    temp_stvr = &sym_table.pstvr[sym_table.st_offset]; /* Point to the STVR */
    temp_stvr->plex = b_get_chmemloc(sym_table.plsBD, b_getsize(sym_table.plsBD)); /* Point to the VID */
    temp_stvr->o_line = line;                          /* Set the line number */

    temp_stvr->status_field &= INITIAL; /* Set the bits to all 0's */
    temp_stvr->status_field |= RESET;   /* Set the reserved bits to 1's */

    if (lexeme[length-2] == '#') {
        temp_stvr->status_field |= STRING;        /* set the status field to 0110 for a string */
        temp_stvr->status_field |= LSB;           /* set the status field to 0111 to prevent updating */
        temp_stvr->i_value.str_offset = R_FAIL_1; /* -1 because it's a string */
    } else
        switch(tolower(lexeme[0])) {   /* Check the first letter of the lexeme to determine the type */
            case 'i':                  /* Fall through because those are all handled as a AVID */
            case 'o':                  /* Fall through because those are all handled as a AVID */
            case 'd':                  /* Fall through because those are all handled as a AVID */
            case 'n': temp_stvr->i_value.int_val = 0;      /* Set integer value to 0 */
                      temp_stvr->status_field |= INTEGER;  /* set status field to 0010 for an int */
                      break;
            default:  temp_stvr->i_value.fpl_val = 0;      /* set floating point value to 0 */
                      temp_stvr->status_field |= FLOAT;    /* set status field to 0100 for a float */
                      break;
        }

    while (i < length) {
        if(!b_addc(sym_table.plsBD, lexeme[i++]))    /* Add each character in the lexeme to lexeme storage */
            return R_FAIL_1;
        if (b_get_r_flag(sym_table.plsBD) == R_ONE)  /* Check the relocation flag */
            r_flag = R_ONE;                          /* Set the relocation flag */
    }

    if (r_flag == R_ONE)                             /* Check the relocation flag */
        for (i = pos = 0; i <= sym_table.st_offset; i++) {                   /* Memory was relocated */
            sym_table.pstvr[i].plex = b_get_chmemloc(sym_table.plsBD, pos);  /* Get the new location */
            pos += (short)strlen(b_get_chmemloc(sym_table.plsBD, pos)) + 1;  /* Move to the next lexeme */
        }

#ifdef DEBUG
    for(i = 0; i <= sym_table.st_offset; i++)
        printf("plex: %s index: %i\n", sym_table.pstvr[i].plex, i); /* Used to check the contents of plex */
#endif

    st_incoffset(); /* Increment st_offset before returning */

    return sym_table.st_offset;
}

/*
 * Purpose: Searches for a lexeme in the symbol table.
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: strcmp(): compare strings
 * Parameters: STD sym_table: copy of the symbol table
 *              char *lexeme: lexeme to lookup
 * Return value: int: symbol table offset of the VID or -1 if not found
 */
int st_lookup(STD sym_table, char *lexeme)
{
    int i;
    if (lexeme && !(SIZE_ZERO))
        for (i=(sym_table.st_offset-1); i>=0; i--)             /* Check each VID in the table */
            if (strcmp(sym_table.pstvr[i].plex, lexeme) == 0)  /* If the lexeme exists, return its' offset */
                return i;
#ifdef DEBUG
    printf("missed: %s\n", lexeme);
#endif
    return R_FAIL_1;
}

/*
 * Purpose: Update the data type indicator in the STVR specified by vid_offset.
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: None
 * Parameters: STD sym_table: copy of the symbol table
 *            int vid_offset: the symbol table VID offset to update
 *               char v_type: variable type
 * Return value: int: symbol table offset of the VID
 */
int st_update_type(STD sym_table, int vid_offset, char v_type)
{
    /* Temporary status field used for bitwise comparison */
    unsigned short status = sym_table.pstvr[vid_offset].status_field;

    /* Ensure that the vid_offset is in range of the table */
    if (SIZE_ZERO || vid_offset > sym_table.st_offset || vid_offset < 0) return R_FAIL_1;

    /* Ensure that the VID hasn't already been updated */
    if ((status & LSB) == 1) return R_FAIL_1;

    /* Flip the bits to leave only the data type as 1's */
    status &= URESET;

    switch (v_type) {
        case 'F':  status |= FLOAT; break;
        case 'I':  status |= INTEGER; break;
        default:   return R_FAIL_1;
    }
    /* Now that the type has been updated, set the update flag */
    status |= LSB;

    sym_table.pstvr[vid_offset].status_field = status;

    return vid_offset;
}

/*
 * Purpose: Update the i_value of the variable specified by vid_offset.
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: None
 * Parameters: STD sym_table: copy of the symbol table
 *            int vid_offset: the symbol table VID offset to update
 *      InitialValue i_value: initial value of the VID
 * Return value: int: symbol table offset of the VID
 */
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value)
{
    if (SIZE_ZERO || vid_offset > sym_table.st_offset || vid_offset < 0) return R_FAIL_1;
    sym_table.pstvr[vid_offset].i_value = i_value; /* Update the Initial Value */
    return vid_offset;
}

/*
 * Purpose: Returns the type of the variable specified by vid_offset.
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: None
 * Parameters: STD sym_table: copy of the symbol table
 *            int vid_offset: the symbol table VID offset to update
 * Return value: char: type of the VID
 */
char st_get_type(STD sym_table, int vid_offset)
{
    /* Get the status field for bitwise comparison */
    unsigned short s_field = sym_table.pstvr[vid_offset].status_field;

    if (SIZE_ZERO || vid_offset > sym_table.st_offset || vid_offset < 0) return R_FAIL_1;

    s_field &= ~URESET; /* Remove the reserved bits that are set to one and the update flag */

    switch (s_field) {
        case STRING:  return 'S';       /* Check if it's a String */
        case FLOAT:   return 'F';       /* Check if it's a float */
        case INTEGER: return 'I';       /* Check if it's an int */
        default:      return R_FAIL_1;  /* It's an error */
    }
}

/*
 * Purpose: Free the allocated memory of the symbol table and set st_size to 0.
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: free(): free allocated memory
 *              b_destroy(): destroy the buffer
 *             st_setsize(): set the symbol table size to 0
 * Parameters: STD sym_table: copy of the symbol table
 * Return value: None
 */
void st_destroy(STD sym_table)
{
    if (!sym_table.pstvr) return; /* Ensure it exists before freeing */
    free(sym_table.pstvr);        /* free the variable record */
    b_destroy(sym_table.plsBD);   /* destory the lexeme storage */
    st_setsize();                 /* set the size of the symbol table to 0 */
}

/*
 * Purpose: Print the contents of the symbol table to the standard output.
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: printf(): standard output
 * Parameters: STD sym_table: copy of the symbol table
 * Return value: int: number of entries printed
 */
int st_print(STD sym_table)
{
    int i = 0;
    if (SIZE_ZERO) return R_FAIL_1;

    printf("\nSymbol Table\n____________\n\nLine Number Variable Identifier\n");

    for(i=0; i < sym_table.st_offset; i++) /* Print out the STVRs' and line numbers*/
        printf("%2i          %s\n", sym_table.pstvr[i].o_line, sym_table.pstvr[i].plex);

    return i; /* return the number of entries printed out */
}

/*
 * Purpose: Set the symbol table size to 0.
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: None
 * Parameters: None
 * Return value: None
 */
static void st_setsize(void)
{   sym_table.st_size = 0;  }

/*
 * Purpose: Increment the symbol table offset.
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: None
 * Parameters: None
 * Return value: None
 */
static void st_incoffset(void)
{   sym_table.st_offset++;  }

/*
 * Purpose: Stores the symbol table into a file named $stable.ste.
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: fopen(): open the file to write to
 *                  printf(): print to the standard output
 *                 fprintf(): print to the file
 *             st_get_type(): get the type of the VID
 *                  fclose(): close the file
 * Parameters: STD sym_table: copy of the symbol table
 * Return value: int: returns the number of records stored
 */
int st_store(STD sym_table)
{
    STVR *temp_stvr;
    char *fname = "$stable.ste";  /* name of the file to write to */
    FILE *stf = fopen(fname,"w"); /* open the file so that we can write to it */
    char *output = '\0';          /* format to print */
    int i = 0;

    if (SIZE_ZERO || !stf) return R_FAIL_1;

    fprintf(stf, "%i", sym_table.st_size); /* Print out the size of the symbol table */

    for (i=0; i < sym_table.st_offset; i++) { /* format each record to be written */
        temp_stvr = &sym_table.pstvr[i];
        switch ( st_get_type(sym_table, i) ) {
            case 'F': output = " %X %i %s %i %.2f";
                      break;
            case 'I':
            case 'S': output = " %X %i %s %i %i";
                      break;
            default: return R_FAIL_1;
        }
        /* Apply the format and write to the file each STVR */
        fprintf(stf,output,temp_stvr->status_field,strlen(temp_stvr->plex),
                temp_stvr->plex,temp_stvr->o_line,temp_stvr->i_value.int_val);
        /* Check if any writing has failed */
        if (ferror(stf)) return R_FAIL_1;
    }

    printf("\nSymbol Table stored.\n");
    fclose(stf);

    return i;
}

/*
 * Purpose: Sort the symbol table (not required).
 * Author: Tyler Despatie
 * History/Versions: 1.0
 * Called functions: None
 * Parameters: STD sym_table: copy of the symbol table
 *              char s_order: order of some sort?
 * Return value: int: 0 for this implementation
 */
int st_sort(STD sym_table, char s_order) {
    return 0; /* Generates 2 warnings, for unused parameters */
}

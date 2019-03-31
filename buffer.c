/*
*   File name: buffer.c
*   Compiler: GNU GCC Compiler
*   Author: [Tyler Despatie, 040-694-672]
*   Course: CST 8152 – Compilers, Lab Section: 401
*   Assignment: 1
*   Date: 25 September 2013
*   Professor: Sv. Ranev
*   Purpose: The Buffer Data Structure
*   Function list: b_create(), b_addc(), b_reset(), b_destroy(), b_isfull(),
*                  b_getsize(), b_getcapacity(), b_setmark(), b_getmark(),
*                  b_getmode(), b_load(), b_isempty(), b_eob(), b_printc(),
*                  b_print(), b_pack(), b_get_r_flag(), b_retract(), b_get_getc_offset(),
*                  b_set_setc_offset(), b_get_chmemloc()
*/

#include "buffer.h"
/*#define DEBUG*/

/*
*   Purpose: Creates the Buffer structure, allocates memory for the character array, and sets the mode
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: calloc(): clearly allocate new memory, malloc(): allocate new memory, free(): free memory
*   Parameters: init_capacity:type short, min 0, max SHRT MAX.
*               inc_factor:type char, increment factor
*               o_mode:type char, min -1, max 1.
*   Return value: Returns Buffer* which points to Buffer Structure or NULL if it cannot create buffer
*   Algorithm: Verify parameters, allocate memory, set necessary structure values
*/
Buffer * b_create (short init_capacity, char inc_factor, char o_mode)
{
    Buffer* buffer; /* New buffer structure */
    if (init_capacity < 0) return NULL;

    buffer = (Buffer*) calloc(1, sizeof(Buffer)); /* Create a Buffer Structure (Buffer Descriptor) */
    if (!buffer) return NULL;

    if (o_mode == 'f' || inc_factor == 0) { /* Check if Fixed Mode set */
        buffer->mode = 0;
        buffer->inc_factor = 0;
    /*} else if (o_mode == 'f' && inc_factor != 0) {
        buffer->mode = 0;
        buffer->inc_factor = 0; Mentioned in the instructions but the code is redundant*/
    } else if (o_mode == 'a' && inc_factor >= 1) { /* Check if valid Additive Mode */
        buffer->mode = 1;
        buffer->inc_factor = (unsigned char)inc_factor; /* conversion warning generated */
    } else if (o_mode == 'm' && (((unsigned char)inc_factor >= 1) && ((unsigned char)inc_factor <= 100))) { /* Check if valid Multiplactive Mode */
        buffer->mode = -1;
        buffer->inc_factor = inc_factor;
    } else { /* in case of invalid mode return NULL */
        free(buffer);
        return NULL;
    }

    buffer->ca_head = (char *) malloc((size_t)init_capacity * (sizeof(char))); /* Allocate memory to the character array */
    if (!buffer->ca_head) return NULL;

    buffer->capacity = init_capacity;
    return buffer;
}
/*
*   Purpose: Add a character to character array if possible, if not expand character array
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: realloc(): reallocate memory
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*               symbol:type char, min 0
*   Return value: Returns Buffer* which points to Buffer Structure
*   Algorithm: Verify parameters passed, check if char array is full, if it is
*              expand, if not add the symbol to the end. Modify necessary structure values
*/
Buffer *b_addc (Buffer * const pBD, char symbol)
{
    char *ptrCmpr; /* Pointer to safely reallocate memory */
    int iAvailSpace = 0, iNewIncrement = 0, iNewCapacity = 0;

    pBD->r_flag =  0;

    if (!pBD || pBD->capacity < 0) return NULL;
    if ((pBD->capacity +1) < 0) return pBD;

    if (pBD->capacity == pBD->addc_offset) {
        switch (pBD->mode) {
            case -1: /* Multiplicative mode */
                if (pBD->capacity == SHRT_MAX) return pBD; /* Check to see if it's already maxed to save time */

                iAvailSpace = (SHRT_MAX - pBD->capacity); /* Algrorithm provided */
                iNewIncrement = (iAvailSpace * ((unsigned char)pBD->inc_factor) / 100); /* Algrorithm provided */
                iNewCapacity = (pBD->capacity + iNewIncrement); /* Algrorithm provided */

                if (iNewCapacity < 0) return pBD; /* Ensure the new capacity is not negative, just in case */
                if ((pBD->capacity < SHRT_MAX && iNewCapacity > SHRT_MAX) || iNewIncrement == 0) /* Check to see if it can be incremented */
                    iNewCapacity = SHRT_MAX; /* Make the capacity the maximum because it cannot be incremented further */

                pBD->capacity = (short)iNewCapacity;/* Set the capacity to our new capacity */
                break;

            case 1: /* Additive mode */
                if (((int)(pBD->capacity + pBD->inc_factor)) > SHRT_MAX)  /* Check if the capacity cannot be expanded further */
                    return pBD;

                pBD->capacity += (unsigned char)pBD->inc_factor; /* Expand the capacity by the increment factor */
                break;

            default: /* Fixed mode */
                return NULL; /* Cannot be expanded */
                break;

        }
            ptrCmpr = (char *) realloc(pBD->ca_head, (size_t)pBD->capacity); /* Safely reallocate memory */
            if (!ptrCmpr) return NULL;

            if (ptrCmpr != pBD->ca_head) {
                pBD->r_flag = SET_R_FLAG; /* Set relocation flag because the memory has shifted */
                pBD->ca_head = ptrCmpr;  /* Keep existing buffer contents intact */
            }

    }
        pBD->ca_head[pBD->addc_offset++] = symbol; /* add symbol to array and increment offset*/
    return pBD;
}
/*
*   Purpose: Reset all necessary Buffer structure values back to default without clearing contents
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: 0 if successful, -1 if unsuccessful.
*/
int b_reset(Buffer * const pBD){
    if (!pBD) return R_FAIL_1;

    pBD->eob = 0;
    pBD->addc_offset = 0; /* Start entering new characters from the beginning */
    pBD->getc_offset = 0; /* Start outputting characters from the beginning */
    pBD->mark_offset = 0; /* Reset the mark */

    return SUCCESS;
}
/*
*   Purpose: Destroy the Buffer structure by freeing allocated memory
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: free()
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: void
*/
void b_destroy(Buffer * const pBD){
    if (!pBD) return;
        free(pBD->ca_head); /* Free character array */
        free(pBD);          /* Free buffer structure */
    return;
}
/*
*   Purpose: Check if the character buffer is full
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: int: 0 if not full, 1 if full
*/
int b_isfull(Buffer * const pBD){
    if (!pBD) return R_FAIL_1;
    if (pBD->capacity == pBD->addc_offset) return R_ONE; /* If capacity is equal, it means it's full */
    return SUCCESS; /* If it's not full, return 0 */
}
/*
*   Purpose: Returns the current size of the buffer
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: short: Returns the size of the buffer
*/
short b_getsize(Buffer * const pBD){
    if (!pBD) return R_FAIL_1;
    return pBD->addc_offset; /* return the addc_offset which is the size */
}
/*
*   Purpose: Return the capacity of the buffer
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: short: Returns the capacity of the buffer
*/
short b_getcapacity(Buffer * const pBD){
    if (!pBD) return R_FAIL_1;
    return pBD->capacity;   /* return the capacity */
}
/*
*   Purpose: Move the marker in the character array
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*               mark:type short, location where to move the marker
*   Return value: int: location where the marker is set
*/
int b_setmark(Buffer * const pBD, short mark){
    if (!pBD || mark < 0 || mark > pBD->capacity) return R_FAIL_1; /* If mark is in an invalid range return -1 */
    pBD->mark_offset = mark; /* set mark location */
    return mark;             /* return mark location */
}
/*
*   Purpose: Get the location of the marker in the character array
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: short: location where the marker is set
*/
short b_getmark(Buffer * const pBD){
    if (!pBD) return R_FAIL_1;
    return pBD->mark_offset; /* return mark location */
}
/*
*   Purpose: Get the mode of the Buffer Structure
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: int: get the mode that is set, return -2 if runtime error
*/
int b_getmode(Buffer * const pBD){
    if (!pBD) return R_FAIL_2; /* Function should notify calling function if error occurs by returning invalid mode */
    return (int)pBD->mode; /* Type-casted due to compiler warning however it's not necessary */
}
/*
*   Purpose: Load the characters into the array from a file
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: fgetc(): get char from file,
*                     feof(): macro to look for EOF char
*                     b_addc(): add char to Buffer's char array
*   Parameters: fi:type FILE * const, pointer to the input file
*               pBD:type Buffer * const, pointer to Buffer structure
*   Return value: int: returns number of characters added, -1 if unsuccesful, -2 if load fail
*/
int b_load (FILE * const fi, Buffer * const pBD){
    char in; /* character input variable */
    int index = 0; /* index for how many characters added */
    if (!pBD || !fi ) return R_FAIL_1; /* Check file pointer */

    in = (char)fgetc(fi); /* Reset the eob flag by getting character before feof() is evaluated */

    while (!feof(fi)) /* Macro to check for the end of file */
    {
        if (b_addc(pBD,in) == NULL) return LOAD_FAIL; /* check if character added correctly */
        in = (char)fgetc(fi); /* Get next character */
        index++;
    }

    return index;
}
/*
*   Purpose: Check if the buffer is empty
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: int: 1 if buffer's empty, 0 if it's not empty
*/
int b_isempty(Buffer * const pBD){
    if (!pBD) return R_FAIL_1;
    if (pBD->addc_offset == 0) return R_ONE; /* If the next character position is the beginning: indicates empty */
    return SUCCESS;
}
/*
*   Purpose: Return the end of buffer status
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: int: 1 if end of buffer content reached, 0 if otherwise
*/
int b_eob(Buffer * const pBD){
    if (!pBD) return R_FAIL_1;
    return pBD->eob;
}
/*
*   Purpose: Returns the next character in the buffer if it is not at the end
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: char: next character in the buffer
*/
char b_getc(Buffer * const pBD){
    if (!pBD) {
     /* pBD->eob = 1; If pBD is null, do not change any of its' values */
        return R_FAIL_2;
    }
    if (pBD->getc_offset == pBD->addc_offset){ /* Checking if we reached the end */
        pBD->eob = 1;                          /* Set the end of buffer flag */
        return R_FAIL_1;
    }
    pBD->eob = 0;
    return pBD->ca_head[pBD->getc_offset++]; /* Return next character */
}
/*
*   Purpose: Print out the characters in the buffer
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: b_set_getc_offset(), b_getc(), b_eob(), printf()
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: int: number of characters printed
*   Algorithm: Verify buffer exists and is not empty, move the getc_offset to 0
*              so that it prints from the beginning, print until the end of buffer is reached
*/
int b_print(Buffer * const pBD){
    char c; /* Character in buffer */
    int index = 0; /* Number of characters printed */

    if (!pBD) return R_FAIL_1;
    if (pBD->addc_offset == 0) { /* Check if the buffer is empty */
        printf("The buffer is empty.\n");
        return R_FAIL_1;
    }
    if (b_set_getc_offset(pBD,0) == R_FAIL_1) /* Set the offset to 0 to start from the beginning */
        return R_FAIL_1;

    c = b_getc(pBD);               /* Get a character so that the eob flag is reset */

    while (b_eob(pBD) != 1)             /* Print till the end of buffer is reached */
    {
        printf("%c",c);
        c = b_getc(pBD);
        index++;               /* Get next character */
    }
    printf("\n");

#ifdef DEBUG
    printf("\n");
    for (index = 0; index < b_getsize(pBD); index++) {
        printf("|%c|", pBD->ca_head[index]);
    }
    printf("\n");
#endif
    return index;                   /* Return number of characters printed */
}
/*
*   Purpose: Shrinks the buffer to a new capacity or expands it in some cases
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: Returns Buffer* which points to Buffer Structure
*   Algorithm: Check valid parameters, reallocate memory, change necessary values
*/
Buffer *b_pack(Buffer * const pBD){
    if (!pBD) return NULL;
    if ((pBD->addc_offset + 1) >= SHRT_MAX) return pBD; /* Checks to see if it can be expanded */

    if ((pBD->addc_offset + 1) != pBD->capacity) {
        char* ptrTemp = (char *) realloc(pBD->ca_head,((unsigned int)(pBD->addc_offset + 1) * (sizeof(char)))); /* Add space for EOF */
        if (ptrTemp == NULL) return NULL;

    if (ptrTemp != pBD->ca_head) {
        pBD->r_flag = SET_R_FLAG;  /* Set relocation flag because the memory has shifted */
        pBD->ca_head = ptrTemp;  /* Keep existing buffer contents intact */
    }
        ptrTemp = NULL;     /* Avoid dangling pointer  */
        pBD->capacity = pBD->addc_offset + 1; /* New capacity of buffer */
    }
    return pBD;
}
/*
*   Purpose: Get the relocation flag
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: char: 1 if memory location changed due to shrinking or expanding, 0 otherwise
*/
char b_get_r_flag(Buffer * const pBD){
    if (!pBD) return R_FAIL_1;
    return pBD->r_flag;
}
/*
*   Purpose: Set the getc_offset to the last character
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: int: 0 if successful, 1 if unsuccessful
*/
int b_retract(Buffer * const pBD){
    if (!pBD || pBD->getc_offset == 0) return R_FAIL_1; /* Check if it can retract */
    --pBD->getc_offset;
    return SUCCESS;
}
/*
*   Purpose: Get getc_offset location
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*   Return value: short: return the getc_offset location, 1 if unsuccessful
*/
short b_get_getc_offset(Buffer * const pBD){
    if (!pBD) return R_FAIL_1;
    return pBD->getc_offset; /* return next character offset */
}
/*
*   Purpose: Set getc_offset location
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*               offset:type short, new offset location
*   Return value: int: 0 if successful, 1 if unsuccessful
*/
int b_set_getc_offset(Buffer * const pBD, short offset){
    if (!pBD || offset < 0 || offset > pBD->addc_offset) return R_FAIL_1;
    pBD->getc_offset = offset; /* set next character offset */
    return SUCCESS;
}
/*
*   Purpose: Return a char * to the location of a character in the character buffer
*   Author: Tyler Despatie
*   History/Versions: 1.0
*   Called functions: None
*   Parameters: pBD:type Buffer * const, pointer to Buffer structure
*               offset:type short, specified location in the character buffer to return
*   Return value: char *: pointer to the location of the character int the buffer
*/
char * b_get_chmemloc(Buffer * const pBD, short offset){
    if (!pBD || offset > pBD->addc_offset || offset < 0) return NULL;
    return &pBD->ca_head[offset]; /* return a pointer to the location of a character in the character buffer */
}







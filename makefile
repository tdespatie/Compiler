COMPILER = gcc
CCFLAGS = -Wall -ansi -pedantic -Wsign-conversion 
all:
	${COMPILER} ${CCFLAGS} parser.h buffer.c platy.c scanner.c stable.c parser.c -o parser  

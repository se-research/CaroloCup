/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuation.
///////////////////////////////////////////////////////////////////////////////

#define STACK_SIZE 50

int stack[STACK_SIZE];
int *stackPointer = stack-1;
int *stackPointerMax = &stack[STACK_SIZE-1];

#define full() (stackPointer == stackPointerMax)
#define empty() (stackPointer == (stack-1))

///////////////////////////////////////////////////////////////////////////////
// Interface methods.
///////////////////////////////////////////////////////////////////////////////

void push(int value) {
    if(!full()) *(++stackPointer) = value;
}

int pop(void) {
    if(!empty()) return *(stackPointer--);
    return 0;
}

int apply(char operator, int first, int second) {
    switch (operator) {
        case '+': return first + second;
        case '-': return first - second;
        case '*': return first * second;
        case '/': return first / second;
    }
    return 0;
}

char *nextToken(char *linep) {
    // This method moves the line pointer to the next non white space character.
    while (isspace((int)*(linep++)));
    while (*linep && !isspace((int)*(linep++)));
    return linep;
}

int RPN_calculator(char *input) {
    int result = 0;

    if (input != NULL) {
        // Reverse Polish Notation calculator.

        int value;
        char operator;
        char *linep = input;
        for (;;) {
            if(*linep == 0) break;

            if (rsscanf(linep, "%d", &value)) {
                push(value);
            }
            else if (rsscanf(linep, "%c", &operator)) {
                int second = pop();
                int first = pop();
                push(apply(operator, first, second));
            }

            linep = nextToken(linep);
        }
        if(!empty()) {
            result = *stackPointer;
        }

        // Reset stack for next calculation.
        stackPointer = stack-1;
    }

    return result;
}

void commandRPNCalculator(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    if (argc == 3) {
        char buffer[200];

        chsprintf(buffer, "%s %s %s\0", argv[0], argv[1], argv[2]);
        
        int result = RPN_calculator(buffer);

        chprintf(chp, "RPN result = %d", result);
        chprintf(chp, "\r\n");
    }
}


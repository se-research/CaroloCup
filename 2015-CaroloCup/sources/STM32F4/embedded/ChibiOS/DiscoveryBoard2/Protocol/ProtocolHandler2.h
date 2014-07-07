/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PROTOCOL_HANDLER2_H
#define PROTOCOL_HANDLER2_H

typedef struct DataT DataT;

struct DataT {
    char payload[1024];
    int length;
};

/**
 * This method runs a generic protocol between STM32F4 Discovery Board
 * and PandaBoard ES.
 *
 * This is the default behavior if the user button is not pressed.
 */
void runProtocol2(void);

#endif // PROTOCOL_HANDLER2_H

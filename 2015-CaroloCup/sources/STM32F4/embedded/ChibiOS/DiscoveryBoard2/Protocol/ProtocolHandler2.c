/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuation.
///////////////////////////////////////////////////////////////////////////////

// Configuration of the serial protocol between STM32F4 Discovery Board and PandaBoard ES.
const SerialConfig configurationSerialUSB = {
    115000,
    0,
    USART_CR2_STOP1_BITS | USART_CR2_LINEN,
    USART_CR3_CTSE
};

DataT dataFromHost;
DataT dataToHost_AfterConsumption;
DataT dataToHost_WithoutConsumption;

#define BUFFER_LENGTH 1024

// Internal buffers for sending and receiving data.
char receiveBuffer[BUFFER_LENGTH];
char sendBuffer[BUFFER_LENGTH];
int writePtr = 0;

///////////////////////////////////////////////////////////////////////////////
// Interface methods.
///////////////////////////////////////////////////////////////////////////////

void processPayload(void) {
    // Call user-defined data handling.

    // 1. Invoke the user-defined protocol-method to process the received data.
    consumeDataFromHost(&dataFromHost);

    // 2. Subsequently invoke the user-defined protocol-method to eventually process new data to be sent to the host.
    produceDataForHost_AfterConsumption(&dataToHost_AfterConsumption, BUFFER_LENGTH);
}

void decodeNextNetstring(void) {
    // Netstrings have the following format:
    // ASCII Number representing the length of the payload + ':' + payload + ','
    
    // Start decoding only if we have received enough data.
    if (writePtr > 3) {
        char *colonSign = NULL;
        unsigned int lengthOfPayload = strtol(receiveBuffer, &colonSign, 10);
        if (*colonSign == 0x3a) {
            // Found colon sign.

            // First, check if the buffer is as long as it is stated in the netstring.
            if (writePtr < (int)lengthOfPayload) {
                // Received data is too short. Skip further processing this part.
                return;
            }

            // Now, check if (receiveBuffer + 1 + lengthOfPayload) == ','.
            if ((colonSign[1 + lengthOfPayload]) == 0x2c) {
                // Successfully found a complete Netstring.
                memcpy(dataFromHost.payload, colonSign + 1, lengthOfPayload);
                dataFromHost.length = lengthOfPayload;

                // Determine the size of Netstring.
                int lengthOfNetstring = (colonSign + 1 + lengthOfPayload + 1) - receiveBuffer;
                // Remove decoded Netstring from receiveBuffer.
                memmove(receiveBuffer, colonSign + 1 + lengthOfPayload + 1, (BUFFER_LENGTH - lengthOfNetstring));

                // Move the writer pointer to the right position after consuming the read bytes.
                writePtr -= lengthOfNetstring;

                // Process successfully decoded payload.
                processPayload();
            }
        }
    }    
}

void consumeNetstrings(void) {
    int oldWritePtr = 0; // Store the old position of the writePtr to make sure to not end up in an infinite loop.
    while ((writePtr > 3) && (oldWritePtr != writePtr)) {
        oldWritePtr = writePtr;
        decodeNextNetstring();
    }    
}

void runProtocol2(void) {
    // Clear buffers.
    int i = 0;
    for (i = 0; i < BUFFER_LENGTH; i++) {
        receiveBuffer[i] = 0;
        sendBuffer[i] = 0;

        dataFromHost.payload[i] = 0;
        dataFromHost.length = 0;

        dataToHost_AfterConsumption.payload[i] = 0;
        dataToHost_AfterConsumption.length = 0;

        dataToHost_WithoutConsumption.payload[i] = 0;
        dataToHost_WithoutConsumption.length = 0;
    }

    systime_t timeOut = MS2ST(10); // Receiving and sending timeout is 10ms.
    size_t bytesRead = 0;
    size_t bytesToWrite = 0;
    size_t bytesWritten = 0;
    char chunkBuffer[10];

    // Start the serial over USB driver in order to exchange data between STM32F4 Discovery Board and PandaBoard ES.
    sdStart((struct SerialDriver*)&SDU1, &configurationSerialUSB);

    while (TRUE) {
        bytesRead = sdReadTimeout(&SDU1, (uint8_t*)chunkBuffer, 10, timeOut);

        if (bytesRead > 0) {
            // Add received bytes to buffer to parse data from.
            memcpy(receiveBuffer+writePtr, chunkBuffer, bytesRead);
            writePtr += bytesRead;

            // Try to decode netstrings from receiveBuffer.
            consumeNetstrings();
        }

        // Check if a payload needs to be sent _AFTER_ data consumption from host.
        if (dataToHost_AfterConsumption.length > 0) {
            bytesToWrite = log(dataToHost_AfterConsumption.length)/log(10) + 1; // Length of the actual number describing the length of the payload.
            chsprintf(sendBuffer, "%d:%s", dataToHost_AfterConsumption.length, dataToHost_AfterConsumption.payload);
            bytesToWrite += 1 /*for ':'*/ + dataToHost_AfterConsumption.length;
            sendBuffer[bytesToWrite] = ',';
            bytesToWrite += 1 /*for ','*/;

            // Send reply if reply is ready.
            int totalBytesToWrite = bytesToWrite;
            while (bytesToWrite > 0) {
                bytesWritten = sdWriteTimeout(&SDU1, (uint8_t*)(sendBuffer+(totalBytesToWrite - bytesToWrite)), bytesToWrite, timeOut);
                bytesToWrite -= bytesWritten;
            }

            // Reset sent payload.
            dataToHost_AfterConsumption.length = 0;
        }

        // Check, if data needs to be sent to the host without having consumed any data earlier.
        produceDataForHost_WithoutConsumption(&dataToHost_WithoutConsumption, BUFFER_LENGTH);

        if (dataToHost_WithoutConsumption.length > 0) {
            bytesToWrite = log(dataToHost_WithoutConsumption.length)/log(10) + 1; // Length of the actual number describing the length of the payload.
            chsprintf(sendBuffer, "%d:%s", dataToHost_WithoutConsumption.length, dataToHost_WithoutConsumption.payload);
            bytesToWrite += 1 /*for ':'*/ + dataToHost_WithoutConsumption.length;
            sendBuffer[bytesToWrite] = ',';
            bytesToWrite += 1 /*for ','*/;

            // Send reply if reply is ready.
            int totalBytesToWrite = bytesToWrite;
            while (bytesToWrite > 0) {
                bytesWritten = sdWriteTimeout(&SDU1, (uint8_t*)(sendBuffer+(totalBytesToWrite - bytesToWrite)), bytesToWrite, timeOut);
                bytesToWrite -= bytesWritten;
            }

            // Reset sent payload.
            dataToHost_WithoutConsumption.length = 0;
        }

        // Allow for thread scheduling.
        chThdSleepMilliseconds(10);
    }
}


/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

#include "Example/RPNCalculator.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuation.
///////////////////////////////////////////////////////////////////////////////

DataT myData;
static int8_t dataFeed = 0;

///////////////////////////////////////////////////////////////////////////////
// Interface methods.
///////////////////////////////////////////////////////////////////////////////

void initializeUserProtocol(void) {
    int i = 0;
    int bufferLength = 1024;
    for (i = 0; i < bufferLength; i++) {
        myData.payload[i] = 0;
        myData.length = 0;
    }
}

void consumeDataFromHost(DataT *ptrToDataFromHost) {
    if (ptrToDataFromHost != NULL) {
        // Check if we should simply run the RPN calculator as a demonstration of if we interface we the connected peripherals.
        //
        // Commands are encoded like this:
        //
        // Length : Command (1 character) : Value ,
        //
        // Otherwise: RPN Calculator
        if (ptrToDataFromHost->length > 2) {
            if (ptrToDataFromHost->payload[1] == ':') {
                if (ptrToDataFromHost->payload[0] == '1') {
                    // Change data feed with value after ':'
                    int value = 0;
                    if (rsscanf(ptrToDataFromHost->payload + 2, "%d", &value)) {
                        if (value >= 0 && value <= 64) {
                            dataFeed = value;
                        }
                    }
                }
            }
            else {
                // Example: Processing received input via RPN Calculator.
                int result = RPN_calculator(ptrToDataFromHost->payload);

                chsprintf(myData.payload, "%d", result);
                myData.length = log(result)/log(10) + 1; // Count number of characters of the result.
            }
        }
    }
}

void produceDataForHost_AfterConsumption(DataT *ptrToDataForHost, int maxBufferLength) {
    int i = 0;
    if (ptrToDataForHost != NULL && maxBufferLength > 0) {
        // Copy data to send buffer.
        if (myData.length < maxBufferLength) {
            for(i = 0; i < myData.length; i++) {
                ptrToDataForHost->payload[i] = myData.payload[i];
            }

            ptrToDataForHost->length = myData.length;
        }
    }
}

void prepareDataFeed(void) {
    char buffer[50];
    int i = 0;
    myData.length = 0;

    if (dataFeed & ONBOARD_ACCELEROMETER_FEED) {
        OnboardAccelerometerDataT accel;
        accel.x = accel.y = accel.z = 0;

        getOnboardAccelerometerData(&accel);

        chsprintf(buffer, "[OA(%d;%d;%d)]$", accel.x, accel.y, accel.z);

        for(i = 0; i < 50; i++) {
            if (buffer[i] != '$')
                myData.payload[myData.length + i] = buffer[i];
            else
                break;
        }
        myData.length += i;
    }

    if (dataFeed & ONBOARD_TEMPERATURE_FEED) {
        TemperatureDataT temp;
        temp.T = 0;

        getTemperatureData(&temp);
        
        chsprintf(buffer, "[T(%d)]$", temp.T);

        for(i = 0; i < 50; i++) {
            if (buffer[i] != '$')
                myData.payload[myData.length + i] = buffer[i];
            else
                break;
        }
        myData.length += i;
    }

    if (dataFeed & INFRARED_FEED) {
        InfraredDataT IR0, IR1, IR2;
        IR0.address = 0;
        IR0.distance = 0;
        IR0.next = &IR1;

        IR1.address = 0;
        IR1.distance = 0;
        IR1.next = &IR2;

        IR2.address = 0;
        IR2.distance = 0;
        IR2.next = NULL;

        getInfraredData(&IR0);
        
        chsprintf(buffer, "[IR((%d;%d);(%d;%d);(%d;%d))]$", IR0.address, IR0.distance,
                                                            IR1.address, IR1.distance,
                                                            IR2.address, IR2.distance);
        for(i = 0; i < 50; i++) {
            if (buffer[i] != '$')
                myData.payload[myData.length + i] = buffer[i];
            else
                break;
        }
        myData.length += i;
    }

    if (dataFeed & ULTRASONIC_FEED) {
        UltrasonicDataT US0, US1, US2;
        US0.address = 0;
        US0.distance = 0;
        US0.next = &US1;

        US1.address = 0;
        US1.distance = 0;
        US1.next = &US2;

        US2.address = 0;
        US2.distance = 0;
        US2.next = NULL;

        getUltrasonicData(&US0);
        
        chsprintf(buffer, "[US((%d;%d);(%d;%d);(%d;%d))]$", US0.address, US0.distance,
                                                            US1.address, US1.distance,
                                                            US2.address, US2.distance);
        for(i = 0; i < 50; i++) {
            if (buffer[i] != '$')
                myData.payload[myData.length + i] = buffer[i];
            else
                break;
        }
        myData.length += i;
    }

    if (dataFeed & WHEELENCODER_FEED) {
        WheelEncoderDataT WC;
        WC.drivenDistanceLeftWheel = 0;
        WC.drivenDistanceRightWheel = 0;
        WC.speedLeftWheel = 0;
        WC.speedRightWheel = 0;

        getWheelEncoderData(&WC);
        
        chsprintf(buffer, "[WC(%d;%d;%d;%d)]$", WC.drivenDistanceLeftWheel,
                                                WC.drivenDistanceRightWheel,
                                                WC.speedLeftWheel,
                                                WC.speedRightWheel);

        for(i = 0; i < 50; i++) {
            if (buffer[i] != '$')
                myData.payload[myData.length + i] = buffer[i];
            else
                break;
        }
        myData.length += i;
    }
}

void produceDataForHost_WithoutConsumption(DataT *ptrToDataForHost, int maxBufferLength) {
    if (ptrToDataForHost != NULL && maxBufferLength > 0) {
        myData.length = 0;

        prepareDataFeed();

        myData.payload[myData.length] = '\0';

        // Copy data to send buffer.
        if (myData.length < maxBufferLength) {
            int i = 0;
            for(i = 0; i < myData.length; i++) {
                ptrToDataForHost->payload[i] = myData.payload[i];
            }

            ptrToDataForHost->length = myData.length;
        }
    }
}


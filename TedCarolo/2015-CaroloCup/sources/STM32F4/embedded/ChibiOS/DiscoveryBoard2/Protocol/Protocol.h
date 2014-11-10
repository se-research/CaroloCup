/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PROTOCOL_H
#define PROTOCOL_H

/**
 * This method is called to initialize this user-defined protocol.
 */
void initializeUserProtocol(void);

/**
 * This method is invoked whenever the STM32F4 Discovery Board
 * has successfully received data from the host (e.g. the
 * PandaBoard ES).
 *
 * @param ptrToDataFromHost Pointer to a DataT structure, which
 *        contains the received data.
 */
void consumeDataFromHost(DataT *ptrToDataFromHost);

/**
 * This method is always invoked _AFTER_ the consumeDataFromHost
 * method has been called to send data from the STM32F4 Discovery
 * Board to the host (e.g. the PandaBoard ES).
 *
 * The attribute DataT.length is set to 0 afterwards.
 *
 * @param ptrToDataForHost Pointer to a DataT structure, which
 *        needs to be filled with data for the host.
 * @maxBufferLength Maximum size of the send buffer (MUST BE OBEYED!)
 */
void produceDataForHost_AfterConsumption(DataT *ptrToDataForHost, int maxBufferLength);

/**
 * This method is called regularly independently from invoked
 * the consumeDataFromHost method. Thus, this method can be used
 * to send data regularly to the host.
 *
 * Please note that using this method is not mandatory but
 * allows another possibility to broadcast data without having
 * to peek the STM32F4 Discovery Board first.
 *
 * The attribute DataT.length is set to 0 afterwards.
 *
 * @param ptrToDataForHost Pointer to a DataT structure, which
 *        needs to be filled with data for the host.
 * @maxBufferLength Maximum size of the send buffer (MUST BE OBEYED!)
 */
void produceDataForHost_WithoutConsumption(DataT *ptrToDataForHost, int maxBufferLength);

#endif // PROTOCOL_H


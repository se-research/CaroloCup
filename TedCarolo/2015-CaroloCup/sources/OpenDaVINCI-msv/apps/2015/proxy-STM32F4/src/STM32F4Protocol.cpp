/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <string>

#include "core/base/Lock.h"
#include "core/data/Constants.h"

#include "STM32F4Protocol.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::data::environment;

    STM32F4Protocol::STM32F4Protocol() :
        AbstractProtocol(),
        m_partialData(),
        m_dataListenerMutex(),
        m_dataListener(NULL),
        m_vehicleData() {}

    STM32F4Protocol::~STM32F4Protocol() {
        setSTM32F4DataListener(NULL);
    }

    void STM32F4Protocol::setSTM32F4DataListener(STM32F4DataListener *listener) {
        Lock l(m_dataListenerMutex);
        m_dataListener = listener;
    }

    void STM32F4Protocol::request(const STM32F4Request &r, const double &speed, const double &steeringWheelAngle) {
        cout << "(STM32F4Protocol): request: " << r << ", speed: " << speed << ", steeringWheelAngle: " << steeringWheelAngle << endl;

        // TODO: Define the data encoding part of your protocol here.
        string myDataToSend = "This is my data to be sent.";

        // The template implementation on the STM32F4 Discovery Board expects data encoded as Netstring:
        string myDataToSend_asNetString = encodeNetstring(myDataToSend);

        // Send data to STM32F4 Discovery Board.
    	sendByStringSender(myDataToSend_asNetString);
    }

    void STM32F4Protocol::handleReceivedData(const string &receivedData) {
        // TODO: Define the data decoding part of your protocol here.

        cout << "Received data: '" << receivedData << "'" << endl;

        // TODO: Get data from the received string and invoke the registered listener, e.g.
        {
            Lock l(m_dataListenerMutex);
            if (m_dataListener != NULL) {
                // TODO: For example, let's assume you have received VehicleData:
                core::data::environment::VehicleData vehicleData;

                // TODO: Then, you invoke the listener for measurements for Vehicle Data:
                m_dataListener->nextMeasurement(vehicleData);
            }
        }
    }

    void STM32F4Protocol::receivedPartialString(const string &s) {
        m_partialData.write(s.c_str(), s.length());

        cout << "Received: '" << s << "', buffer: '" << m_partialData.str() << "'" << endl;

        decodeNetstring();

        // Put the write pointer to the end of the stream.
        m_partialData.seekp(0, ios_base::end);
    }

    void STM32F4Protocol::decodeNetstring(void) {
        // Netstrings have the following format:
        // ASCII Number representing the length of the payload + ':' + payload + ','

        m_partialData.seekg(0, ios_base::beg);

        // Start decoding only if we have received enough data.
        while(m_partialData.str().length() > 3) {
            unsigned int lengthOld = m_partialData.str().length();
            const string &s = m_partialData.str();
            const char *receiveBuffer = s.c_str();
            char *colonSign = NULL;
            unsigned int lengthOfPayload = strtol(receiveBuffer, &colonSign, 10);
            if (lengthOfPayload == 0) {
                // Remove empty Netstring: 0:, (size is 3).

                m_partialData.str(m_partialData.str().substr(3));
                continue; // Try to decode next Netstring if any.
            }

            if (*colonSign == 0x3a) {
                // Found colon sign. 

                // First, check if the buffer is as long as it is stated in the netstring.
                if (m_partialData.str().length() < lengthOfPayload) {
                    // Received data is too short. Skip further processing this part.
                    break;
                }

                // Now, check if (receiveBuffer + 1 + lengthOfPayload) == ','.
                if ((colonSign[1 + lengthOfPayload]) == 0x2c) {
                    // Successfully found a complete Netstring.
                    string receivedPayload = m_partialData.str().substr((colonSign + 1) - receiveBuffer, lengthOfPayload);

                    // Handle received payload:
                    handleReceivedData(receivedPayload);

                    // Remove decoded Netstring: "<lengthOfPayload> : <payload> ,"
                    int lengthOfNetstring = (colonSign + 1 + lengthOfPayload + 1) - receiveBuffer;

                    m_partialData.str(m_partialData.str().substr(lengthOfNetstring));
                }
            }

            if (lengthOld == m_partialData.str().length()) {
                // This should not happen, received data might be corrupt.

                // Reset buffer.
                m_partialData.str("");
            }
        }    
    }

    string STM32F4Protocol::encodeNetstring(const string &d) {
        stringstream netstring;
        if (d.length() > 0) {
            netstring << (int)d.length() << ":" << d << ",";
        }
        else {
            netstring << "0:,";
        }
        return netstring.str();
    }

} // msv


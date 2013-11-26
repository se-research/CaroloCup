/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef ARDUINO_MEGA_PROTOCOL_H_
#define ARDUINO_MEGA_PROTOCOL_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <sstream>

#include "core/base/Mutex.h"
#include "core/wrapper/AbstractProtocol.h"
#include "core/wrapper/StringSender.h"

//Not yet implemented
//#include "ArduinoMegaDataListener.h"

namespace carolocup {

    using namespace std;

    class ArduinoMegaProtocol : public core::wrapper::AbstractProtocol {
        public:
            enum AruinoMegaRequest {
                Velocity = 0,
                SteeringANgle = 1,
            };

        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            ArduinoMegaProtocol(const ArduinoMegaProtocol &);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            ArduinoMegaProtocol& operator=(const ArduinoMegaProtocol &);

        public:
            /**
             * Constructor.
             */
            ArduinoMegaProtocol();

            virtual ~ArduinoMegaProtocol();

            /**
             * This method requests data from STM32F4 board.
             *
             * @param r Requested data from the STM32F4 board.
             * @param speed Desired speed value: will be mapped to ?.
             * @param steeringWheelAngle Desired steering wheel angle: will be mapped to ?.
             */
            void request(const ArduinoMegaRequest &r, const double &speed, const double &steeringWheelAngle);

            /**
             * This method sets the STM32F4DataListener.
             *
             * @param listener STM32F4DataListener to distribute the data.
             */
            void setArduinoMegaDataListener(ArduinoMegaDataListener *listener);

            virtual void receivedPartialString(const string &partialData);

        private:
            /**
             * This method handles the actual data for this protocol.
             *
             * @param packet.
             */
            void handlePacket(const uint32_t &packet);

            /**
             * This method maps the transferred multiplier according to the protocol specification:
             *
             *   00 = 1
             *   01 = 3
             *   10 = 5
             *   11 = 10
             *
             * @param m.
             * @return mapped multiplier.
             */
            uint32_t getMultiplier(const uint32_t &m);

            /**
             * This method returns the numeric value from the packet.
             *
             * @param packet The packet from which the data should be extracted.
             * @param startBit (0-31) bit from which the data should be extracted.
             * @param length must be less than or equal to (32-startBit) length of data to be extracted.
             * @return Value from the packet.
             */
            uint32_t getValueFromPacket(const uint32_t &packet, const uint32_t &startBit, const uint32_t &length) const;

            stringstream m_partialData;

            core::base::Mutex m_dataListenerMutex;
            ArduinoMegaDataListener *m_dataListener;

            core::data::environment::VehicleData m_vehicleData;
    };
}

#endif /* ARDUINO_MEGA_PROTOCOL_H_ */


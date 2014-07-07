/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef STM32F4PROTOCOL_H_
#define STM32F4PROTOCOL_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <sstream>

#include "core/base/Mutex.h"
#include "core/wrapper/AbstractProtocol.h"
#include "core/wrapper/StringSender.h"

#include "STM32F4DataListener.h"

namespace msv {

    using namespace std;

    class STM32F4Protocol : public core::wrapper::AbstractProtocol {
        public:
            enum STM32F4Request {
                NoData = 0,
            };

        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            STM32F4Protocol(const STM32F4Protocol &);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            STM32F4Protocol& operator=(const STM32F4Protocol &);

        public:
            /**
             * Constructor.
             */
            STM32F4Protocol();

            virtual ~STM32F4Protocol();

            /**
             * This method requests data from STM32F4 board.
             *
             * @param r Requested data from the STM32F4 board.
             * @param speed Desired speed value: will be mapped to ?.
             * @param steeringWheelAngle Desired steering wheel angle: will be mapped to ?.
             */
            void request(const STM32F4Request &r, const double &speed, const double &steeringWheelAngle);

            /**
             * This method sets the STM32F4DataListener.
             *
             * @param listener STM32F4DataListener to distribute the data.
             */
            void setSTM32F4DataListener(STM32F4DataListener *listener);

            virtual void receivedPartialString(const string &partialData);

        private:
            /**
             * This method is called whenever a Netstring could be
             * successfully decoded from STM32F4 Discovery Board.
             *
             * @param receivedData Data received and decoded from STM32F4 Discovery Board.
             */
            void handleReceivedData(const string &receivedData);

        private:
            void decodeNetstring();

            string encodeNetstring(const string &d);

            stringstream m_partialData;

            core::base::Mutex m_dataListenerMutex;
            STM32F4DataListener *m_dataListener;

            core::data::environment::VehicleData m_vehicleData;
    };
}

#endif /* STM32F4PROTOCOL_H_ */


/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PROXY_H_
#define PROXY_H_

#include <map>

#include "core/base/ConferenceClientModule.h"
#include "core/base/Mutex.h"
#include "core/data/environment/VehicleData.h"
#include "core/wrapper/StringListener.h"

#include "PointSensor.h"
#include "SensorBoardData.h"
#include "UserButtonData.h"

namespace msv {

    using namespace std;

    /**
     * This class is a skeleton to translate OpenDaVINCI Containers to UDP_Server commands.
     */
    class Proxy : public core::base::ConferenceClientModule, public core::wrapper::StringListener {
        public:
            // Packets that only expect ack
            enum CAR_NORES_PACKET_ID {
                CAR_PACKET_SET_POWER_SERVO = 0,
                CAR_PACKET_WRITE_POS,
                CAR_PACKET_ADD_POINT,
                CAR_PACKET_AP_RUN,
                CAR_PACKET_AP_CLEAR,
                CAR_PACKET_RESET_TRAVEL_CNT,
                CAR_PACKET_SET_LIMITED,
                CAR_PACKET_FULL_BRAKE,
                CAR_PACKET_SERVO_OFFSET
            };

            // Packets that expect response
            enum CAR_RES_PACKET_ID {
                CAR_PACKET_READ_VALUES = 0,
                CAR_PACKET_READ_POS,
                CAR_PACKET_READ_SENS_ULTRA,
                CAR_PACKET_READ_SENS_IR,
                CAR_PACKET_READ_TRAVEL_COUNTER,
                CAR_PACKET_PING,
                CAR_PACKET_ACK2 = 254
            };

            enum CAR_SPECIAL_CMD {
                CAR_PACKET_RES = 1,
                CAR_PACKET_NORES = 2,
                CAR_PACKET_ACK = 254
            };

        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Proxy(const Proxy &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Proxy& operator=(const Proxy &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Proxy(const int32_t &argc, char **argv);

            virtual ~Proxy();

            core::base::ModuleState::MODULE_EXITCODE body();

            virtual void nextString(const string &s);

        private:
            virtual void setUp();

            virtual void tearDown();

            /**
             * This method creates the payload part of the packet to be sent to UDP_Server.
             *
             * @param speed Speed information.
             * @param dir Steering information.
             * @return payload part of the packet to be sent to UDP_Server.
             */
            string createPayloadForSettingAccelerationAndSteering(const double &speed, const uint8_t &direction) const;

            string createPayloadForQueryOnboardData() const;

            string createPayloadForQueryUltraSonicData() const;

            string createPayloadForQueryInfraRedData() const;

            string createPayloadForQueryPositionData() const;

            /**
             * This method resets the current position data (just "set" the given position so the car thinks it is at the given position).
             *
             * @param x absolute position x in mm. 
             * @param y absolute position y in mm.
             * @param heading absolute heading.
             */ 
            string createPayloadForWritePos(const double &x, const double &y, const double &heading) const;

            string createPayloadForAddPosition(const double &x, const double &y, const double &speed) const;

            string createPayloadForReadTravelCounter(const bool &isAbs) const;

            string createPayloadForRunAP(const bool &run) const;

            string createPayloadForClearAP() const;

            string createPayloadForResetTravelCounter() const;

            string createPayloadForFullBrake() const;
 
            string createPayloadForServoOffset(const uint32_t &offset) const;

            /**
             * This method creates the outer frame (CRC16 checksum).
             *
             * @param s Payload to be sent to the server.
             * @return packet to be sent to UDP_Server.
             */
            string createPacket(const string &s) const;

            unsigned short crc16(const unsigned char *buf, unsigned int len) const;

        private:
            map<uint16_t, PointSensor*> m_mapOfPointSensors;
            
            core::base::Mutex m_userButtonMutex;
            UserButtonData m_userButtonData;
            
            core::base::Mutex m_sensorBoardMutex;
            SensorBoardData m_sensorBoardData;

            core::base::Mutex m_vehicleDataMutex;
            core::data::environment::VehicleData m_vehicleData;

            uint32_t m_steeringOffset;
            bool m_debug;
    };

} // msv

#endif /*PROXY_H_*/

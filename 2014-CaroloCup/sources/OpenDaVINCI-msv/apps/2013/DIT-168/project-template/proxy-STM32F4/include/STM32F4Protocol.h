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

/*
    Communication Bytes:

    Before any data is transferred from the Discovery Board to the Panda Board, 
    a communication in the form of a (Hand-Shake) must occur between the Discovery 
    and Panda boards.

    The communication bytes, are two bytes that contains inside of them the values 
    that represent what kind of data needs to be sent from the Discovery board.

    The communication bytes also contains the data which will be sent to the board, 
    which are the setters for the (Steering Data and the Speed of the car).

    The bytes are divided as follow:

    Byte 1: Bit 0-3 = Indicate what data is requested from the Discovery Board
    Byte 1: Bit 4-7 = Speed (part 1)
    Byte 2: Bit 0-1 = Speed (part 2)
    Byte 2: Bit 2-7 = Steering

    Using four bits to request the data in case we want to add alternative requests 
    later.

    0   : No Data			                    : 0000 0000
    1   : All Infrareds		                    : 0000 0001
    2   : All Ultra Sonics		                : 0000 0010
    3   : Yaw Data                              : 0000 0011
    4   : Magnetometer Data All                 : 0000 0100
    5   : Gyroscope Data All                    : 0000 0101
    6   : accelerometer (Razor) All             : 0000 0110
    7   : accelerometer (Discovery) All         : 0000 0111
    8   : Razor Data All                        : 0000 1000
    9   : Razor Data + Discovery All            : 0000 1001
    10  : Current Position                      : 0000 1010
    11  : relTravelledPath + absTravelledPath   : 0000 1011
    12  : Velocity                              : 0000 1100
    13  : Orientation                           : 0000 1101
    14-15: Reserved	 		                    : 0000 XXXX


    ################################################################################
    SENSORS - Values are in centimeters

    --------------------------------------------------------------------------------
    INFRARED

    Byte 1: Bit 0-4 = IR1
            Bit 5-7 = IR2 (part 1)
    Byte 2: Bit 0-1 = IR2 (part 2)
            Bit 2-6 = IR3

    --------------------------------------------------------------------------------
    ULTRASONIC

    One byte per UltraSonic
    Bit 0-5 = Data.
    Bit 6-7 = Multiplier.

    00 = 1
    01 = 3
    10 = 5
    11 = 10


    ################################################################################
    ################################################################################
    ################################################################################

    Update:
    The STM32F4 board generally sends 4 bytes as an answer to all requests.
    --------------------------------------------------------------------------------
    INFRARED

    4 bits as type specification
    5 bits per infrared sensor
    maximum of 5 infrared sensors possible

    Byte 1: Bit 0-3 = 0001 (answer to the requested data of type 1 see above)
            Bit 4-7 = IR1 (part 1)
    Byte 2: Bit 0   = IR1 (part 2)
            Bit 1-5 = IR2
	        Bit 6-7 = IR3 (part 1)
    Byte 3: Bit 0-2 = IR3 (part 2)
            Bit 3-7 = ?   (unused)
    Byte 4: Bit 0-5 = ?   (unused)
            Bit 6-7 = 01  Packet terminator

    --------------------------------------------------------------------------------

    --------------------------------------------------------------------------------
    ULTRASONIC

    4 bits as type specification
    6 bits per ultrasonic sensor + 2 bits for multiplier
    maximum of 3 ultrasonic sensors possible

    Byte 1: Bit 0-3 = 0010 (answer to the requested data of type 2 see above)
            Bit 4-7 = US1 - data (part 1)
    Byte 2: Bit 0-1 = US1 - data (part 2)
            Bit 2-3 = US1 - multiplier
	        Bit 4-7 = US2 - data (part 1)
    Byte 3: Bit 0-1 = US2 - data (part 2)
            Bit 2-3 = US2 - multiplier
	        Bit 4-7 = US3 - data (part 1)
    Byte 4: Bit 0-1 = US3 - data (part 2)
            Bit 2-3 = US3 - multiplier
	        Bit 4-5 = ? (unused)
            Bit 6-7 = 01  Packet terminator

    One byte per UltraSonic
    Bit 0-5 = Data.
    Bit 6-7 = Multiplier.

    00 = 1
    01 = 3
    10 = 5
    11 = 10

    --------------------------------------------------------------------------------

    RAZOR Board 9DoF IMU

    Yaw, Pitch, Roll:
    -----------------
    Yaw = 9 bits. -180 to 180
    Pitch = Ignored
    Roll = Ignored

    Byte 1: Bit 0-3 = 0011 (answer to the requested data of type 3 see above)
            Bit 4-7 = Yaw - data (part 1)
    Byte 2: Bit 0-3 = Yaw - data (part 2)
    	    Bit 4   = Yaw - negative data (Flag for the Yaw, negative values) the Bit is SET if the data is negative.
            Bit 5-7 = ? (unused)
    Byte 3: Bit 0-5 = ? (unused)
            Bit 6-7 = ? (unused)
    Byte 4: Bit 0-5 = ? (unused)
            Bit 6-7 = 01  Packet terminator

    Compass(Magnetometer):
    ----------------------
    X = 9 bits. -250 to 250
    Y = 9 bits. -250 to 250
    Z = Ignored

    Byte 1: Bit 0-3 = 0100 (answer to the requested data of type 4 see above)
            Bit 4-7 = Magnetometer X - data (part 1)
    Byte 2: Bit 0-3 = Magnetometer X - data (part 2)
            Bit 4   = Magnetometer X - data (Flag for X, negative values) the Bit is SET if the data is negative.
            Bit 5-7 = Magnetometer Y - data (part 1)
    Byte 3: Bit 0-4 = Magnetometer Y - data (part 2)
            Bit 5   = Magnetometer Y - negative data (Flag for Y, negative values) the Bit is SET if the data is negative.
            Bit 6-7 = ? (unused)
    Byte 4: Bit 0-5 = ? (unused)
            Bit 6-7 = 01  Packet terminator

    Gyroscope:
    ----------

    X = 9 bits. -250 to 250
    Y = 9 bits. -250 to 250
    Z = Ignored

    Byte 1: Bit 0-3 = 0100 (answer to the requested data of type 4 see above)
            Bit 4-7 = Gyro X - data (part 1)
    Byte 2: Bit 0-3 = Gyro X - data (part 2)
            Bit 4   = Gyro X - negative data (Flag for X, negative values) the Bit is SET if the data is negative.
            Bit 5-7 = Gyro Y - data (part 1)
    Byte 3: Bit 0-4 = Gyro Y - data (part 2)
            Bit 5   = Gyro Y - negative data (Flag for Y, negative values) the Bit is SET if the data is negative.
            Bit 6-7 = ? (unused)
    Byte 4: Bit 0-5 = ? (unused)
            Bit 6-7 = 01  Packet terminator


    Razor Accelerometer:
    --------------

    X = 9 bits. -250 to 250
    Y = 9 bits. -250 to 250 
    Z = Ignored

    Byte 1: Bit 0-3 = 0100 (answer to the requested data of type 4 see above)
            Bit 4-7 = Razor Accelerometer X - data (part 1)
    Byte 2: Bit 0-3 = Razor Accelerometer X - data (part 2)
            Bit 4   = Razor Accelerometer X - negative data (Flag for X, negative values) the Bit is SET if the data is negative.
            Bit 5-7 = Razor Accelerometer Y - data (part 1)
    Byte 3: Bit 0-4 = Razor Accelerometer Y - data (part 2)
            Bit 5   = Razor Accelerometer Y - negative data (Flag for Y, negative values) the Bit is SET if the data is negative.
            Bit 6-7 = ? (unused)
    Byte 4: Bit 0-5 = ? (unused)
            Bit 6-7 = 01  Packet terminator

    --------------------------------------------------------------------------------
    --------------------------------------------------------------------------------

    Discovery Board Accelerometer:

    X = 9 bits. -250 to 250
    Y = 9 bits. -250 to 250 
    Z = Ignored

    Byte 1: Bit 0-3 = 0100 (answer to the requested data of type 4 see above)
            Bit 4-7 = Discovery Accelerometer X  - data (part 1)
    Byte 2: Bit 0-3 = Discovery Accelerometer  X - data (part 2)
            Bit 4   = Discovery Accelerometer  X - negative data (Flag for X, negative values) the Bit is SET if the data is negative.
            Bit 5-7 = Discovery Accelerometer  Y - data (part 1)
    Byte 3: Bit 0-4 = Discovery Accelerometer  Y - data (part 2)
            Bit 5   = Discovery Accelerometer  Y - negative data (Flag for Y, negative values) the Bit is SET if the data is negative.
            Bit 6-7 = ? (unused)
    Byte 4: Bit 0-5 = ? (unused)
            Bit 6-7 = 01  Packet terminator


    --------------------------------------------------------------------------------
    --------------------------------------------------------------------------------

    Current Position:
    -----------------

    4 bits as type specification

    positionX = 2500 cms = bits 0-12 = value , bit 13 = +- sign. -- 13 Bits 
    positionY = 2500 cms = bits 0-12 = value , bit 13 = +- sign. -- 13 Bits 

    Byte 1: Bit 0-3 = 1010 (answer to the requested data of type 10 see above)
            Bit 4-7 = X Position - (part 1)
    Byte 2: Bit 0-7 = X Position - (part 2)
    Byte 3: Bit 0   = X Position - negative data (Flag for X, negative values) the Bit is SET
            Bit 1-7 = Y Position - (part 1)
    Byte 4: Bit 0-4 = Y Position - (part 2)
            Bit 5   = Y Position - negative data (Flag for Y, negative values) the Bit is SET
            Bit 6-7 = 01  Packet terminator

    --------------------------------------------------------------------------------
    --------------------------------------------------------------------------------

    RelTravelledPath + AbsTravelledPath :
    -------------------------------------

    4 bits as type specification

    relTravelledPath = X/2 = 50 cms= 6 bits
    absTravelledPath = X/2 = 50000 cms = 16 bits 

    Byte 1: Bit 0-3 = 1011 (answer to the requested data of type 11 see above)
            Bit 4-7 = relTravelledPath - (part 1)
    Byte 2: Bit 0-1 = relTravelledPath - (part 2)
            Bit 2-7 = absTravelledPath - (part 1)
    Byte 3: Bit 0-7 = absTravelledPath - (part 2)
    Byte 4: Bit 0-1 = absTravelledPath - (part 3)
            Bit 2-5 = ? (unused)
            Bit 6-7 = 01  Packet terminator

    --------------------------------------------------------------------------------
    --------------------------------------------------------------------------------

    Velocity:
    ---------

    4 bits as type specification

    VelocityX = 511 cms = bits 0-8 = value , bit 9 = +- sign. -- 10 Bits 
    VelocityY = 511 cms = bits 0-8 = value , bit 9 = +- sign. -- 10 Bits  

    Byte 1: Bit 0-3 = 1100 (answer to the requested data of type 12 see above)
            Bit 4-7 = Velocity X data - (part 1)
    Byte 2: Bit 0-4 = Velocity X data - (part 2)
            Bit 5   = Velocity X - negative data (Flag for X, negative values) the Bit is SET
    Byte 2: Bit 6-7 = Velocity Y data - (part 1)
    Byte 3: Bit 0-6 = Velocity Y data - (part 2)
            Bit 7   = Velocity Y - negative data (Flag for Y, negative values) the Bit is SET
    Byte 4: Bit 0-5 = ? (unused)
            Bit 6-7 = 01  Packet terminator

    --------------------------------------------------------------------------------
    --------------------------------------------------------------------------------

    Orientation :
    -------------

    4 bits as type specification

    orientationZ_rad = X*100/2 = 7 bits // When the data is received in the Panda Board, it must be multiplied by (pi) to get the radiant form.


    Byte 1: Bit 0-3 = 1101 (answer to the requested data of type 13 see above)
            Bit 4-7 = Orientation Z - (part 1)
    Byte 2: Bit 0-2 = Orientation Z - (part 2)
            Bit 3-7 = ? (unused)
    Byte 3: Bit 0-7 = ? (unused)
    Byte 4: Bit 0-5 = ? (unused)
            Bit 6-7 = 01  Packet terminator

    --------------------------------------------------------------------------------
    --------------------------------------------------------------------------------


    Important Notes about (Out of Range values for Razor / Discovery Board):

     - If the data is out of range (i.e over 250 or less than -250) for the Accelerometer, Magnetometer and Gyro, the protocol will return -0, which is formed by setting bit (13) to (1) while the rest of the data to (0), in the form of (Terminator00 0000 0001 0000 0000 request).

     - If the data is out of range (i.e over 180 or less than -180) for the Yaw,  the protocol will return -0, which is formed by setting bit (13) to (1) while the rest of the data to (0), in the form of (Terminator00 0000 0001 0000 0000 request).

     - If the data is out of range (i.e over 2500 or less than -2500) for the Current Position (X,Y), the protocol will return -0, which is formed by setting bit (17) to (1) while the rest of the X-data to (0), in the form of (Terminator 0 0000 0000 0000 1 0000 0000 0000 request).

*/

    class STM32F4Protocol : public core::wrapper::AbstractProtocol {
        public:
            enum STM32F4Request {
                NoData = 0,
                AllInfrareds = 1,
                AllUltrasonics = 2,
                YawData = 3,
                Magnetometer = 4,
                Gyroscope = 5,
                AccelerometerRazor = 6,
                AccelerometerSTM32 = 7,
                RazorAll = 8,
                RazorAllSTM32 = 9,
                CurrentPosition = 10,
                TraveledPath = 11,
                Velocity = 12,
                Orientation = 13,
                Reserved14 = 14,
                Reserved15 = 15,
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
            STM32F4DataListener *m_dataListener;

            core::data::environment::VehicleData m_vehicleData;
    };

}

#endif /* STM32F4PROTOCOL_H_ */


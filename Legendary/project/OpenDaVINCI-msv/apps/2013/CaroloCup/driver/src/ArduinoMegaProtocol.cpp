/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <string>

#include "core/base/Lock.h"
#include "core/data/Constants.h"

#include "ArduinoMegaProtocol.h"

namespace carolocup {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::data::environment;

    ArduinoMegaProtocol::ArduinoMegaProtocol() :
        AbstractProtocol(),
        m_partialData(),
        m_dataListenerMutex(),
        //m_dataListener(NULL),
        m_vehicleData() {}

    ArduinoMegaProtocol::~ArduinoMegaProtocol() {
        //setArduinoMegaDataListener(NULL);
    }
	std::string itos(int n)
	{
	   const int max_size = std::numeric_limits<int>::digits10 + 1 /*sign*/ + 1 /*0-terminator*/;
	   char buffer[max_size] = {0};
	   sprintf(buffer, "%d", n);
	   return std::string(buffer);
	}

    /*void ArduinoMegaProtocol::setArduinoMegaDataListener(ArduinoMegaDataListener *listener) {
        Lock l(m_dataListenerMutex);
        m_dataListener = listener;
    }

    void ArduinoMegaProtocol::request(const ArduinoMegaRequest &r, const double &speed, const double &steeringWheelAngle) {
        cout << "(ArduinoMegaProtocol): request: " << r << ", speed: " << speed << ", steeringWheelAngle: " << steeringWheelAngle << endl;

        double tmpSpeed = speed;
        if (speed < -2.0) {
            tmpSpeed = -2.0;
        }
        if (speed > 2.0) {
            tmpSpeed = 2.0;
        }
        const double SPEED_RANGE = 2.0 * 2;
        const double BITS = 63;
        uint16_t mappedSpeed = (uint8_t)((tmpSpeed + 2.0)/(SPEED_RANGE/BITS));
            
        double tmpSteeringWheelAngle = steeringWheelAngle;
        if (steeringWheelAngle < -28.0) {
            tmpSteeringWheelAngle = -28.0;
        }
        if (steeringWheelAngle > 28.0) {
            tmpSteeringWheelAngle = 28.0;
        }
        const double STEERINGWHEELANGLE_RANGE = 28.0 * 2;
        uint16_t mappedSteeringWheelAngle = (uint8_t)((tmpSteeringWheelAngle + 2.0)/(STEERINGWHEELANGLE_RANGE/BITS));

        uint16_t data = 0;
        data = r | (mappedSpeed << 4) | (mappedSteeringWheelAngle << 10);
	
		sendByStringSender(itos(data));
    }*/

    void ArduinoMegaProtocol::receivedPartialString(const string &s) {
        m_partialData.write(s.c_str(), s.length());

        // One packet has the size of 4 bytes.
        const uint32_t PACKET_SIZE = sizeof(uint32_t);

        // Get the size of received data.
        m_partialData.seekg(0, ios_base::end);
        uint32_t streamSize = m_partialData.tellg();
        m_partialData.seekg(0, ios_base::beg);

        // Decode as much as possible from the currently received data (currently assuming that we will not loose any data).
        while (streamSize >= PACKET_SIZE) {
            // Read next packet.
            uint32_t nextPacket = 0;
            m_partialData.read(reinterpret_cast<char*>(&nextPacket), sizeof(uint32_t));
            nextPacket = ntohl(nextPacket);

            // TODO: Ensure that we have a valid packet (request header & packet terminator).

            handlePacket(nextPacket);

            streamSize -= PACKET_SIZE;
        }
        // Put the write pointer to the end of the stream.
        m_partialData.seekp(0, ios_base::end);
    }

    uint32_t ArduinoMegaProtocol::getValueFromPacket(const uint32_t &packet, const uint32_t &startBit, const uint32_t &length) const {
        uint32_t retVal = 0;
        if ( (startBit < 32) && ((startBit + length) < 33) ) {
            retVal = (packet >> startBit) & (uint32_t)(pow(2, length) - 1);
        }
        return retVal;
    }

    void ArduinoMegaProtocol::handlePacket(const uint32_t &packet) {
        //ArduinoMegaRequest r = (ArduinoMegaRequest)getValueFromPacket(packet, 0, 4);
        //uint32_t packetTerminator = getValueFromPacket(packet, 30, 2);

        cout << "(ArduinoMegaProtocol): request: " << (uint32_t)(packet) << endl;

    }

    uint32_t ArduinoMegaProtocol::getMultiplier(const uint32_t &m) {
        uint32_t retVal = 1;

        switch (m) {
            case 0:
                retVal = 1;
            break;
            case 1:
                retVal = 3;
            break;
            case 2:
                retVal = 5;
            break;
            case 3:
                retVal = 10;
            break;
            default:
                retVal = 1;
        }

        return retVal;
    }


} // msv


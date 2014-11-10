/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <cstring>
#include <cmath>

#include <string>

#include "core/macros.h"
#include "core/base/Lock.h"
#include "core/base/LIFOQueue.h"
#include "core/data/Container.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/data/Constants.h"
#include "core/data/TimeStamp.h"
#include "core/data/control/VehicleControl.h"
#include "core/io/ContainerConference.h"
#include "core/io/StreamFactory.h"
#include "core/io/URL.h"
#include "core/exceptions/Exceptions.h"
#include "core/wrapper/SerialPortFactory.h"
#include "core/wrapper/SerialPort.h"

#include "ProxySTM32F4.h"
#include "STM32F4Protocol.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::data::control;
    using namespace core::exceptions;
    using namespace core::io;

    ProxySTM32F4::ProxySTM32F4(const int32_t &argc, char **argv) :
	    ConferenceClientModule(argc, argv, "proxy-STM32F4"),
        m_mapOfPointSensors(),
        m_userButtonMutex(),
        m_userButtonData(),
        m_sensorBoardMutex(),
        m_sensorBoardData(),
        m_vehicleDataMutex(),
        m_vehicleData(),
        m_debug(false) {
    }

    ProxySTM32F4::~ProxySTM32F4() {
    }

    void ProxySTM32F4::setUp() {
	    // This method will be call automatically _before_ running body().
    }

    void ProxySTM32F4::tearDown() {
	    // This method will be call automatically _after_ return from body().
    }

    void ProxySTM32F4::handleConnectionError() {
        cout << "ProxySTM32F4: TODO: Handle connection error here." << endl;
    }

    void ProxySTM32F4::nextMeasurement(const vector<InfraredSensorMeasurement> &measurement) {
        Lock lock(m_sensorBoardMutex);

        vector<InfraredSensorMeasurement> l = measurement;
        vector<InfraredSensorMeasurement>::iterator it = l.begin();

        while (it != l.end()) {
            InfraredSensorMeasurement m = (*it++);
            if (m_debug) {
                cout << "ProxySTM32F4: " << m.address << ": " << m.value << endl;
            }

            double distance = m.value/100.0; // Convert to m.

            // Address was configured in configuration file.
            const uint16_t SENSOR_ID = m_mapOfPointSensors[m.address]->getID();

            // Check if we need to clamp the distance to -1.
            if (distance > m_mapOfPointSensors[m.address]->getClampDistance()) {
                distance = -1;
            }

            // Update SensorBoardData.
            m_sensorBoardData.update(SENSOR_ID, distance);
            if (m_debug) {
                cerr << "Updated " << SENSOR_ID << " with " << distance << endl;
            }
        }
    }

    void ProxySTM32F4::nextMeasurement(const vector<UltrasonicSensorMeasurement> &measurement) {
        Lock lock(m_sensorBoardMutex);

        vector<UltrasonicSensorMeasurement> l = measurement;
        vector<UltrasonicSensorMeasurement>::iterator it = l.begin();

        while (it != l.end()) {
            UltrasonicSensorMeasurement m = (*it++);
            if (m_debug) {
                cout << "ProxySTM32F4: " << m.address << ": " << m.value << endl;
            }

            double distance = m.value/100.0; // Convert to m.

            // Address was configured in configuration file.
            const uint16_t SENSOR_ID = m_mapOfPointSensors[m.address]->getID();

            // Check if we need to clamp the distance to -1.
            if (distance > m_mapOfPointSensors[m.address]->getClampDistance()) {
                distance = -1;
            }

            // Update SensorBoardData.
            m_sensorBoardData.update(SENSOR_ID, distance);
            if (m_debug) {
                cerr << "Updated " << SENSOR_ID << " with " << distance << endl;
            }
        }
    }

    void ProxySTM32F4::nextMeasurement(const RazorMeasurement &/*measurement*/) {
        if (m_debug) {
            cout << "ProxySTM32F4: Received Razor measurement." << endl;
        }
    }

    void ProxySTM32F4::nextMeasurement(const STM32F4AccelerometerMeasurement &/*measurement*/) {
        if (m_debug) {
            cout << "ProxySTM32F4: Received STM32F4 accelerometer measurement." << endl;
        }
    }

    void ProxySTM32F4::nextMeasurement(const core::data::environment::VehicleData &vd) {
        if (m_debug) {
            cout << "ProxySTM32F4: Received STM32F4 IMU algorithms (VehicleData) measurement." << endl;
        }
        {
            Lock l(m_vehicleDataMutex);
            m_vehicleData = vd;
        }
    }

    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE ProxySTM32F4::body() {
        if (getFrequency() < 20) {
            cerr << endl << endl << "ProxySTM32F4: WARNING! Running proxy-STM32F4 with a LOW frequency (consequence: data updates are too seldom and will influence your algorithms in a negative manner!) --> suggestions: --freq=20 or higher! Current frequency: " << getFrequency() << " Hz." << endl << endl << endl;
        }

        // Get configuration data.
        KeyValueConfiguration kv = getKeyValueConfiguration();
        m_debug = kv.getValue<uint32_t>("proxy-STM32F4.debug") == 1;

        const URL u(kv.getValue<string>("proxy-STM32F4.serial_port"));
        const string SERIAL_PORT = u.getResource();
        const uint32_t SERIAL_SPEED = kv.getValue<uint32_t>("proxy-STM32F4.serial_speed");

        const double WHEELBASE = kv.getValue<double>("proxy-STM32F4.wheelbase");
        const double MAX_STEERING_LEFT_RAD = atan(WHEELBASE/kv.getValue<double>("proxy-STM32F4.minimumTurningRadiusLeft") );
        const double MAX_STEERING_RIGHT_RAD = atan(WHEELBASE/kv.getValue<double>("proxy-STM32F4.minimumTurningRadiusRight"));
        const bool INVERTED_STEERING = (kv.getValue<int32_t>("proxy-STM32F4.invertedSteering") != 0) ? true : false;
        const double SPEED_MAX = kv.getValue<double>("proxy-STM32F4.speed.max");

        const string USER_START_BUTTON_FILE = kv.getValue<string>("proxy-STM32F4.user_start_button_file");
        const string BRAKE_LED_FILE = kv.getValue<string>("proxy-STM32F4.GPIO_brakeLED_file");
        const string TURN_LEFT_LED_FILE = kv.getValue<string>("proxy-STM32F4.GPIO_turnLeftLED_file");
        const string TURN_RIGHT_LED_FILE = kv.getValue<string>("proxy-STM32F4.GPIO_turnRightLED_file");

        // Setup all point sensors.
        for (uint32_t i = 0; i < getKeyValueConfiguration().getValue<uint32_t>("proxy-STM32F4.numberOfSensors"); i++) {
            stringstream sensorID;
            sensorID << "proxy-STM32F4.sensor" << i << ".id";
            uint16_t id(getKeyValueConfiguration().getValue<uint16_t>(sensorID.str()));

            stringstream sensorName;
            sensorName << "proxy-STM32F4.sensor" << i << ".name";
            string name(getKeyValueConfiguration().getValue<string>(sensorName.str()));

            stringstream sensorAddress;
            sensorAddress << "proxy-STM32F4.sensor" << i << ".address";
            uint16_t address(getKeyValueConfiguration().getValue<uint16_t>(sensorAddress.str()));

            stringstream sensorClampDistance;
            sensorClampDistance << "proxy-STM32F4.sensor" << i << ".clampDistance";
            double clampDistance(getKeyValueConfiguration().getValue<double>(sensorClampDistance.str()));

            PointSensor *ps = new PointSensor(id, name, address, clampDistance);

            if (ps != NULL) {
                // Save for later.
                m_mapOfPointSensors[ps->getAddress()] = ps;

                // Initialize m_sensorBoardData data structure for this sensor.
                m_sensorBoardData.update(ps->getID(), -1);

                if (m_debug) {
                    cerr << "Registered point sensor " << ps->getName() << "(" << ps->getID() << ")" << ": " << ps->getAddress() << endl;
                }
            }
        }


        // Create LIFO data store to receive containers.
        core::base::LIFOQueue lifo;
        addDataStoreFor(lifo);

        // The following lines are used to open the GPIO file specified in the configuration file to test if the user pressed the button to start.
        istream *in = NULL;
        const int16_t USER_START_BUTTON_PRESSED_VALUE = 0; // if the user pressed the button the value changes to 0.
        bool userButtonAvailable = false;
        bool userButtonPressed = false;
        bool userButtonReleased = false;
        bool userButtonPressedTS_set = false;
        TimeStamp userButtonPressedTS;
        TimeStamp userButtonReleasedTS;
        double userButtonPressedDuration = 0;

        URL url_start_button_file(USER_START_BUTTON_FILE);
        try {
            in = &(StreamFactory::getInstance().getInputStream(url_start_button_file));
            userButtonAvailable = true;

            m_userButtonData.setButtonStatus(UserButtonData::RELEASED);
        }
        catch(InvalidArgumentException &iae) {
            // The specified user start button file could not be opened. Assuming that there is not such file available and start right ahead.
            userButtonAvailable = false;

            cerr << "(ProxySTM32F4) warning: '" << iae.toString() << "': User button unavailable." << endl;
        }

        // The following lines are used to open the GPIO file specified in the configuration file to control the brake LEDs.
        ostream *outBrakeLED = NULL;
        const string BRAKE_LED_OFF = "0"; // Value to turn off the brake LEDs.
        const string BRAKE_LED_ON = "1"; // Value to turn off the brake LEDs.
        URL url_brake_LED_file(BRAKE_LED_FILE);
        try {
            outBrakeLED = &(StreamFactory::getInstance().getOutputStream(url_brake_LED_file));
        }
        catch(InvalidArgumentException &iae) {
            // The specified file could not be opened. Assuming that there is not such file available.
            outBrakeLED = NULL;

            cerr << "(ProxySTM32F4) warning: '" << iae.toString() << "': Brake LEDs unavailable." << endl;
        }

        // The following lines are used to open the GPIO file specified in the configuration file to control the left turning LEDs.
        ostream *outTurnLeftLED = NULL;
        const string TURN_LEFT_LED_OFF = "0"; // Value to turn off the turn left LEDs.
        const string TURN_LEFT_LED_ON = "1"; // Value to turn off the turn left LEDs.
        URL url_turn_left_LED_file(TURN_LEFT_LED_FILE);
        try {
            outTurnLeftLED = &(StreamFactory::getInstance().getOutputStream(url_turn_left_LED_file));
        }
        catch(InvalidArgumentException &iae) {
            // The specified file could not be opened. Assuming that there is not such file available.
            outTurnLeftLED = NULL;

            cerr << "(ProxySTM32F4) warning: '" << iae.toString() << "': Brake turning LEDs (left) unavailable." << endl;
        }
        uint32_t turnLeftLEDcounter = 0;
        const uint32_t turnLeftLEDcounterMax = ((uint32_t)(getFrequency()) < 2 ? 2 : (uint32_t)(getFrequency()));

        // The following lines are used to open the GPIO file specified in the configuration file to control the right turning LEDs.
        ostream *outTurnRightLED = NULL;
        const string TURN_RIGHT_LED_OFF = "0"; // Value to turn off the turn left LEDs.
        const string TURN_RIGHT_LED_ON = "1"; // Value to turn off the turn left LEDs.
        URL url_turn_right_LED_file(TURN_RIGHT_LED_FILE);
        try {
            outTurnRightLED = &(StreamFactory::getInstance().getOutputStream(url_turn_right_LED_file));
        }
        catch(InvalidArgumentException &iae) {
            // The specified file could not be opened. Assuming that there is not such file available.
            outTurnRightLED = NULL;

            cerr << "(ProxySTM32F4) warning: '" << iae.toString() << "': Brake turning LEDs (right) unavailable." << endl;
        }
        uint32_t turnRightLEDcounter = 0;
        const uint32_t turnRightLEDcounterMax = ((uint32_t)(getFrequency()) < 2 ? 2 : (uint32_t)(getFrequency()));


        // Open serial port.
        core::wrapper::SerialPort *serialPort = core::wrapper::SerialPortFactory::createSerialPort(SERIAL_PORT, SERIAL_SPEED);

        STM32F4Protocol protocol;
        protocol.setSTM32F4DataListener(this);           // ProxySTM32F4 shall receive completely received packets from the STM32F4Protocol.
        protocol.setStringSender(serialPort);            // The STM32F4Protocol shall use the SerialPort to send data.
        serialPort->setPartialStringReceiver(&protocol); // The SerialPort will distribute partially received data to STM32F4Protocol.

        // Start receiving.
        serialPort->start();

        // Values to be sent to the STM32F4 board.
        double steering = 0;
        double speed = 0;

        while (getModuleState() == ModuleState::RUNNING) {
            // Try to wait for the "start" signal: The file USER_START_BUTTON_FILE/value changes from 1 --> 0.
            if (userButtonAvailable) {
                if (in != NULL) {
                    if (in->good()) {
                        int16_t input;
                        *in >> input;
                        if (userButtonPressed && (USER_START_BUTTON_PRESSED_VALUE != input)) {
                            userButtonPressed = false;
                            userButtonReleased = true;

                            {
                                Lock l(m_userButtonMutex);
                                m_userButtonData.setButtonStatus(UserButtonData::RELEASED);
                            }

                            if (m_debug) {
                                cout << "button released!" << endl;
                            }
                        }

                        if (!userButtonPressed && (USER_START_BUTTON_PRESSED_VALUE == input)) {
                            userButtonPressed = true;
                            userButtonReleased = false;

                            {
                                Lock l(m_userButtonMutex);
                                m_userButtonData.setButtonStatus(UserButtonData::PRESSED);
                            }

                            if (m_debug) {
                                cout << "button pressed!" << endl;
                            }
                        }
                    }

                    // Clear any error states of the stream.
                    in->clear();
                    // Seek to the beginning of the input stream.
                    in->seekg(ios::beg);

                    if (userButtonPressed && !userButtonReleased && !userButtonPressedTS_set) {
                        userButtonPressedTS = TimeStamp();
                        userButtonPressedTS_set = true;

                        // Turn off the turn left LEDs.
                        if (outTurnLeftLED != NULL) {
                            *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                            outTurnLeftLED->flush();
                        }
                        // Turn off the turn right LEDs.
                        if (outTurnRightLED != NULL) {
                            *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                            outTurnRightLED->flush();
                        }
                        // Turn off the brake LEDs.
                        if (outBrakeLED != NULL) {
                            *outBrakeLED << BRAKE_LED_OFF << endl;
                            outBrakeLED->flush();
                        }
                    }
                    if (!userButtonPressed && userButtonReleased) {
                        userButtonReleasedTS = TimeStamp();

                        // Reset TS for button pressed.
                        userButtonPressedTS_set = false;

                        // Reset buttonReleased flag to measure again.
                        userButtonReleased = false;

                        userButtonPressedDuration = (userButtonReleasedTS - userButtonPressedTS).toMicroseconds() / 1000000.0;

                        {
                            Lock l(m_userButtonMutex);
                            m_userButtonData.setDuration(userButtonPressedDuration);
                            m_userButtonData.setButtonStatus(UserButtonData::RELEASED);
                        }

                        if (m_debug) {
                            cerr << "User button pressed for " << userButtonPressedDuration << "s." << endl;
                        }
                    }
                }
            }

            bool foundData = false;
            VehicleControl vc;

            // Regular data processing: try to find matching containers.
            while (!lifo.isEmpty() && !foundData) {
                // Read next received container.
                Container con = lifo.pop();

                if (con.getDataType() == Container::VEHICLECONTROL) {
                    vc = con.getData<VehicleControl>();
                    foundData = true;
                }
            }

            // Clear lifo for next cycle.
            lifo.clear();

            if (foundData) {
                // Translate to HW/SW-interface specific format.
                if (m_debug) {
                    cout << "Received: " << vc.toString() << endl;
                }

                // Check if we need to decorate ourselves with the LEDs.
                if (vc.getBrakeLights()) {
                    if (m_debug) {
                        cout << "Turn ON brake lights." << endl;
                    }
                    // Turn on the brake LEDs.
                    if (outBrakeLED != NULL) {
                        *outBrakeLED << BRAKE_LED_ON << endl;
                        outBrakeLED->flush();
                    }
                }
                else {
                    if (m_debug) {
                        cout << "Turn OFF brake lights." << endl;
                    }
                    // Turn off the brake LEDs.
                    if (outBrakeLED != NULL) {
                        *outBrakeLED << BRAKE_LED_OFF << endl;
                        outBrakeLED->flush();
                    }
                }

                if (vc.getLeftFlashingLights()) {
                    if (turnLeftLEDcounter < (turnLeftLEDcounterMax/2)) {
                        if (m_debug) {
                            cout << "Left flashing OFF." << endl;
                        }
                        if (outTurnLeftLED != NULL) {
                            *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                            outTurnLeftLED->flush();
                        }
                    }
                    if (turnLeftLEDcounter >= (turnLeftLEDcounterMax/2)) {
                        if (m_debug) {
                            cout << "Left flashing ON." << endl;
                        }
                        if (outTurnLeftLED != NULL) {
                            *outTurnLeftLED << TURN_LEFT_LED_ON << endl;
                            outTurnLeftLED->flush();
                        }
                    }
                    turnLeftLEDcounter++;
                    turnLeftLEDcounter %= turnLeftLEDcounterMax;
                }
                else {
                    if (m_debug) {
                        cout << "Turn OFF left flashing lights." << endl;
                    }
                    // Turn off the left turn LEDs.
                    if (outTurnLeftLED != NULL) {
                        *outTurnLeftLED << TURN_LEFT_LED_OFF << endl;
                        outTurnLeftLED->flush();
                    }
                    turnLeftLEDcounter = 0;
                }

                if (vc.getRightFlashingLights()) {
                    if (turnRightLEDcounter < (turnRightLEDcounterMax/2)) {
                        if (m_debug) {
                            cout << "Right flashing OFF." << endl;
                        }
                        if (outTurnRightLED != NULL) {
                            *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                            outTurnRightLED->flush();
                        }
                    }
                    if (turnRightLEDcounter >= (turnRightLEDcounterMax/2)) {
                        if (m_debug) {
                            cout << "Right flashing ON." << endl;
                        }
                        if (outTurnRightLED != NULL) {
                            *outTurnRightLED << TURN_RIGHT_LED_ON << endl;
                            outTurnRightLED->flush();
                        }
                    }
                    turnRightLEDcounter++;
                    turnRightLEDcounter %= turnRightLEDcounterMax;
                }
                else {
                    if (m_debug) {
                        cout << "Turn OFF right flashing lights." << endl;
                    }
                    // Turn off the right turn LEDs.
                    if (outTurnRightLED != NULL) {
                        *outTurnRightLED << TURN_RIGHT_LED_OFF << endl;
                        outTurnRightLED->flush();
                    }
                    turnRightLEDcounter = 0;
                }


                // Transform data from VehicleControl to UDP_Server packet data.
                // Transform vc.getSteeringWheelAngle() (-MAX_STEERING_LEFT_RAD .. 0 (straight) .. MAX_STEERING_RIGHT_RAD (max right)) to +255 (max left) .. 127 (straight) .. 0 (max right) in the regular case
                // Transform vc.getSteeringWheelAngle() (MAX_STEERING_LEFT_RAD .. 0 (straight) .. -MAX_STEERING_RIGHT_RAD (max right)) to +255 (max left) .. 127 (straight) .. 0 (max right) in the inverted case
                if (fabs(vc.getSteeringWheelAngle()) < 1e-2) {
                    // Straight forward.
                    steering = 127;
                }
                else {
                    if (!INVERTED_STEERING) {
                        // Negative vc.getSteeringWheelAngle() == steer to the left.
                        if (vc.getSteeringWheelAngle() < 0) {
                            const double MAX_LEFT = 255;
                            const double CENTER = 127;
                            steering = CENTER - (vc.getSteeringWheelAngle() * ((MAX_LEFT-CENTER)/MAX_STEERING_LEFT_RAD));
                            if (steering > MAX_LEFT) {
                                steering = MAX_LEFT;
                            }
                            if (steering < CENTER) {
                                steering = CENTER;
                            }
                        }
                        else if (vc.getSteeringWheelAngle() > 0) {
                            const double MAX_RIGHT = 0;
                            const double CENTER = 127;
                            steering = CENTER - (vc.getSteeringWheelAngle() * ((CENTER-MAX_RIGHT)/MAX_STEERING_RIGHT_RAD));
                            if (steering < MAX_RIGHT) {
                                steering = MAX_RIGHT;
                            }
                            if (steering > CENTER) {
                                steering = CENTER;
                            }
                        }
                    }
                    else {
                        // Negative vc.getSteeringWheelAngle() == steer to the right.
                        if (vc.getSteeringWheelAngle() < 0) {
                            const double MAX_RIGHT = 0;
                            const double CENTER = 127;
                            steering = CENTER + (vc.getSteeringWheelAngle() * ((CENTER-MAX_RIGHT)/MAX_STEERING_RIGHT_RAD));
                            if (steering < MAX_RIGHT) {
                                steering = MAX_RIGHT;
                            }
                            if (steering > CENTER) {
                                steering = CENTER;
                            }
                        }
                        else if (vc.getSteeringWheelAngle() > 0) {
                            const double MAX_LEFT = 255;
                            const double CENTER = 127;
                            steering = CENTER + (vc.getSteeringWheelAngle() * ((MAX_LEFT-CENTER)/MAX_STEERING_LEFT_RAD));
                            if (steering > MAX_LEFT) {
                                steering = MAX_LEFT;
                            }
                            if (steering < CENTER) {
                                steering = CENTER;
                            }
                        }
                    }
                }

                speed = vc.getSpeed();
                if (speed < -SPEED_MAX) {
                    speed = -SPEED_MAX;
                }
                if (speed > SPEED_MAX) {
                    speed = SPEED_MAX;
                }
            }

            // Send the last known values for speed and steering to STM32F4.
            protocol.request(STM32F4Protocol::NoData, speed, steering);

            // Distribute data.
            {
                Lock l(m_userButtonMutex);
                // Distribute UserButtonData.
                // MSV: Create a container with user data Container::USER_BUTTON.
                Container c(Container::USER_BUTTON, m_userButtonData);
                getConference().send(c);
            }

            {
                Lock l(m_sensorBoardMutex);
                // Distribute SensorBoardData.
                // MSV: Create a container with user data Container::USER_DATA_0.
                Container c(Container::USER_DATA_0, m_sensorBoardData);
                getConference().send(c);
            }

            {
                Lock l(m_vehicleDataMutex);
                // Distribute VehicleData.
                // MSV: Create a container with user data Container::VEHICLEDATA.
                Container c(Container::VEHICLEDATA, m_vehicleData);
                getConference().send(c);
            }
        }

        // Stop the car after stopping the ProxySTM32F4
        protocol.request(STM32F4Protocol::NoData, 0, 0);

        serialPort->stop();

        // Destroy connections to UDP_Server.
        OPENDAVINCI_CORE_DELETE_POINTER(serialPort);

        return ModuleState::OKAY;
    }

} // msv


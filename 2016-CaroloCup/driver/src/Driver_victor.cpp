/*
* Mini-Smart-Vehicles.
*
* This software is open source. Please see COPYING and AUTHORS for further information.
*
*
*/

#include <cstdio>
#include <cmath>

#include "core/io/conference/ContainerConference.h"
#include "core/data/Container.h"

#include "GeneratedHeaders_AutomotiveData.h"

#include "Driver.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace coredata;

    int MinParkingDist = 54;
//int MaxParkingDist = 70;
    int SafeDistance = 30;
    int Distance;
    int CurrentDistSpot;
    int CurrentDistSpot2;
//int DesiredDistance1 = 105; 
    int DesiredDistance1 = 115;
//int DesiredDistance2 = 70;
    int DesiredDistance2 = 55;
//int DesiredDistance3 = 48;
    int DesiredDistance3 = 80;
    int DesiredDistance4 = 44;
    int DesiredDistance5 = 3;
//int SpeedF1 = 3;
//int SpeedF2 = 6;
//int SpeedB1 = -4;
//int SpeedB2 = -6;
    double SpeedF1 = 0.5;
    double SpeedF2 = 0.5;
    double SpeedB1 = -0.5;
    double SpeedB2 = -0.5;
    double Stop_Speed = 0.0;
    int CurrentDist;
    int CurrentDist1;
    int CurrentDist2;
    int CurrentDist3;
    int CurrentDist4;
    int CurrentDist5;
    int USFront;
    int USRear;
    int IRdis_SL;
    int IRdis_RL;
    int IRdis_RR;
    int IRdis_SR;
    bool rightIndicator = false;
    bool leftIndicator = false;
    bool brakeIndicator = false;
    double ParkAngle;
    int gyro;
    int forwardDistance = 0;


    Driver::Driver(const int32_t &argc, char **argv) :
            TimeTriggeredConferenceClientModule(argc, argv, "Driver"), driving_state(DRIVE), parking_state(
            BACKWARDS_RIGHT) {
    }

    Driver::~Driver() {
    }

    void Driver::setUp() {
        // This method will be call automatically _before_ running body().
    }

    void Driver::tearDown() {
        // This method will be call automatically _after_ return from body().
    }


    double start_timer;
    double start_timer2;
    double time_taken;
    double time_taken2;
    double start_timerB;
    double time_takenB;
    double start_timerIndicator;
    double time_takenIndicator;


    double driving_speed;    // Speed of the car
    int desiredSteeringWheelAngle;// Angle of the wheels

// This method will do the main data processing job.
    coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode Driver::body() {
        driving_state = DRIVE;
        parking_state = BACKWARDS_RIGHT;
        // Get configuration data.
        KeyValueConfiguration kv = getKeyValueConfiguration();
//	const uint32_t m_sensorId = kv.getValue<int32_t> ("irus.sensor2.id");
//	cout << "***********  Sensor ID: " << m_sensorId << endl;

        VehicleControl vc;

        TimeStamp start;
        int gapWidth = 0;
        //bool measuring = false;


        while (getModuleStateAndWaitForRemainingTimeInTimeslice() == coredata::dmcp::ModuleStateMessage::RUNNING) {
            // In the following, you find example for the various data sources that are available:

            // 1. Get most recent vehicle data:
            Container containerVehicleData = getKeyValueDataStore().get(
                    Container::VEHICLEDATA);
            VehicleData vd = containerVehicleData.getData<VehicleData>();
            // cerr << "Most recent vehicle data: '" << vd.toString() << "'" << endl;

            // 2. Get most recent sensor board data:
            Container containerSensorBoardData = getKeyValueDataStore().get(Container::USER_DATA_0);
            SensorBoardData sbd =
                    containerSensorBoardData.getData<SensorBoardData>();
// 		cout << "Most recent sensor board data: '" << sbd.toString() << "'"
// 				<< endl;

            // 3. Get most recent user button data:
            Container containerUserButtonData = getKeyValueDataStore().get(
                    Container::USER_BUTTON);
            UserButtonData ubd = containerUserButtonData.getData<UserButtonData>();
            // cerr << "Most recent user button data: '" << ubd.toString() << "'"
            // << endl;

            // 4. Get most recent steering data as fill from lanedetector for example:
            Container containerSteeringData = getKeyValueDataStore().get(
                    Container::USER_DATA_1);
            SteeringData sd = containerSteeringData.getData<SteeringData>();
            // cerr << "Most recent steering data: '" << sd.toString() << "'" << endl;

            //Sensors
            // new configuration
            IRdis_SL = sbd.getValueForKey_MapOfDistances(1); // Side Left IR // *on legendary is //theFront-Side-Right
            IRdis_RL = sbd.getValueForKey_MapOfDistances(2); // Rear Left IR
            IRdis_RR = sbd.getValueForKey_MapOfDistances(3); // Rear Right IR
            IRdis_SR = sbd.getValueForKey_MapOfDistances(5); // Side Right IR
            USFront = sbd.getValueForKey_MapOfDistances(0); // Front UltraSonic
            USRear = sbd.getValueForKey_MapOfDistances(4); // Rear UltraSonic
            gyro = sbd.getValueForKey_MapOfDistances(8); // Gyroscope
            //WheelEncoder
            Distance = (sbd.getValueForKey_MapOfDistances(6) + sbd.getValueForKey_MapOfDistances(7)) /
                       2;// WheeelEncoder Data (mm)
            //	Distance =vd.getAbsTraveledPath()*10;
            //Status
            cout << " ===== Rear IRs difference: " << abs(IRdis_RL - IRdis_RR) << endl;
            cout << " ===== Side Left Infrared reading: " << IRdis_SL << endl;
            cout << " ===== Rear Left Infrared reading: " << IRdis_RL << endl;
            cout << " ===== Rear Right Infrared reading: " << IRdis_RR << endl;
            cout << " ===== Side Right Infrared reading: " << IRdis_SR << endl;
            cout << " ===== Front UltraSonic reading: " << USFront << endl;
            cout << " ===== Rear UltraSonic reading: " << USRear << endl;
            cout << " ===== WheeelEncoder Data (mm): " << Distance << endl;
            cout << " ===== Driving_Speed: " << driving_speed << endl;
            cout << " ===== DesiredSteeringWheelAngle: " << desiredSteeringWheelAngle << endl;
            cout << " ===== Parking spot length: " << gapWidth << endl;
            cout << " ===== Parking Preparation length: " << forwardDistance << endl;
            cout << " ===== Ptime_takenIndicator: " << time_takenIndicator << endl;
            cout << " ===== Gyroscope Angle " << gyro << endl;
            cout << "  ===== Angle :" << ParkAngle << endl;

            cout << "========  REAched" << (CurrentDist1 + DesiredDistance2) << endl;

// 		
            // Design your control algorithm here depending on the input data from above.
            switch (driving_state) {
                case DRIVE: {
                    cout << "\t In drive mode" << endl;
                    driving_speed = SpeedF2;
//			desiredSteeringWheelAngle = -1;
//			desiredSteeringWheelAngle = 0;
                    if (gyro < 0) { desiredSteeringWheelAngle = 1; }
                    else if (gyro > 0) { desiredSteeringWheelAngle = -1; }
                    else { desiredSteeringWheelAngle = 0; }

                    if ((USFront < SafeDistance && USFront > 2)) {
                        driving_state = NO_POSSIBLE_PARKING_PLACE;
                    }
                    if ((IRdis_SL < 25 && IRdis_SL > 2)) {
                        driving_state = START_OBST;

                    }
                }
                    break;

                case START_OBST: {
                    cout << "\t \t START_OBST mode" << endl;
                    //driving_speed = 1;

                    if ((USFront < SafeDistance && USFront > 2)) {
                        driving_state = NO_POSSIBLE_PARKING_PLACE;

                    }


                    if ((IRdis_SL > 25 || IRdis_SL < 2)) {
                        driving_state = POSSIBLE_SPOT;
                        CurrentDistSpot = Distance;
                    }
                }
                    break;

                case POSSIBLE_SPOT: {

// 			cout << "---- DIstance so far: " << Distance << endl;
                    cout << "\t POSSIBLE_SPOT" << endl;;

                    if ((USFront < SafeDistance && USFront > 2)) {
                        driving_state = NO_POSSIBLE_PARKING_PLACE;

                    }

                    if (IRdis_SL < 25 && IRdis_SL > 2) {
//         		if(IRdis_SL < 2){
//			  gapWidth = (Distance - CurrentDistSpot);
                        gapWidth = (Distance - CurrentDistSpot);
//			cout << "\t GapWidth" << gapWidth << endl;
                        //CurrentDistSpot2 = Distance;
                        if (gapWidth > MinParkingDist) {
                            desiredSteeringWheelAngle = 0;
//desiredSteeringWheelAngle=42;
                            driving_speed = SpeedF1;
                            CurrentDist = Distance;
                            driving_state = STOP_FOR_PARKING;
                            //driving_state = PREPARE_FOR_PARKING;
                        } else {
                            driving_state = DRIVE;
                        }
                    }

                    cout << "\t Parking spot length: " << gapWidth << endl;
                }
                    break;

                case STOP_FOR_PARKING: {
                    driving_speed = SpeedF1;
                    rightIndicator = true;
                    cout << "\t STOP_FOR_PARKING" << endl;

                    cout << "++++++++++ Stoping timer: " << time_taken << endl;

                    if ((USFront < SafeDistance && USFront > 2)) {
                        driving_state = NO_POSSIBLE_PARKING_PLACE;

                    }
//			if (Distance > (CurrentDist + DesiredDistance1)) {  
                    if (Distance > (CurrentDist + (DesiredDistance1 - gapWidth))) {
                        //parking(vc, vd);
                        forwardDistance = Distance - CurrentDist;
                        CurrentDist1 = Distance;
                        driving_state = PARKING;
                        driving_speed = Stop_Speed;


                    }

                }
                    break;

                case PARKING: {
                    cout << "========: PARKING (calling parking)" << endl;
                    parking();
                }
                    break;

                case NO_POSSIBLE_PARKING_PLACE: {

                    rightIndicator = false;
                    cout << "\t\t========  NO_POSSIBLE_PARKING_PLACE" << endl;
                    driving_speed = Stop_Speed;
                }
                    break;


                default: {

                    cout << "Non of these states" << endl;

                    driving_speed = Stop_Speed;
//			desiredSteeringWheelAngle = -1;
                    desiredSteeringWheelAngle = 0;
                }
            }

            // Create vehicle control data.

            // With setSpeed you can set a desired speed for the vehicle in the range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards)
            vc.setSpeed(driving_speed);

            // With setSteeringWheelAngle, you can steer in the range of -26 (left) .. 0 (straight) .. +25 (right)
            //double desiredSteeringWheelAngle = 0; // 4 degree but SteeringWheelAngle expects the angle in radians!
            vc.setSteeringWheelAngle(desiredSteeringWheelAngle);

            // You can also turn on or off various lights:
            vc.setBrakeLights(brakeIndicator);
            vc.setFlashingLightsLeft(leftIndicator);
            vc.setFlashingLightsRight(rightIndicator);

            // Create container for finally sending the data.
            Container c(Container::VEHICLECONTROL, vc);
            // Send container.
            getConference().send(c);

        }
        driving_speed = 0;
        return coredata::dmcp::ModuleExitCodeMessage::OKAY;
    }


    void Driver::parking() {
        cout << "\t\t========:  parking()" << endl;
        switch (parking_state) {
            case BACKWARDS_RIGHT: {

                rightIndicator = true;
                driving_speed = SpeedB2; //driving_speed = SpeedB2;
                desiredSteeringWheelAngle = 42;
                cout << "========  BACKWARDS_RIGHT" << endl;
//		if (Distance > (CurrentDist1 + DesiredDistance2)) {
                if (Distance > (CurrentDist1 + DesiredDistance2)) {
                    parking_state = BACKWARDS_LEFT;
                    CurrentDist2 = Distance;

                }
            }
                break;

            case BACKWARDS_LEFT: {

                driving_speed = SpeedB1; //driving_speed = SpeedB1;
                desiredSteeringWheelAngle = -25;
                cout << "\t========  BACKWARDS_LEFT" << endl;
//		if ((Distance > (CurrentDist2 + DesiredDistance3)) || (IRdis_RL < 10 && IRdis_RL > 2) || (IRdis_RR < 10 && IRdis_RR > 2)) {			
//		if ((Distance > (CurrentDist2 + DesiredDistance3))) {			
//		if ((Distance > (CurrentDist2 + DesiredDistance3)) || USRear < 5 ) {
                if ((Distance > (CurrentDist2 + DesiredDistance3)) || (gyro < 2 && gyro > -2)) {
                    parking_state = DONE;
                    CurrentDist3 = Distance;
                }
            }
                break;
/*	case STOP_FOR_FRONT: {
    driving_speed = Stop_Speed;
    desiredSteeringWheelAngle = 0;
    parking_state = FORWARD_RIGHT;
        }
        break;
*/
            case FORWARD_RIGHT: {
                driving_speed = SpeedF1;
                desiredSteeringWheelAngle = 42;
                cout << "\t\t========  FORWARD_RIGHT" << endl;
                ParkAngle = asin((IRdis_RL - IRdis_RR) / 11.0); //11 = dist between IR's
                if ((USFront < 12 && USFront > 1)) {//if (USFront < 2) {
//		if ((Distance > (CurrentDist3 + DesiredDistance4)) && (USFront < 2)){
                    parking_state = DONE;//		parking_state = BACK_AGAIN;

                }
//		if ((abs (IRdis_RL - IRdis_RR)) < 1) {
//		  //(Distance > (CurrentDist3 + DesiredDistance4)) || 
//			parking_state = STOP;
// 			TimeStamp currentTime3;
// 			start_timer2 = currentTime3.toMicroseconds() / 1000.0;
//			TimeStamp currentTime5;
//			start_timerIndicator = currentTime5.toMicroseconds() / 1000.0;

//		} 
            }

                break;


            case BACK_AGAIN: {
/*		driving_speed = SpeedB1;
    desiredSteeringWheelAngle = -42;
    cout << "\t========  BACK_AGAIN"  << endl;
    if ((((abs (IRdis_RL - IRdis_RR)) < 1) && ((IRdis_RL < 15 && IRdis_RL > 2) && (IRdis_RR < 15 && IRdis_RR > 2))) || (USRear < 10)){
//                      (Distance > (CurrentDist4 + DesiredDistance5 || (Distance < (CurrentDist4 + DesiredDistance5))
            TimeStamp currentTime5;
        start_timerIndicator = currentTime5.toMicroseconds() / 1000.0;
        parking_state = STOP;
    }
*/
                rightIndicator = false;
                driving_speed = Stop_Speed;
                desiredSteeringWheelAngle = 0;
                parking_state = FORWARD_RIGHT;

            }

                break;

            case STOP: {
                driving_speed = Stop_Speed;
//		desiredSteeringWheelAngle = -1;
                desiredSteeringWheelAngle = 0;
                rightIndicator = false;
                cout << "\t\t========  STOP" << endl;
                cout << "****  stop the car  ****" << endl;
                TimeStamp currentTime6;
                time_takenIndicator = (currentTime6.toMicroseconds() / 1000.0) - start_timerIndicator;
                if (time_takenIndicator < 4000) {
                    rightIndicator = true;
                    leftIndicator = true;

                } else {
                    parking_state = DONE;
                }
            }
                break;

            case DONE: {
                rightIndicator = false;
                leftIndicator = false;
                cout << "\t\t========  DONE" << endl;
                driving_speed = Stop_Speed;
            }

                break;
            default: {

                cout << "Non of these states" << endl;

                //driving_speed = 4;
                //desiredSteeringWheelAngle = 0;
            }
        }
    }

} // msv
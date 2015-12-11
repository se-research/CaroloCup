//
// Created by Mickaël on 2015-12-11.
//

#include "ParkingDriver.h"

bool debug = false;

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace coredata;

    int MinParkingDist = 54;
    int SafeDistance = 30;
    int Distance;
    int CurrentDistSpot;
    int CurrentDist3;
    int DesiredDistance1 = 115;
    int DesiredDistance2 = 55;
    int DesiredDistance3 = 80;
    float SpeedF1 = 0.5;
    double SpeedF2 = 0.5;
    float SpeedB1 = -0.5f;
    float SpeedB2 = -0.5f;
    float Stop_Speed = 0.0;
    int CurrentDist;
    int CurrentDist1;
    int CurrentDist2;
    int USFront;
    int USRear;
    int IRdis_SL;
    int IRdis_RL;
    int IRdis_RR;
    int IRdis_SR;
    double ParkAngle;
    int gyro;
    int forwardDistance = 0;

    double time_taken;
    double start_timerIndicator;
    double time_takenIndicator;

    int gapWidth;

    ParkingDriver::ParkingDriver(const int32_t &argc, char **argv) :
            DriverGeneric(argc, argv),
            driving_state(DRIVE),
            parking_state(BACKWARDS_RIGHT) { }

    ParkingDriver::~ParkingDriver() { }

    void ParkingDriver::Initialize() {
        driving_state = DRIVE;
        parking_state = BACKWARDS_RIGHT;
        gapWidth = 0;
    }

    void ParkingDriver::Routine() {

        // Get most recent sensor board data:
        Container containerSensorBoardData = getKeyValueDataStore().get(Container::USER_DATA_0);
        SensorBoardData sbd =
                containerSensorBoardData.getData<SensorBoardData>();

        //Sensors
        // new configuration
        IRdis_SL = (int) sbd.getValueForKey_MapOfDistances(1); // Side Left IR // *on legendary is //theFront-Side-Right
        IRdis_RL = (int) sbd.getValueForKey_MapOfDistances(2); // Rear Left IR
        IRdis_RR = (int) sbd.getValueForKey_MapOfDistances(3); // Rear Right IR
        IRdis_SR = (int) sbd.getValueForKey_MapOfDistances(5); // Side Right IR
        USFront = (int) sbd.getValueForKey_MapOfDistances(0); // Front UltraSonic
        USRear = (int) sbd.getValueForKey_MapOfDistances(4); // Rear UltraSonic
        gyro = (int) sbd.getValueForKey_MapOfDistances(8); // Gyroscope
        //WheeelEncoder Data (mm)
        Distance = (int) ((sbd.getValueForKey_MapOfDistances(6) + sbd.getValueForKey_MapOfDistances(7)) / 2);
        //Status
        cout << " ===== Rear IRs difference: " << abs(IRdis_RL - IRdis_RR) << endl;
        cout << " ===== Side Left Infrared reading: " << IRdis_SL << endl;
        cout << " ===== Rear Left Infrared reading: " << IRdis_RL << endl;
        cout << " ===== Rear Right Infrared reading: " << IRdis_RR << endl;
        cout << " ===== Side Right Infrared reading: " << IRdis_SR << endl;
        cout << " ===== Front UltraSonic reading: " << USFront << endl;
        cout << " ===== Rear UltraSonic reading: " << USRear << endl;
        cout << " ===== WheeelEncoder Data (mm): " << Distance << endl;
        cout << " ===== Desired speed: " << desiredSpeed << endl;
        cout << " ===== Desired steering: " << desiredSteering << endl;
        cout << " ===== Parking spot length: " << gapWidth << endl;
        cout << " ===== Parking Preparation length: " << forwardDistance << endl;
        cout << " ===== Ptime_takenIndicator: " << time_takenIndicator << endl;
        cout << " ===== Gyroscope Angle " << gyro << endl;
        cout << "  ===== Angle :" << ParkAngle << endl;

        cout << "========  REAched" << (CurrentDist1 + DesiredDistance2) << endl;

        // State machines
        switch (driving_state) {
            case DRIVE: {
                cout << "\t In drive mode" << endl;
                desiredSpeed = (float) SpeedF2;
                //desiredSteeringWheelAngle = -1;
                //desiredSteeringWheelAngle = 0;
                //if (gyro < 0) { desiredSteeringWheelAngle = 1; }
                //else if (gyro > 0) { desiredSteeringWheelAngle = -1; }
                //else { desiredSteeringWheelAngle = 0; }
                //LaneDetectionData ldd;
                //laneFollowing(&ldd);
                desiredSteering = 0;

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
                cout << "\t POSSIBLE_SPOT" << endl;;

                if ((USFront < SafeDistance && USFront > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;

                }

                if (IRdis_SL < 25 && IRdis_SL > 2) {
                    gapWidth = (Distance - CurrentDistSpot);
                    if (gapWidth > MinParkingDist) {
                        desiredSteering = 0;
                        desiredSpeed = SpeedF1;
                        CurrentDist = Distance;
                        driving_state = STOP_FOR_PARKING;
                    } else {
                        driving_state = DRIVE;
                    }
                }

                cout << "\t Parking spot length: " << gapWidth << endl;
            }
                break;

            case STOP_FOR_PARKING: {
                desiredSpeed = 0;
                flashingLightsRight = true;
                cout << "\t STOP_FOR_PARKING" << endl;

                cout << "++++++++++ Stoping timer: " << time_taken << endl;

                if ((USFront < SafeDistance && USFront > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;
                }

                if (Distance > (CurrentDist + (DesiredDistance1 - gapWidth))) {
                    forwardDistance = Distance - CurrentDist;
                    CurrentDist1 = Distance;
                    driving_state = PARKING;
                    desiredSpeed = Stop_Speed;
                }

            }
                break;

            case PARKING: {
                cout << "========: PARKING (calling parking)" << endl;
                parking();
            }
                break;

            case NO_POSSIBLE_PARKING_PLACE: {

                flashingLightsRight = false;
                cout << "\t\t========  NO_POSSIBLE_PARKING_PLACE" << endl;
                desiredSpeed = Stop_Speed;
            }
                break;


            default: {

                cout << "Non of these states" << endl;

                desiredSpeed = Stop_Speed;
//			desiredSteeringWheelAngle = -1;
                desiredSteering = 0;
            }
        }
        return;
    }


    void ParkingDriver::parking() {
        cout << "\t\t========:  parking()" << endl;
        switch (parking_state) {
            case BACKWARDS_RIGHT: {

                flashingLightsRight = true;
                desiredSpeed = SpeedB2;
                desiredSteering = 42;
                cout << "========  BACKWARDS_RIGHT" << endl;
                if (Distance > (CurrentDist1 + DesiredDistance2)) {
                    parking_state = BACKWARDS_LEFT;
                    CurrentDist2 = Distance;

                }
            }
                break;

            case BACKWARDS_LEFT: {

                desiredSpeed = SpeedB1;
                desiredSteering = -42;
                cout << "\t========  BACKWARDS_LEFT" << endl;
                if ((Distance > (CurrentDist2 + DesiredDistance3)) || (gyro < 2 && gyro > -2)) {
                    parking_state = DONE;
                    CurrentDist3 = Distance;
                }
            }
                break;

            case FORWARD_RIGHT: {
                desiredSpeed = SpeedF1;
                desiredSteering = 42;
                cout << "\t\t========  FORWARD_RIGHT" << endl;
                ParkAngle = asin((IRdis_RL - IRdis_RR) / 11.0); //11 = dist between IR's
                if ((USFront < 12 && USFront > 1)) {
                    parking_state = DONE;
                }
            }

                break;


            case BACK_AGAIN: {
                flashingLightsRight = false;
                desiredSpeed = Stop_Speed;
                desiredSteering = 0;
                parking_state = FORWARD_RIGHT;

            }

                break;

            case STOP: {
                desiredSpeed = Stop_Speed;
                desiredSteering = 0;
                flashingLightsRight = false;
                cout << "\t\t========  STOP" << endl;
                cout << "****  stop the car  ****" << endl;
                TimeStamp currentTime6;
                time_takenIndicator = (currentTime6.toMicroseconds() / 1000.0) - start_timerIndicator;
                if (time_takenIndicator < 4000) {
                    flashingLightsRight = true;
                    flashingLightsLeft = true;

                } else {
                    parking_state = DONE;
                }
            }
                break;

            case DONE: {
                flashingLightsRight = false;
                flashingLightsLeft = false;
                cout << "\t\t========  DONE" << endl;
                desiredSpeed = Stop_Speed;
            }

                break;
            default: {

                cout << "Non of these states" << endl;
            }
        }
    }

} // msv
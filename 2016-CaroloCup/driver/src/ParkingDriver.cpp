//
// Created by Mickaël on 2015-12-11.
// 

#include <LaneFollowingDriver.h>
#include "ParkingDriver.h"

bool debug = false;


namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace coredata;

    int OverallDistance;
    int Distance1;
    int CurrentDistSpot;
    int DistanceEndObstacle;
    int CurrentDist1;
    int CurrentDist2;
    int CurrentDist3;
//   int MinParkingDist = 54;
    int MinParkingDist;
    int MinParkingDist1 = 53; // This is the value of a 63 spot, ie the IR is fucking slow to react. So it looks smaller to the car
    int MinParkingDist2 = 64;
    int SafeDistance = 30;
//  int DesiredDistance1 = 103;
    int DesiredDistance1 = 33;    // Distance after finding the end of the spot
//    int DesiredDistance2 = 55;
//    int DesiredDistance3 = 80;
    int DesiredDistance4 = 50;
// 18  25 28 5
    int DesiredDistance5 = 13;
    int DesiredDistance6 = 23;
    int StopParkingDistance;
// 28
//    float SpeedF1 = 0.5;
//    float SpeedF2 = 0.5;
//    float SpeedF3 = 0.5;
//    float SpeedB1 = -0.2;
//    float SpeedB2 = -0.6;
    float SpeedF1 = 0.5; //0.95
    float SpeedF2 = 0.5; // 1.1
    float SpeedF3 = 0.5;    // 0.9
    float SpeedB1 = -0.5;
    // -1.25
    float SpeedB2 = -0.5;
    // -1.4
    float SpeedB3 = -0.5;
    float Stop_Speed_Forward = -0.3;
    float Stop_Speed_Backward = 0.3;
    float steeringFactor = 1.0;
    int distanceThreshold = 1;
    int IRMinThres = 12;
    int IRMaxThres = 16;
    int USFront;
    int USRear;
    int IR_SideFront;
    int IR_SideBack;
    int IR_BackRight;
    int IR_BackLeft;
    int UVStopValue = 9; // 18
    int IRStopValue = 12;
    int IRFrontValue;
    int IRFrontValue1 = 17;
    int IRFrontValue2 = 18;
    int IRSideValue;
    int USCheck = 6;
    int USStraight;
    int USStraight1 = 25;  // 26
    int USStraight2 = 31;
    int USStrCheck;
    double ParkAngle;
    int currentGyro;
    int gyroBackRight1 = 25; //7 11
//  25 18 10 20
    int gyroBackRight2 = 29; // 26   // 63 cm gap 
    int gyroBackRight3 = 29; // 70 cm gap 
    int count;
    int countStop;
    int countStop1 = 0;
    int countStop2 = 0;
    int countDeath;
    int countDeath1 = 3;
    int countDeath2 = 3;
    int gyroStopValue = 4;
    int gyroBackValue;
    int gyro;
    int forwardDistance;
    int backRight;
    int backLeft;
    int backStraight;
    int backStopValue;
    double time_taken;
    double start_timerIndicator;
    double time_takenIndicator;
    int gapWidth;
    int foundBox;
    int obstacleDetection;
    int foundSmallGap;
    double sec;
    float ActualSpeed;

    std::list<int> IrRearLeftReadings;
    std::list<int> IrRearRightReadings;
//    std::list<int> IrFrontLeftReadings;
//    std::list<int> IrFrontRightReadings;


    ParkingDriver::ParkingDriver(const int32_t &argc, char **argv) :
            DriverGeneric(argc, argv),
            driving_state(DRIVE),
            parking_state(BACKWARDS_RIGHT),
            laneDriver(0),
            m_timestamp(0),
            previousError(0.0) {
        runStartBoxSequence = false;
        //Create lane driver
        laneDriver = new LaneFollowingDriver(argc, argv);
        // Init laneDriver module
        laneDriver->runModule();
        driving_state = DRIVE;
        parking_state = BACKWARDS_RIGHT;
        OverallDistance = 0;
        Distance1 = 0;
        gapWidth = 0;
        MinParkingDist = 0;
        forwardDistance = 0;
        gyroBackValue = 0;
        USStraight = 0;
        countStop = 0;
        CurrentDistSpot = 0;
        DistanceEndObstacle = 0;
        CurrentDist1 = 0;
        CurrentDist2 = 0;
        CurrentDist3 = 0;
        IRSideValue = 0;
        brakeLights = false;
        flashingLightsRight = false;
        flashingLightsLeft = false;
        USStrCheck = 0;
        countDeath = 0;
        IRFrontValue = 0;
    }

    ParkingDriver::~ParkingDriver() {
        free(laneDriver);
    }

    void ParkingDriver::Initialize() {
    }


    float ParkingDriver::SpeedControl(float setSpeed, float actualSpeed, double timeStep, float *pError) {
        return setSpeed;
        float error;
        float kd = 0.0;
        float kpf = 0.50;
        float kpb = 0.0;
        float speed;
        error = abs(setSpeed) - actualSpeed;
        if (setSpeed > 0.0) {
            speed = setSpeed + kpf * error + kd * (error - *pError / timeStep);
        }
        else if (setSpeed < 0.0) {
            speed = -(abs(setSpeed) + kpb * error + kd * (error - *pError / timeStep));
        }
        *pError = error;
        return speed;
    }


    void ParkingDriver::Routine() {

        if (m_timestamp != 0) {
            TimeStamp now;
            int32_t currTime = (int32_t) now.toMicroseconds();
            sec = (currTime - m_timestamp) / (1000000.0);
        }
        TimeStamp now;
        m_timestamp = (int32_t) now.toMicroseconds();


        // Get most recent sensor board data:
        Container containerSensorBoardData = getKeyValueDataStore().get(Container::USER_DATA_0);
        SensorBoardData sbd =
                containerSensorBoardData.getData<SensorBoardData>();

        //Sensors
        // new configuration
        IR_SideFront = (int) sbd.getValueForKey_MapOfDistances(
                1); // Side Left IR // *on legendary is //theFront-Side-Right
        IR_SideBack = (int) sbd.getValueForKey_MapOfDistances(2); // Rear Left IR
        IR_BackRight = (int) sbd.getValueForKey_MapOfDistances(3); // Rear Right IR
        IR_BackLeft = (int) sbd.getValueForKey_MapOfDistances(5); // Side Right IR
        USFront = (int) sbd.getValueForKey_MapOfDistances(0); // Front UltraSonic
        USRear = (int) sbd.getValueForKey_MapOfDistances(4); // Rear UltraSonic
        gyro = (int) sbd.getValueForKey_MapOfDistances(8); // Gyroscope
        //WheeelEncoder Data (mm)
        OverallDistance = (int) ((sbd.getValueForKey_MapOfDistances(6) + sbd.getValueForKey_MapOfDistances(7)) / 2);
        // angle of the car
        ParkAngle = (asin(abs(IR_BackRight - IR_BackLeft) / 11.5)) * 180 / 3.14; //11 = dist between IR's
        ActualSpeed = abs(OverallDistance - Distance1) / (sec * 100);
        Distance1 = OverallDistance;



// Filters
//        IrRearLeftReadings.push_front(IRdis_SR);
//        IrRearRightReadings.push_front(IRdis_RR);
//        IrFrontLeftReadings.push_front(IRdis_SL);
//        IrFrontRightReadings.push_front(IRdis_RL);
//        Filter sensors

//        IRdis_SR = GetIRFiltered(IrRearLeftReadings, 5);
//        IRdis_RR = GetIRFiltered(IrRearRightReadings, 5);
//        IRdis_SL = GetIRFiltered(IrFrontLeftReadings, 5);
//        IRdis_RL = GetIRFiltered(IrFrontRightReadings, 5);


//Status
        cout << " ===== IR - SideFront: " << IR_SideFront << " SideBack:" << IR_SideBack << endl;
        cout << " ===== IR - BackLeft: " << IR_BackLeft << " BackRight:" << IR_BackRight << endl;
        cout << " ===== US: Front:" << USFront << " Rear:" << USRear << endl;
        cout << " ===== WheelEncoder Data (mm): " << OverallDistance << endl;
        cout << " ===== Desired speed: " << desiredSpeed << " Actual Speed: " << ActualSpeed << endl;
        cout << " ===== Desired steering: " << desiredSteering << endl;
        cout << " ===== Parking spot length: " << gapWidth << endl;
        cout << " ===== Parking Preparation length: " << forwardDistance << endl;
        cout << " ===== Gyro" << gyro << endl;
        cout << " ===== Park Angle :" << ParkAngle << endl;
        cout << " ===== Gyro Backwards Angle :" << gyroBackValue << endl;
        cout << " ===== IR Side Value :" << IRSideValue << endl;
        cout << " ===== US Straight Value :" << USStrCheck << "  Number Of Counts" << count << endl;
        cout << " ===== Ptime_takenIndicator: " << time_takenIndicator << endl;
        cout << " ===== flashingLightsRight: " << flashingLightsRight << endl;
        cout << " ===== flashingLightsLeft: " << flashingLightsLeft << endl;
        cout << " ===== setBrakeLights: " << brakeLights << endl;


        // State machines
        switch (driving_state) {
            case DRIVE: {
                foundBox = 0;
                foundSmallGap = 0;
                cout << "\t In drive mode" << endl;
                // Run the lane driver
                laneDriver->Routine();
                desiredSteering = steeringFactor * laneDriver->desiredSteering;
                desiredSpeed = SpeedF2;

                if ((USFront < SafeDistance && USFront > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;
                }
                if ((IR_SideFront < 25 && IR_SideFront > 2)) {
                    driving_state = START_OBST;
                }
            }
                break;

            case START_OBST: {
                cout << "\t \t START_OBST mode" << endl;
                desiredSpeed = SpeedF2;
                laneDriver->Routine();
                desiredSteering = steeringFactor * laneDriver->desiredSteering;
                if ((USFront < SafeDistance && USFront > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;

                }


                if (IR_SideFront < 25 && IR_SideFront > 0) {
                    foundBox = 1;
                }
                if (foundBox == 1) {
                    if ((IR_SideFront > 25 || IR_SideFront < 2)) {
                        driving_state = POSSIBLE_SPOT;
                        CurrentDistSpot = OverallDistance;
                        foundBox = 0;
                    }
                }
            }
                break;

            case POSSIBLE_SPOT: {
                cout << "\t POSSIBLE_SPOT" << endl;;
                laneDriver->Routine();
                desiredSteering = steeringFactor * laneDriver->desiredSteering;

                // Detection of end of lane
                if ((USFront < SafeDistance && USFront > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;
                }


                if (IR_SideFront < 25 && IR_SideFront > 0) {
                    IRSideValue = IR_SideFront;

                    gapWidth = (OverallDistance - CurrentDistSpot);
                    if (gapWidth > MinParkingDist1) {
                        desiredSpeed = SpeedF1;
                        DistanceEndObstacle = OverallDistance;
                        driving_state = STOP_FOR_PARKING;
                        currentGyro = gyro;
                    } else {
                        driving_state = DRIVE;
                    }
                }

                cout << "\t Parking spot length: " << gapWidth << endl;
            }
                break;
                // Stops the car and then switch to INIT_PARKING when car has stopped
            case STOP_FOR_PARKING:

                cout << "\t STOP_FOR_PARKING" << endl;
                cout << "\t OverallDistance: " << OverallDistance << endl;
                cout << "\t DistanceEndObstacle: " << DistanceEndObstacle << endl;
                cout << "\t DesiredDistance1: " << DesiredDistance1 << endl;

                laneDriver->Routine();
                desiredSteering = steeringFactor * laneDriver->desiredSteering;

                if (OverallDistance <= DistanceEndObstacle + DesiredDistance1) {
                    desiredSpeed = SpeedF2;
                }
                else {
                    flashingLightsRight = true;
                    desiredSpeed = Stop_Speed_Forward;

                    // Check if car has stopped to move
                    if (ActualSpeed < 0.01) {
                        desiredSpeed = 0.0;
                        driving_state = INIT_PARKING;
                    }
                }




                /*

                if ((USFront < SafeDistance && USFront > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;
                }

                if ((IRdis_SL > 25 || IRdis_SL < 1)) {
                    foundSmallGap = 1;
                }

                if (foundSmallGap == 1) {
                    if (IRdis_SL < 25 && IRdis_SL > 0) {
                        driving_state = DRIVE;
                    }
                }
                */
//		if(IRSideValue<IRMinThres){
//		DesiredDistance1 = DesiredDistance1-distanceThreshold;}
//		else if(IRSideValue>IRMaxThres){
//               DesiredDistance1 = DesiredDistance1+distanceThreshold;}

                break;

                // Called once before parking to set up variables then switch to PARKING
            case INIT_PARKING:
                cout << "========: INIT_PARKING" << endl;

                //if (OverallDistance > (DistanceEndObstacle + (DesiredDistance1 - gapWidth))) {
//              if (IRdis_RL > 0 && IRdis_RL < 25 ){

                StopParkingDistance = OverallDistance;
                forwardDistance = OverallDistance - DistanceEndObstacle;
                CurrentDist1 = OverallDistance;
                driving_state = PARKING;
                desiredSpeed = 0.0;
                count = 0;
// For 55 cm Gap
                if (gapWidth > MinParkingDist && gapWidth <= MinParkingDist1) {
                    gyroBackValue = gyroBackRight1;
                    countStop = countStop2;
                }
// For 63 cm Gap
                else if (gapWidth > MinParkingDist1 && gapWidth <= MinParkingDist2) {
                    gyroBackValue = gyroBackRight2;
                    USStraight = USStraight1;
                    countStop = countStop1;
                    countDeath = countDeath1;
                    IRFrontValue = IRFrontValue1;
                }
// For 70 cm Gap
                else if (gapWidth > MinParkingDist2) {
                    gyroBackValue = gyroBackRight3;
                    countStop = countStop2;
                    USStraight = USStraight2;
                    countDeath = countDeath2;
                    IRFrontValue = IRFrontValue2;
                }
                // }
                break;

            case PARKING: {
                cout << "========: PARKING (calling parking)" << endl;
                parking();
            }
                break;

            case NO_POSSIBLE_PARKING_PLACE: {

                flashingLightsRight = false;
                cout << "\t\t========  NO_POSSIBLE_PARKING_PLACE" << endl;
                desiredSpeed = 0.0;
            }
                break;


            default: {

                cout << "Non of these states" << endl;

                desiredSpeed = 0.0;
                desiredSteering = 0;
            }
        }
        return;
    }


    void ParkingDriver::parking() {
        cout << "\t\t========:  parking()" << endl;
        switch (parking_state) {
            case BACKWARDS_RIGHT: {
                if (OverallDistance - StopParkingDistance <= 45) {
                    cout << "========  BACKWARDS_RIGHT" << endl;
                    cout << "========  OverallDistance " << OverallDistance << endl;
                    flashingLightsRight = true;
                    desiredSpeed = SpeedB2;
                    desiredSteering = (42 * 3.14) / 180;
                }
                else {
                    parking_state = BACKWARDS_LEFT;
                }

                /*
                if (gyro < (currentGyro - gyroBackValue)) {
                    backRight = OverallDistance - CurrentDist1;
                    parking_state = BACK_AGAIN;
                    CurrentDist2 = OverallDistance;
                }*/
            }
                break;

            case BACKWARDS_LEFT: {

                desiredSpeed = SpeedB3;
                desiredSteering = -(42 * 3.14) / 180;
                int BackStopLimit = 12;
                cout << "\t========  BACKWARDS_LEFT" << endl;
                /*if (((USFront > 0) && (USRear > 0) && ((USFront + USRear) <= USStraight)) || (count > countDeath)) {
                    USStrCheck = (USFront + USRear);
                    parking_state = STOP;
                    desiredSpeed = Stop_Speed_Backward;
                }
                else*/

                if (((USFront > 0) && (USRear > 0) && ((USFront + USRear) <= USStraight)) || count >= 2) {
                    USStrCheck = (USFront + USRear);
                    parking_state = STOP;
                    desiredSpeed = Stop_Speed_Backward;
                }

                else if ((IR_BackLeft < BackStopLimit && IR_BackLeft > 0)
                         || (IR_BackRight < BackStopLimit && IR_BackRight > 0)
                         || (USRear < BackStopLimit && USRear > 0)) {
                    parking_state = FORWARD_RIGHT;
                    desiredSpeed = Stop_Speed_Backward;
                }
            }
                break;

            case FORWARD_RIGHT: {
                flashingLightsRight = false;
                desiredSpeed = SpeedF3;
                desiredSteering = 42 * 3.14 / 180;
                cout << "\t\t========  FORWARD_RIGHT" << endl;
                int FrontStopLimit = 12;

                if (((USFront > 0) && (USRear > 0) && ((USFront + USRear) <= USStraight)) || count >= 2) {
                    USStrCheck = (USFront + USRear);
                    desiredSpeed = Stop_Speed_Backward;
                    parking_state = STOP;
                }
                else if (USFront < FrontStopLimit && USFront > 0) {
                    desiredSpeed = Stop_Speed_Forward;
                    parking_state = BACKWARDS_LEFT;
                    count++;
                }
            }
                break;


            case BACK_AGAIN:
                cout << "\t\t========  BACKWARDS_STRAIGHT" << endl;
                desiredSpeed = SpeedB1;
                desiredSteering = 0;
//		if(Distance > CurrentDist2 + DesiredDistance4){
//                if (USRear < UVStopValue && USRear > 0)  {
                if (IR_BackLeft < IRStopValue && IR_BackLeft > 0) {
                    desiredSpeed = 0;
                    backStraight = OverallDistance - CurrentDist2;
                    desiredSteering = -42 * 3.14 / 180;
                    parking_state = FORWARD_RIGHT;  // BACKWARDS_LEFT
                }
                break;

            case STOP: {
                if ((USRear > USCheck && USRear > 0) && (USFront > USCheck && USFront > 0)) {
                    flashingLightsRight = true;
                    flashingLightsLeft = true;
                    desiredSteering = 0;
                    desiredSpeed = 0.0;
                    parking_state = DONE;
                } else if (USFront < USCheck && USFront > 0) {
                    desiredSteering = 0;
                    desiredSpeed = SpeedB3;
                    parking_state = STOP;
                }
                else if (USRear < USCheck && USRear > 0) {
                    desiredSteering = 0;
                    desiredSpeed = SpeedF3;
                    parking_state = STOP;
                }
            }
                break;

            case DONE: {
                cout << "\t\t========  DONE" << endl;
                backStopValue = gyro;
                desiredSpeed = 0.0;
                desiredSteering = 0.0;
                parking_state = DONE;
            }

                break;

            default: {

                cout << "None of these states" << endl;
            }
        }
    }
} // msv


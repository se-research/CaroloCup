//
// Created by Mickaël on 2015-12-11.
// 

#include <LaneFollowingDriver.h>
#include "ParkingDriver.h"

bool debug = false;


namespace msv {

    using namespace std;
    using namespace odcore::base;
    using namespace odcore::data;
    using namespace odcore::data;


    float maxSteeringPositive = 42.f * 3.14f / 180.f;
    int OverallDistance;
    int Distance1;
    int CurrentDistSpot;
    int DistanceEndObstacle;
    int MinParkingDist;
    int MinParkingDist1 = 53; // This is the value of a 63 spot, ie the IR is fucking slow to react. So it looks smaller to the car
    int MinParkingDist2 = 64;
    int SafeDistance = 30;
    int DesiredDistance1 = 33;    // Distance after finding the end of the spot
    int StopParkingDistance;
    float SpeedF1 = 0.3; //0.95
    float SpeedF2 = 0.3; // 1.1
    float SpeedF3 = 0.3;    // 0.9
    float SpeedParkingBack = -0.3f;
    float SpeedParkingForward = 0.3f;
    float SpeedB3 = -0.3f;
    float Stop_Speed_Forward = -0.3f;
    float Stop_Speed_Backward = 0.3f;
    float steeringFactor = 1.0;
    int USFront;
    int USRear;
    int IR_SideFront;
    int IR_SideBack;
    int IR_BackRight;
    int IR_BackLeft;
    int IRStopValue = 12;
    int IRSideValue;
    int USCheck = 6;
    int USStraight;
    int USStraight1 = 25;  // 26
    int USStraight2 = 31;
    int USStrCheck;
    double ParkAngle;
    int gyroBackRight1 = 25; //7 11
//  25 18 10 20
    int gyroBackRight2 = 29; // 26   // 63 cm gap 
    int gyroBackRight3 = 29; // 70 cm gap 
    int countStop;
    int countStop1 = 0;
    int countStop2 = 0;
    int countDeath1 = 3;
    int countDeath2 = 3;
    int gyroBackValue;
    int gyro;
    int forwardDistance;
    double time_takenIndicator;
    int gapWidth;
    int foundBox;
    double sec;
    float ActualSpeed;


    ParkingDriver::ParkingDriver(const int32_t &argc, char **argv) :
            DriverGeneric(argc, argv),
            driving_state(DRIVE),
            parking_state(BACKWARDS_RIGHT),
            laneDriver(0),
            m_timestamp(0),
            previousError(0.0) {
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
        IRSideValue = 0;
        brakeLights = false;
        flashingLightsRight = false;
        flashingLightsLeft = false;
        USStrCheck = 0;
    }

    ParkingDriver::~ParkingDriver() {
        free(laneDriver);
    }

    void ParkingDriver::Initialize() {
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
        Container containerSensorBoardData = getKeyValueDataStore().get(SensorBoardData::ID());
        SensorBoardData sbd =
                containerSensorBoardData.getData<SensorBoardData>();

        //Sensors
        // new configuration
        IR_SideFront = (int) sbd.getValueForKey_MapOfDistances(1); // Side Left IR // *on legendary is //theFront-Side-Right
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
        cout << " ===== US Straight Value :" << USStrCheck << "  Number Of Counts" << countStop << endl;
        cout << " ===== Ptime_takenIndicator: " << time_takenIndicator << endl;
        cout << " ===== flashingLightsRight: " << flashingLightsRight << endl;
        cout << " ===== flashingLightsLeft: " << flashingLightsLeft << endl;
        cout << " ===== setBrakeLights: " << brakeLights << endl;


        // State machines
        switch (driving_state) {
            case DRIVE: {
                foundBox = 0;
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
                    if (gapWidth > MinParkingDist1 && gapWidth <= MinParkingDist2) {
                        desiredSpeed = SpeedF1;
                        DistanceEndObstacle = OverallDistance;
                        driving_state = STOP_FOR_PARKING;
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
                    brakeLights = true;
                    desiredSpeed = Stop_Speed_Forward;

                    // Check if car has stopped to move
                    if (ActualSpeed < 0.01) {
                        desiredSpeed = 0.0;
                        driving_state = INIT_PARKING;
                        brakeLights = false;
                    }
                }
                break;

                // Called once before parking to set up variables then switch to PARKING
            case INIT_PARKING:
                cout << "========: INIT_PARKING" << endl;
                StopParkingDistance = OverallDistance;
                forwardDistance = OverallDistance - DistanceEndObstacle;
                driving_state = PARKING;
                desiredSpeed = 0.0;
                countStop = 0;
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
                }
// For 70 cm Gap
                else if (gapWidth > MinParkingDist2) {
                    gyroBackValue = gyroBackRight3;
                    countStop = countStop2;
                    USStraight = USStraight2;
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
                    desiredSpeed = SpeedParkingBack;
                    desiredSteering = maxSteeringPositive;
                }
                else {
                    parking_state = BACKWARDS_LEFT;
                }
            }
                break;

            case BACKWARDS_LEFT: {

                int BackStopLimit = 14;
                cout << "\t========  BACKWARDS_LEFT" << endl;

                if (((USFront > 0) && (USRear > 0) && ((USFront + USRear) <= USStraight)) || countStop > 1) {
                    USStrCheck = (USFront + USRear);
                    parking_state = STOP;
                    desiredSpeed = Stop_Speed_Backward;
                    brakeLights = true;
                }

                else if ((IR_BackLeft < BackStopLimit && IR_BackLeft > 0)
                         || (IR_BackRight < BackStopLimit && IR_BackRight > 0)
                         || (USRear < BackStopLimit && USRear > 0)) {
                    parking_state = FORWARD_RIGHT;
                    desiredSpeed = Stop_Speed_Backward;
                    brakeLights = true;
                }
                else{
                    desiredSpeed = SpeedParkingBack;
                    desiredSteering = -maxSteeringPositive;
                    brakeLights = false;
                }
            }
                break;

            case FORWARD_RIGHT: {
                flashingLightsRight = false;
                cout << "\t\t========  FORWARD_RIGHT" << endl;
                int FrontStopLimit = 14;

                if (((USFront > 0) && (USRear > 0) && ((USFront + USRear) <= USStraight)) || countStop > 1) {
                    USStrCheck = (USFront + USRear);
                    desiredSpeed = Stop_Speed_Backward;
                    parking_state = STOP;
                    brakeLights = true;
                }
//                else if (USFront < FrontStopLimit && USFront > 0)  {
		else if ((USFront < FrontStopLimit && USFront > 0) || (IR_BackLeft>17 || IR_BackLeft == 0)) {
                    desiredSpeed = Stop_Speed_Forward;
                    parking_state = BACKWARDS_LEFT;
                    brakeLights = true;
                    countStop++;
                }
                else{
                    brakeLights = false;
                    desiredSpeed = SpeedParkingForward;
                    desiredSteering = maxSteeringPositive;
                }
            }
                break;


            case BACK_AGAIN:
                cout << "\t\t========  BACKWARDS_STRAIGHT" << endl;
                desiredSpeed = SpeedParkingBack;
                desiredSteering = 0;
//		if(Distance > CurrentDist2 + DesiredDistance4){
//                if (USRear < UVStopValue && USRear > 0)  {
                if (IR_BackLeft < IRStopValue && IR_BackLeft > 0) {
                    desiredSpeed = 0;
                    //backStraight = OverallDistance - CurrentDist2;
                    desiredSteering = 0;
                    parking_state = FORWARD_RIGHT;  // BACKWARDS_LEFT
                }
                break;

            case STOP: {
                if ((USRear > USCheck && USRear > 0) && (USFront > USCheck && USFront > 0)) {
                    desiredSteering = 0;
                    desiredSpeed = 0.0;
                    brakeLights = true;

                    flashingLightsRight = true;
                    flashingLightsLeft = true;
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
                desiredSpeed = 0.0;
                desiredSteering = 0.0;
                parking_state = DONE;

                flashingLightsRight = false;
                flashingLightsLeft = false;
                brakeLights = false;
            }
                break;

            default: {

                cout << "None of these states" << endl;
            }
        }
    }
} // msv


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
    int overallDistance;
    int initialDistance;
    int currentDistanceSpot;
    int distanceEndObstacle;
    int minParkingDistance;
    int firstMinParkingDistance = 53; // This is the value of a 63 spot, ie the IR is fucking slow to react. So it looks smaller to the car
    int secondMinParkingDistance = 64;
    int safeDistance = 30;
    int desiredDistance = 33;    // Distance after finding the end of the spot
    int stopParkingDistance;
    float speedF1 = 0.3; //0.95
    //float SpeedF2 = 0.3; // 1.1
    //float SpeedF3 = 0.3;    // 0.9
    float speedParkingBack = -0.3f;
    float speedParkingForward = 0.3f;
    float SpeedB3 = -0.3f;
    float stop_Speed_Forward = -0.3f;
    float stop_Speed_Backward = 0.3f;
    float steeringFactor = 1.0;
    int ultrasonic_Front;
    int ultrasonic_Rear;
    int infrared_SideFront;
    int infrared_SideBack;
    int infrared_BackRight;
    int infrared_BackLeft;
    int infraredStopValue = 12;
    int infraredSideValue;
    int ultrasonicCheck = 6;
    int ulrasonicStraight;
    int firstUltrasonicStraight = 25;  // 26
    int secondUltrasonicStraight = 31;
    int ultrasonicStrCheck;
    double parkAngle;
    int firstGyroBackRight = 25; //7 11
//  25 18 10 20
    int secondGyroBackRight = 29; // 26   // 63 cm gap 
    int thirdGyroBackRight = 29; // 70 cm gap 
    int countStop;
    int firstCountStop = 0;
    int secondCountStop = 0;
    //int countDeath1 = 3;
    //int countDeath2 = 3;
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
            laneFollowingDriver(0),
            m_timestamp(0),
            previousError(0.0) {
        //Create lane driver
        laneFollowingDriver = new LaneFollowingDriver(argc, argv);
        // Init laneFollowingDriver module
        laneFollowingDriver->runModule();
        driving_state = DRIVE;
        parking_state = BACKWARDS_RIGHT;
        overallDistance = 0;
        initialDistance = 0;
        gapWidth = 0;
        minParkingDistance = 0;
        forwardDistance = 0;
        gyroBackValue = 0;
        ultrasonicStraight = 0;
        countStop = 0;
        currentDistanceSpot = 0;
        distanceEndObstacle = 0;
        infraredSideValue = 0;
        brakeLights = false;
        flashingLightsRight = false;
        flashingLightsLeft = false;
        ultrasonicStrCheck = 0;
    }

    ParkingDriver::~ParkingDriver() {
        free(laneFollowingDriver);
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
        infrared_SideFront = (int) sbd.getValueForKey_MapOfDistances(1); // Side Left IR // *on legendary is //theFront-Side-Right
        infrared_SideBack = (int) sbd.getValueForKey_MapOfDistances(2); // Rear Left IR
        infrared_BackRight = (int) sbd.getValueForKey_MapOfDistances(3); // Rear Right IR
        infrared_BackLeft = (int) sbd.getValueForKey_MapOfDistances(5); // Side Right IR
        ultrasonic_Front = (int) sbd.getValueForKey_MapOfDistances(0); // Front UltraSonic
        ultrasonic_Rear = (int) sbd.getValueForKey_MapOfDistances(4); // Rear UltraSonic
        gyro = (int) sbd.getValueForKey_MapOfDistances(8); // Gyroscope
        //WheeelEncoder Data (mm)
        overallDistance = (int) ((sbd.getValueForKey_MapOfDistances(6) + sbd.getValueForKey_MapOfDistances(7)) / 2);
        // angle of the car
        parkAngle = (asin(abs(infrared_BackRight - infrared_BackLeft) / 11.5)) * 180 / 3.14; //11 = dist between IR's
        ActualSpeed = abs(overallDistance - initialDistance) / (sec * 100);
        initialDistance = overallDistance;



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
        cout << " ===== IR - SideFront: " << infrared_SideFront << " SideBack:" << infrared_SideBack << endl;
        cout << " ===== IR - BackLeft: " << infrared_BackLeft << " BackRight:" << infrared_BackRight << endl;
        cout << " ===== US: Front:" << ultrasonic_Front << " Rear:" << ultrasonic_Rear << endl;
        cout << " ===== WheelEncoder Data (mm): " << overallDistance << endl;
        cout << " ===== Desired speed: " << desiredSpeed << " Actual Speed: " << ActualSpeed << endl;
        cout << " ===== Desired steering: " << desiredSteering << endl;
        cout << " ===== Parking spot length: " << gapWidth << endl;
        cout << " ===== Parking Preparation length: " << forwardDistance << endl;
        cout << " ===== Gyro" << gyro << endl;
        cout << " ===== Park Angle :" << parkAngle << endl;
        cout << " ===== Gyro Backwards Angle :" << gyroBackValue << endl;
        cout << " ===== IR Side Value :" << infaredSideValue << endl;
        cout << " ===== US Straight Value :" << ultrasonicStrCheck << "  Number Of Counts" << countStop << endl;
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
                laneFollowingDriver->Routine();
                desiredSteering = steeringFactor * laneFollowingDriver->desiredSteering;
                desiredSpeed = speedF1; //SpeedF2;

                if ((ultrasonic_Front < safeDistance && ultrasonic_Front > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;
                }
                if ((infrared_SideFront < 25 && infrared_SideFront > 2)) {
                    driving_state = START_OBST;
                }
            }
                break;

            case START_OBST: {
                cout << "\t \t START_OBST mode" << endl;
                desiredSpeed = speedF1; //SpeedF2;
                laneFollowingDriver->Routine();
                desiredSteering = steeringFactor * laneFollowingDriver->desiredSteering;
                if ((ultrasonic_Front < safeDistance && ultrasonic_Front > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;

                }


                if (infrared_SideFront < 25 && infrared_SideFront > 0) {
                    foundBox = 1;
                }
                if (foundBox == 1) {
                    if ((infrared_SideFront > 25 || infrared_SideFront < 2)) {
                        driving_state = POSSIBLE_SPOT;
                        currentDistanceSpot = overallDistance;
                        foundBox = 0;
                    }
                }
            }
                break;

            case POSSIBLE_SPOT: {
                cout << "\t POSSIBLE_SPOT" << endl;;
                laneFollowingDriver->Routine();
                desiredSteering = steeringFactor * laneFollowingDriver->desiredSteering;

                // Detection of end of lane
                if ((ultrasonic_Front < safeDistance && ultrasonic_Front > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;
                }


                if (infrared_SideFront < 25 && infrared_SideFront > 0) {
                    infraredSideValue = infrared_SideFront;

                    gapWidth = (overallDistance - currentDistanceSpot);
                    if (gapWidth > firstMinParkingDistance && gapWidth <= secondMinParkingDistance) {
                        desiredSpeed = speedF1;
                        distanceEndObstacle = overallDistance;
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
                cout << "\t overallDistance: " << overallDistance << endl;
                cout << "\t distanceEndObstacle: " << distanceEndObstacle << endl;
                cout << "\t desiredDistance: " << desiredDistance << endl;

                laneFollowingDriver->Routine();
                desiredSteering = steeringFactor * laneFollowingDriver->desiredSteering;

                if (overallDistance <= distanceEndObstacle + desiredDistance) {
                    desiredSpeed = speedF2; //SpeedF2;
                }
                else {
                    flashingLightsRight = true;
                    brakeLights = true;
                    desiredSpeed = stop_Speed_Forward;

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
                stopParkingDistance = overallDistance;
                forwardDistance = overallDistance - distanceEndObstacle;
                driving_state = PARKING;
                desiredSpeed = 0.0;
                countStop = 0;
// For 55 cm Gap
                if (gapWidth > minParkingDistance && gapWidth <= firstMinParkingDistance) {
                    gyroBackValue = firstGyroBackRight;
                    countStop = secondCountStop;
                }
// For 63 cm Gap
                else if (gapWidth > firstMinParkingDistance && gapWidth <= secondMinParkingDistance) {
                    gyroBackValue = secondGyroBackRight;
                    ultrasonicStraight = firstUltrasonicStraight;
                    countStop = firstCountStop;
                }
// For 70 cm Gap
                else if (gapWidth > secondMinParkingDistance) {
                    gyroBackValue = thirdGyroBackRight;
                    countStop = secondCountStop;
                    ultrasonicStraight = secondUltrasonicStraight;
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
                if (overallDistance - stopParkingDistance <= 45) {
                    cout << "========  BACKWARDS_RIGHT" << endl;
                    cout << "========  overallDistance " << overallDistance << endl;
                    flashingLightsRight = true;
                    desiredSpeed = speedParkingBack;
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

                if (((ultrasonic_Front > 0) && (ultrasonic_Rear > 0) && ((Ultrasonic_Front + ultrasonic_Rear) <= ultrasonicStraight)) || countStop > 1) {
                    ultrasonicStrCheck = (ultrasonic_Front + ultrasonic_Rear);
                    parking_state = STOP;
                    desiredSpeed = stop_Speed_Backward;
                    brakeLights = true;
                }

                else if ((infrared_BackLeft < BackStopLimit && infrared_BackLeft > 0)
                         || (infrared_BackRight < BackStopLimit && infrared_BackRight > 0)
                         || (ultrasonic_Rear < BackStopLimit && ultrasonic_Rear > 0)) {
                    parking_state = FORWARD_RIGHT;
                    desiredSpeed = stop_Speed_Backward;
                    brakeLights = true;
                }
                else{
                    desiredSpeed = speedParkingBack;
                    desiredSteering = -maxSteeringPositive;
                    brakeLights = false;
                }
            }
                break;

            case FORWARD_RIGHT: {
                flashingLightsRight = false;
                cout << "\t\t========  FORWARD_RIGHT" << endl;
                int FrontStopLimit = 14;

                if (((ultrasonic_Front > 0) && (ultrasonic_Rear > 0) && ((ultrasonic_Front + ultrasonic_Rear) <= ultrasonicStraight)) || countStop > 1) {
                    ultrasonicStrCheck = (ultrasonic_Front + ultrasonic_Rear);
                    desiredSpeed = stop_Speed_Backward;
                    parking_state = STOP;
                    brakeLights = true;
                }
//                else if (ultrasonic_Front < FrontStopLimit && ultrasonic_Front > 0)  {
		else if ((ultrasonic_Front < FrontStopLimit && ultrasonic_Front > 0) || (infrared_BackLeft>17 || infrared_BackLeft == 0)) {
                    desiredSpeed = stop_Speed_Forward;
                    parking_state = BACKWARDS_LEFT;
                    brakeLights = true;
                    countStop++;
                }
                else{
                    brakeLights = false;
                    desiredSpeed = speedParkingForward;
                    desiredSteering = maxSteeringPositive;
                }
            }
                break;


            case BACK_AGAIN:
                cout << "\t\t========  BACKWARDS_STRAIGHT" << endl;
                desiredSpeed = speedParkingBack;
                desiredSteering = 0;
//		if(Distance > CurrentDist2 + DesiredDistance4){
//                if (ultrasonic_Rear < UVStopValue && ultrasonic_Rear > 0)  {
                if (infrared_BackLeft < infraredStopValue && infrared_BackLeft > 0) {
                    desiredSpeed = 0;
                    //backStraight = overallDistance - CurrentDist2;
                    desiredSteering = 0;
                    parking_state = FORWARD_RIGHT;  // BACKWARDS_LEFT
                }
                break;

            case STOP: {
                if ((ultrasonic_Rear > ultrasonicCheck && ultrasonic_Rear > 0) && (ultrasonic_Front > ultrasonicCheck && ultrasonic_Front > 0)) {
                    desiredSteering = 0;
                    desiredSpeed = 0.0;
                    brakeLights = true;

                    flashingLightsRight = true;
                    flashingLightsLeft = true;
                    parking_state = DONE;
                } else if (ultrasonic_Front < ultrasonicCheck && ultrasonic_Front > 0) {
                    desiredSteering = 0;
                    desiredSpeed = SpeedB3;
                    parking_state = STOP;
                }
                else if (ultrasonic_Rear < ultrasonicCheck && ultrasonic_Rear > 0) {
                    desiredSteering = 0;
                    desiredSpeed = speedF1; //SpeedF3;
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


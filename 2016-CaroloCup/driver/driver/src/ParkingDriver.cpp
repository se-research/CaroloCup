
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

//   int MinParkingDist = 54;
    int MinParkingDist = 47;
    int SafeDistance = 30;
    int Distance;
    int CurrentDistSpot;
//  int DesiredDistance1 = 103;
    int DesiredDistance1 = 107;
//    int DesiredDistance2 = 55;
//    int DesiredDistance3 = 80;
    int DesiredDistance4 = 50;
// 18  25 28 5
    int DesiredDistance5 = 13;
    int DesiredDistance6 = 23;
// 28
//    float SpeedF1 = 0.5;
//    float SpeedF2 = 0.5;
//    float SpeedF3 = 0.5;
//    float SpeedB1 = -0.2;
//    float SpeedB2 = -0.6;
    float SpeedF1 = 1;
    float SpeedF2 = 1;
    float SpeedF3 = 1;	// 0.9
    float SpeedB1 = -1.4;// -1.2
    float SpeedB2 = -1.4;// -1.4
    float Stop_Speed = 0.0;
    int CurrentDist;
    int CurrentDist1;
    int CurrentDist2;
    int CurrentDist3;
    int USFront;
    int USRear;
    int IRdis_SL;
    int IRdis_RL;
    int IRdis_RR;
    int IRdis_SR;
    int UVStopValue = 18;
    int IRStopValue = 12;
    int UVFrontValue = 23;
    int IRFrontValue = 17;
    double ParkAngle;
    int currentGyro;
    int gyroBackRight = 11; //7
//  25 18 10 20
    int count;
    int countStop=5;
    int gyroStopValue = 4;
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

    ParkingDriver::ParkingDriver(const int32_t &argc, char **argv) :
            DriverGeneric(argc, argv),
            driving_state(DRIVE),
            parking_state(BACKWARDS_RIGHT),
            laneDriver(0) {
        //Create lane driver
        laneDriver = new LaneFollowingDriver(argc, argv);
        // Init laneDriver module
        laneDriver->runModule();
        driving_state = DRIVE;
        parking_state = BACKWARDS_RIGHT;
        gapWidth = 0;
    }

    ParkingDriver::~ParkingDriver() {
        free(laneDriver);
    }

    void ParkingDriver::Initialize() {
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
        // angle of the car
        ParkAngle = (asin(abs(IRdis_RR-IRdis_SR)/11.0))*180/3.14; //11 = dist between IR's
        //Status
        cout << " ===== Rear IRs difference: " << abs(IRdis_SR - IRdis_RR) << endl;
        cout << " ===== Side Left Infrared reading: " << IRdis_SL << endl;
        cout << " ===== Rear Left Infrared reading: " << IRdis_RL << endl;
        cout << " ===== Rear Right Infrared reading: " << IRdis_RR << endl;
        cout << " ===== Side Right Infrared reading: " << IRdis_SR << endl;
        cout << " ===== Front UltraSonic reading: " << USFront << endl;
        cout << " ===== Rear UltraSonic reading: " << USRear << endl;
        cout << " ===== WheelEncoder Data (mm): " << Distance << endl;
        cout << " ===== Desired speed: " << desiredSpeed << endl;
        cout << " ===== Desired steering: " << desiredSteering << endl;
        cout << " ===== Parking spot length: " << gapWidth << endl;
        cout << " ===== Parking Preparation length: " << forwardDistance << endl;
        cout << " ===== Ptime_takenIndicator: " << time_takenIndicator << endl;
        cout << " ===== Gyroscope Angle " << gyro << endl;
        cout << " ===== Angle :" << ParkAngle << endl;
        cout << " ===== Backwards Right :" << backRight << endl;
        cout << " ===== Backwards Left :" << backLeft << endl;
        cout << " ===== Backwards Straight :" << backStraight << endl;
        cout << " ===== Backwards Stop Value :" << backStopValue << endl;
//	cout << " ===== Front Stop Value :" << frontStopValue << endl;
//      cout << " ========  REAched" << (CurrentDist1 + DesiredDistance2) << endl;

        // State machines
        switch (driving_state) {
            case DRIVE: {
                foundBox = 0;
                cout << "\t In drive mode" << endl;
                // Run the lane driver
                laneDriver->Routine();
                desiredSteering = laneDriver->desiredSteering;
                desiredSpeed = SpeedF2;

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
                //		desiredSpeed = SpeedF1;
                //            	laneDriver->Routine();
                desiredSteering = laneDriver->desiredSteering;
                if ((USFront < SafeDistance && USFront > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;

                }


                if (IRdis_SL < 25 && IRdis_SL > 0 ){
                    foundBox = 1;}
                if(foundBox == 1){
                    if ((IRdis_SL > 25 || IRdis_SL < 2)) {
                        driving_state = POSSIBLE_SPOT;
                        CurrentDistSpot = Distance;
                        foundBox=0;
                    }
                }
            }
                break;

            case POSSIBLE_SPOT: {
                cout << "\t POSSIBLE_SPOT" << endl;;
//		laneDriver->Routine();
//		desiredSteering = laneDriver->desiredSteering;
                if ((USFront < SafeDistance && USFront > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;

                }

                if (IRdis_SL < 25 && IRdis_SL > 0) {
                    gapWidth = (Distance - CurrentDistSpot);
                    if (gapWidth > MinParkingDist) {
//		laneDriver->Routine();
//		desiredSteering = laneDriver->desiredSteering;
                        desiredSpeed = SpeedF1;
                        CurrentDist = Distance;
                        driving_state = STOP_FOR_PARKING;
                        currentGyro = gyro;
                    } else {
                        driving_state = DRIVE;
                    }
                }

                cout << "\t Parking spot length: " << gapWidth << endl;
            }
                break;

            case STOP_FOR_PARKING: {
                flashingLightsRight = true;
                cout << "\t STOP_FOR_PARKING" << endl;
//              cout << "++++++++++ Stoping timer: " << time_taken << endl;
//		laneDriver->Routine();
                desiredSteering = laneDriver->desiredSteering;
                if ((USFront < SafeDistance && USFront > 2)) {
                    driving_state = NO_POSSIBLE_PARKING_PLACE;
                }

                if (Distance > (CurrentDist + (DesiredDistance1 - gapWidth))) {
                    forwardDistance = Distance - CurrentDist;
                    CurrentDist1 = Distance;
                    driving_state = PARKING;
                    desiredSpeed = Stop_Speed;
//		    desiredSteering = 0;
                    count = 0;
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
                desiredSteering = (42*3.14)/180;
                cout << "========  BACKWARDS_RIGHT" << endl;
//               if (Distance > (CurrentDist1 + DesiredDistance2)) {
                if(gyro < (currentGyro-gyroBackRight)){
//	         if(Distance > (currentDist1 - tan(gyro*3.14/180)*55)){
                    backRight = Distance - CurrentDist1;
                    parking_state = BACK_AGAIN;
                    CurrentDist2 = Distance;
                }
            }
                break;

            case BACKWARDS_LEFT: {

                desiredSpeed = SpeedB1;
                desiredSteering = -(42*3.14)/180;
                cout << "\t========  BACKWARDS_LEFT" << endl;
//		if (USRear < UVStopValue && USRear > 0)  {
                if (IRdis_SR < IRStopValue && IRdis_SR > 0)  {
//		if(gyro<currentGyro+gyroStopValue && gyro>currentGyro-gyroStopValue){
//			parking_state = DONE;
//			desiredSpeed = Stop_Speed;
//		}else{
                    parking_state = FORWARD_RIGHT;
                    desiredSpeed = Stop_Speed;
//                    }
                }
            }
                break;

            case FORWARD_RIGHT: {
                desiredSpeed = SpeedF3;
                desiredSteering = 42*3.14/180;
                cout << "\t\t========  FORWARD_RIGHT" << endl;
//		if((USRear > UVFrontValue || USRear == 0) || (USFront < UVStopValue && USFront > 0)){
                if((IRdis_SR > IRFrontValue || IRdis_SR == 0) || (USFront < UVStopValue && USFront > 0)){
//		if((USRear == 0) || (USFront < UVStopValue && USFront > 0)){
//		if(gyro==0){
                    if(count>countStop){
                        parking_state = DONE;
                        desiredSpeed = Stop_Speed;
                    }else{  parking_state = BACKWARDS_LEFT;
                        desiredSpeed = Stop_Speed;
                        count = count + 1;
                    }

                }
            }

                break;

            case BACK_AGAIN: {
                cout << "\t\t========  BACKWARDS_STRAIGHT" << endl;
                flashingLightsRight = false;
                desiredSpeed = SpeedB1;
                desiredSteering = 0;
//		if(Distance > CurrentDist2 + DesiredDistance4){
                if (USRear < UVStopValue && USRear > 0)  {
                    desiredSpeed = 0;
                    backStraight = Distance - CurrentDist2;
                    desiredSteering = -42*3.14/180;
                    parking_state = FORWARD_RIGHT;  // BACKWARDS_LEFT
                }
            }

                break;

            case STOP: {
                desiredSpeed = SpeedB2;
                desiredSteering = -(42*3.14)/180;
                if (USRear < UVStopValue && USRear > 0)  {
                    if(gyro<gyroStopValue && gyro>-gyroStopValue){
                        parking_state = DONE;
                        desiredSpeed = Stop_Speed;
                    }else{  parking_state = FORWARD_RIGHT;
                        desiredSpeed = Stop_Speed; }

                }
            }
                break;

            case DONE: {
//                flashingLightsRight = false;
//                flashingLightsLeft = false;
                cout << "\t\t========  DONE" << endl;
                backStopValue = gyro;
                desiredSpeed = Stop_Speed;
                desiredSteering = 0;
                parking_state = DONE;
            }

                break;

            default: {

                cout << "Non of these states" << endl;
            }
        }
    }

} // msv
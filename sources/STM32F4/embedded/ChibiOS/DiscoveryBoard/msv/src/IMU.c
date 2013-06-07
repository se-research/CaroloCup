/*
 * IMU.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "math.h"

#include "ch.h"
#include "hal.h"
#include "tm.h"

#include "msv/include/IMU.h"
#include "msv/include/RAZOR.h"

#define ORIENTATION_X_WEIGHT_COMPLEMENT (1.0 - ORIENTATION_X_WEIGHT)
#define ORIENTATION_Y_WEIGHT_COMPLEMENT (1.0 - ORIENTATION_Y_WEIGHT)
#define ORIENTATION_Z_WEIGHT_COMPLEMENT (1.0 - ORIENTATION_Z_WEIGHT)

static unsigned int firstCallCalculateOrientation; // Internal state machine.
static unsigned int firstCallUpdatePosition; // Internal state machine.

static TimeMeasurement orientationTimer; // Data structure for time measurements for the orientation calculations.
static TimeMeasurement speedTimer; // Data structure for time measurements for the position calculations.


/**
 * This internal method updates the position.
 * The results will be available in positionX and positionY.
 *
 * @param speedPtr Pointer to the current speed value.
 * @param dT Time that has passed in milliseconds.
 */
void updatePosition_dT(const int *speedPtr, float dT) {
 //   chMtxLock(&vehicleDataMutex);
    {
        const float cosZ = cos(orientationZ);
        const float sinZ = sin(orientationZ);

        const float speed_t = (*speedPtr) * (dT/1000.0);

        const float deltaX = cosZ * speed_t;
        const float deltaY = sinZ * speed_t;

        positionX += deltaX;
        positionY += deltaY;

        relTravelledPath = sqrt(deltaX*deltaX + deltaY*deltaY);
        absTravelledPath += relTravelledPath;

        velocityX = cosZ * (*speedPtr);
        velocityY = sinZ * (*speedPtr);
    }
 //   chMtxUnlock();
}


void updatePosition(const int *speedPtr) {
    if (firstCallUpdatePosition == 0) {
        tmStopMeasurement(&speedTimer);

        // Calculate the time that has passed since last call.
        float dT = RTT2MS(speedTimer.last);

        updatePosition_dT(speedPtr, dT);

        tmStartMeasurement(&speedTimer);
    }
    if (firstCallUpdatePosition == 1) {
        tmStartMeasurement(&speedTimer);
        firstCallUpdatePosition = 0;
    }
}


/**
 * This internal method normalizes the angle to the range 0..360 deg.
 *
 * @param anglePtr Pointer to the angle for normalizing.
 */
void normalizeAngleDEG(float *anglePtr) {
    if (anglePtr != NULL) {
        while (*anglePtr < 0) {
            *anglePtr += 360;
        }
        while (*anglePtr > 359) {
            *anglePtr -= 360;
        }
    }
}


/**
 * This internal method calculates the orientation using complementary filters.
 * The results will be available in orientationX, orientationY and orientationZ.
 *
 * @param razorDataPtr Pointer to the filled razor data structure.
 * @param dT Time that has passed in milliseconds.
 */
void calculateOrientation_complementaryFilters(const int *razorDataPtr, float dT) {
    chMtxLock(&razorDataMutex);
    {
        const int *gyroXrate = (const int*)razorDataPtr[6];
        const int *gyroYrate = (const int*)razorDataPtr[7];
        const int *gyroZrate = (const int*)razorDataPtr[8];
          
        const int *accXangle = (const int*)razorDataPtr[9];
        const int *accYangle = (const int*)razorDataPtr[10];
        const int *accZangle = (const int*)razorDataPtr[11];

        // Using a complementary filter to improve the output (cf. https://sites.google.com/site/myimuestimationexperience/filters/complementary-filter)
        orientationX = (ORIENTATION_X_WEIGHT * (orientationX + (*gyroXrate * dT / 1000.0))) + (ORIENTATION_X_WEIGHT_COMPLEMENT * (*accXangle));
        orientationY = (ORIENTATION_Y_WEIGHT * (orientationY + (*gyroYrate * dT / 1000.0))) + (ORIENTATION_Y_WEIGHT_COMPLEMENT * (*accYangle));
        orientationZ = (ORIENTATION_Z_WEIGHT * (orientationZ + (*gyroZrate * dT / 1000.0))) + (ORIENTATION_Z_WEIGHT_COMPLEMENT * (*accZangle));

        // Restrict orientation to 0..360 degree:
       
        normalizeAngleDEG(&orientationX);
        normalizeAngleDEG(&orientationY);
        normalizeAngleDEG(&orientationZ);
    }
    chMtxUnlock();

   // chMtxLock(&vehicleDataMutex);
  //  {
        orientationX_rad = orientationX;
        orientationY_rad = orientationY;
        orientationZ_rad = orientationZ;
  //  }
  //  chMtxUnlock();
}


void calculateOrientation(const int *razorDataPtr) {
    if (firstCallCalculateOrientation == 0) {
        tmStopMeasurement(&orientationTimer); 

        // Calculate the time that has passed since last call.
        float dT = RTT2MS(orientationTimer.last);

        calculateOrientation_complementaryFilters(razorDataPtr, dT);

        tmStartMeasurement(&orientationTimer);
    }
    if (firstCallCalculateOrientation == 1) {
        tmStartMeasurement(&orientationTimer);
        firstCallCalculateOrientation = 0;
    }
}

static WORKING_AREA(imoThreadArea, 128);
static msg_t imoThread(void *arg) {
  //int* razorInfo = {0,0,0,0,0,0,0,0,0,0,0,0};
  int speed = 1;
  (void)arg;
  chRegSetThreadName("imuThread");
  // Reader thread loop.
  while (TRUE) {
    
    calculateOrientation(razorData);
    chThdSleepMilliseconds(10);
    updatePosition(&speed);
    //sleep for a while
    chThdSleepMilliseconds(10);
  }
  return (msg_t)0;
}

void initIMU() {
    firstCallCalculateOrientation = 1;
    firstCallUpdatePosition = 1;

    orientationX = 0;
    orientationY = 0;
    orientationZ = 0;

    orientationX_rad = 0;
    orientationY_rad = 0;
    orientationZ_rad = 0;

    positionX = 0;
    positionY = 0;

    relTravelledPath = 0;
    absTravelledPath = 0;

    velocityX = 0;
    velocityY = 0;

    chMtxInit(&razorDataMutex);
    chMtxInit(&vehicleDataMutex);

    tmObjectInit(&orientationTimer);
    tmObjectInit(&speedTimer);
    chThdCreateStatic(imoThreadArea, sizeof(imoThreadArea),NORMALPRIO + 10, imoThread, NULL);
}

void getImuValues(float values[7]){
   values[0] = orientationZ;
   values[1] = positionX;
   values[2] = positionY;
   values[3] = relTravelledPath;
   values[4] = absTravelledPath;
   values[5] = velocityX;
   values[6] = velocityY;

}

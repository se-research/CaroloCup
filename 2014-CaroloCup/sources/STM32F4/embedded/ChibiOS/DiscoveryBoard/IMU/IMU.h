/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef IMU_H_INCLUDED
#define IMU_H_INCLUDED

#define PI 3.141592654
#define DEG2RAD (PI / 180.0)
#define RAD2DEG (1.0 / DEG2RAD)

#define ORIENTATION_X_WEIGHT 0.98
#define ORIENTATION_Y_WEIGHT 0.98
#define ORIENTATION_Z_WEIGHT 0.98

/*
Byte  0: yaw rate
Byte  1: pitch rate
Byte  2: roll rate
Byte  3: compass X
Byte  4: compass Y
Byte  5: compass Z
Byte  6: gyro X
Byte  7: gyro Y
Byte  8: gyro Z
Byte  9: acc X
Byte 10: acc Y
Byte 11: acc Z
*/
//static int razorDataExample[12]; // This should be the data from the Razor interface.
static Mutex razorDataMutex; // The data from the Razor interface must be protected whenever the IMU is accessing it. This mutex should be in the Razor interface definition.

static float orientationX; // Output of the filtered orientation around X-axis in degree.
static float orientationY; // Output of the filtered orientation around Y-axis in degree.
static float orientationZ; // Output of the filtered orientation around Z-axis in degree.

static Mutex vehicleDataMutex;

// The following data will be used to update VehicleData data structure.
static float orientationX_rad; // Output of the filtered orientation around X-axis in radians.
static float orientationY_rad; // Output of the filtered orientation around Y-axis in radians.
static float orientationZ_rad; // Output of the filtered orientation around Z-axis in radians.

static float positionX; // Estimated position X.
static float positionY; // Estimated position Y.

static float relTravelledPath; // Estimated length of relatively travelled path.
static float absTravelledPath; // Estimated length of absolutely travelled path.

static float velocityX; // Estimated velocity X.
static float velocityY; // Estimated velocity Y.

/**
 * This method initalizes this IMU algorithm.
 */
void initIMU(void);

/**
 * This method calculates the orientation (actually a wrapper).
 * The results will be available in orientationX, orientationY and orientationZ.
 *
 * @param razorDataPtr Pointer to the filled razor data structure.
 */
void calculateOrientation(const int *razorDataPtr);

/**
 * This method updates the estimated position (actually a wrapper).
 * This method should be called very frequently (200Hz) to improve the position estimation.
 *
 * The results will be available in orientationX, orientationY and orientationZ.
 *
 * @param speedPtr Pointer to the current speed in m/s.
 */
void updatePosition(const int *speedPtr);

#endif // IMU_H_INCLUDED

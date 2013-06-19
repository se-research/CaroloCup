/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuation.
///////////////////////////////////////////////////////////////////////////////

static Thread *ThreadAcceleration = NULL;
static Thread *ThreadSteering = NULL;

#define CENTER_STEERING 1500 // Depends on the concrete vehicle.
#define NO_ACCELERATION 1478 // Pulse width for no acceleration.
#define ACCELERATION_PULSE 1485 // If the battery has more power just increase the pulse variable.

static int steeringServo = 0; // 1265 mapping require 
static int accelerationMotor = 0; // min 1490 max over 2000 for forward movement.

// Configuration for the PWM output.
static PWMConfig pwmConfiguration = {
    1000000, // 1MHz PWM clock frequency.
    20000, // PWM period of 20 milliseconds.
    NULL, // No callback are used.
    {
        {PWM_OUTPUT_ACTIVE_HIGH, NULL},
        {PWM_OUTPUT_ACTIVE_HIGH, NULL},
        {PWM_OUTPUT_ACTIVE_HIGH, NULL},
        {PWM_OUTPUT_ACTIVE_HIGH, NULL}
    },
    0
};

// Prototypical definitions.
int convertMotorData(int speed);

///////////////////////////////////////////////////////////////////////////////
// Interface methods.
///////////////////////////////////////////////////////////////////////////////

Thread* getThreadAcceleration(void) {
    return ThreadAcceleration;
}

Thread* getThreadSteering(void) {
    return ThreadSteering;
}

void setMotorData(int steering, int speed) {
    // steeringServo = steer;
    steeringServo = CENTER_STEERING + (steering * 11.75);

    // Call method to convert.
    accelerationMotor = speed; //convertMotorData(speed);	
}

void commandControlSteeringAccelerationMotors(BaseSequentialStream *chp, int argc, char *argv[]) {
    if (argc == 2) {
        int steering = atoi(argv[0]);
        int speed = atoi(argv[1]);

        chprintf(chp, "Steering: %d, speed: %d\r\n", steering, speed);

        setMotorData(steering, speed);
    }
    else {
        chprintf(chp, "To control the servo and acceleration motor interactively, invoke this\r\n");
        chprintf(chp, "method like this: csam steering acceleration (for example csam 5 1).\r\n");
    }
}

///////////////////////////////////////////////////////////////////////////////
// Actor accessing methods.
///////////////////////////////////////////////////////////////////////////////

/**
 * This method translates the data to a pulse for acceleration.
 * TODO: Implementation of a PID controller based on feedback from a wheel encoder is required.
 *
 * @param speed Desired speed.
 */
int convertMotorData(int speed) {
    int new_speed = 0;

    // because of the problems with the voltage I created the variable pulse. It work
    // as a constant to adjust the other limits of the signals I sent. This value works for around 7,2 v at thr battery

    if (31 > speed && speed > -31) {
        if (speed <= -1) {
            // BACKWARDS:
            int old_min = -30;  
            int old_max = -1;  
            int new_min = ACCELERATION_PULSE - 100;
            int new_max = ACCELERATION_PULSE - 10;

            new_speed = (((speed - old_min) * (new_max - new_min)) / (old_max - old_min)) + new_min;

            accelerationMotor = new_speed;
        }
        else if (speed >= 1) {
            // FORWARD:
            int old_min = 1; 
            int old_max = 30;  
            int new_min = ACCELERATION_PULSE + 5;
            int new_max = ACCELERATION_PULSE + 100;

            new_speed = (((speed - old_min) * (new_max - new_min)) / (old_max - old_min)) + new_min;

            accelerationMotor = new_speed;
        }
        else {
            // STRAIGHT:
            accelerationMotor = NO_ACCELERATION;
        }
    }
    else {
        accelerationMotor = NO_ACCELERATION;
    }

    // TODO: Complete this conversion method first. For now, NO_ACCELERATION.
    return NO_ACCELERATION;
}

// Thread for interfacing with the motor.
static WORKING_AREA(workingAreaThread_Acceleration, 512);
static msg_t Thread_Acceleration(void *arg) {
    (void)arg;
    chRegSetThreadName("Acceleration");

    waitForCompletingInitialization();

    while (TRUE) {
        pwmEnableChannel(&PWMD3, 2, accelerationMotor);
        chThdSleepMilliseconds(10);  
    }

    return (msg_t)0;
}

// Thread for interfacing with the steering servo.
static WORKING_AREA(workingAreaThread_Steering, 512);
static msg_t Thread_Steering(void *arg) {
    (void)arg;
    chRegSetThreadName("Steering");

    waitForCompletingInitialization();

    while (TRUE) {
        pwmEnableChannel(&PWMD3, 3, steeringServo);
        chThdSleepMilliseconds(10);
    }

    return (msg_t)0;
}

///////////////////////////////////////////////////////////////////////////////
// Initialization method.
///////////////////////////////////////////////////////////////////////////////

void initializeSteeringAcceleration(void) {
	// ESC for the acceleration motor is connected to pin PC8.	
    palSetPadMode(GPIOC, 8, PAL_MODE_ALTERNATE(2));

	// Steering servo for steering is connected to pin PC9.	
    palSetPadMode(GPIOC, 9, PAL_MODE_ALTERNATE(2)); //650

    // Initializes the PWM driver 3 in order to control the actors.
    pwmStart(&PWMD3, &pwmConfiguration);

    // Enable the connection to the acceleration motor.
    pwmEnableChannel(&PWMD3, 2, NO_ACCELERATION);

    // Enable the connection to the steering servo.
    pwmEnableChannel(&PWMD3, 3, CENTER_STEERING);

    // Wait until the motors are enabled.
    chThdSleepMilliseconds(100);

    // Starting threads.
    ThreadAcceleration = chThdCreateStatic(workingAreaThread_Acceleration,
                                           sizeof(workingAreaThread_Acceleration),
                                           NORMALPRIO + 15, Thread_Acceleration, NULL);
    ThreadSteering = chThdCreateStatic(workingAreaThread_Steering,
                                       sizeof(workingAreaThread_Steering),
                                       NORMALPRIO + 15, Thread_Steering, NULL);
}  


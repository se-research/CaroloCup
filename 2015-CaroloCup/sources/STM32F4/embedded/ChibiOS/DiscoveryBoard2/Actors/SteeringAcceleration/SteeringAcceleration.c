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

#define CENTER_STEERING 1450 // Depends on the concrete vehicle.
#define NO_ACCELERATION 1500 // Pulse width for no acceleration.
#define MAX_POSITIVE_ACCELERATION 1900 // Pulse width for max negative acceleration (to be experimentally determined, for example 800 for max negative acceleration, 1900 for max positive acceleration).
#define MAX_NEGATIVE_ACCELERATION 800 // Pulse width for max negative acceleration (to be experimentally determined, for example 800 for max negative acceleration, 1900 for max positive acceleration).
#define TIM3_CHANNEL3 2
#define TIM3_CHANNEL4 3

static int steeringServo = 0; // 1265 mapping required 
static int desiredSpeed = NO_ACCELERATION;

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
    0,
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
    // (void)speed;
    // Map desired steering angle to PWM value.
    steeringServo = CENTER_STEERING + (steering * 11.75);

    // To do: Identify correct PWM values for acceleration forwards/backwards.
    // desiredSpeed = speed;

    // For now: explicitly NO_ACCELERATION! Do not change until you know what you are doing!
    // Furthermore, make sure the car is put on a stack of books to avoid damages to yourself and others!
    
    //Step-wose increasing or decreasing the PWM 
    if (speed > desiredSpeed) {        
        for(; desiredSpeed < speed; desiredSpeed++) {
             chThdSleepMilliseconds(11);
        }
    }
    if (speed < desiredSpeed) {
        for(; desiredSpeed > speed; desiredSpeed--) {  
            chThdSleepMilliseconds(11);
        }
    }        
    else { 
        desiredSpeed = speed;
    }
}

void commandControlSteeringAccelerationMotors(BaseSequentialStream *chp, int argc, char *argv[]) {
    if (argc == 2) {
        int steering = atoi(argv[0]);
        int speed = atoi(argv[1]);

        //Correct the etered value for the motor if in it not in the correct range
        if (speed < MAX_NEGATIVE_ACCELERATION) { speed = MAX_NEGATIVE_ACCELERATION; }
        if (speed > MAX_POSITIVE_ACCELERATION) { speed = MAX_POSITIVE_ACCELERATION; }
        
        chprintf(chp, "Steering: %d, speed: %d\r\n", steering, speed);

        setMotorData(steering, speed);

        chprintf(chp, "%d \r\n", desiredSpeed);

    }
    else {
        chprintf(chp, "To control the servo and acceleration motor interactively, invoke this\r\n");
        chprintf(chp, "method like this: csam steering acceleration (for example csam 5 1).\r\n");
    }
}

///////////////////////////////////////////////////////////////////////////////
// Actor accessing methods.
///////////////////////////////////////////////////////////////////////////////

// Thread for interfacing with the motor.
static WORKING_AREA(workingAreaThread_Acceleration, 512);
static msg_t Thread_Acceleration(void *arg) {
    (void)arg;
    chRegSetThreadName("Acceleration");

    waitForCompletingInitialization();

    while (TRUE) {

        pwmEnableChannel(&PWMD3, TIM3_CHANNEL3, desiredSpeed);
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
   
        pwmEnableChannel(&PWMD3, TIM3_CHANNEL4, steeringServo); //steeringServo);
        chThdSleepMilliseconds(10);

    }

    return (msg_t)0;
}


///////////////////////////////////////////////////////////////////////////////
// Initialization method.
///////////////////////////////////////////////////////////////////////////////

void initializeSteeringAcceleration(void) {
	// ESC for the acceleration motor is connected to pin PC8.	
    palSetPadMode(GPIOC, 8, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM3));

	// Steering servo for steering is connected to pin PC9.	
    palSetPadMode(GPIOC, 9, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_TIM3)); //650

    // Initialize the PWM driver 3 in order to control the actors.
    pwmStart(&PWMD3, &pwmConfiguration);

    // Enable the connection to the acceleration motor.
    pwmEnableChannel(&PWMD3, TIM3_CHANNEL3, NO_ACCELERATION);

    // Enable the connection to the steering servo.
    pwmEnableChannel(&PWMD3, TIM3_CHANNEL4, CENTER_STEERING);

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


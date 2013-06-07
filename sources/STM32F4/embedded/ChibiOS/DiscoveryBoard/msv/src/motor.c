#include "stdlib.h"
#include "ch.h"
#include "hal.h"
#include "msv/include/motor.h"

#define defaultSteering 1500 

int steeringMotor; //1265 mapping require 
int speedMotor; // min 1490 max over 2000 for forward movement.
int drive_pulse = 1400; //initalizing width of the pulse

//hardware config

static PWMConfig pwmcfg = {
  1000000, /* 1MHz PWM clock frequency */
  20000, /* PWM period 20 milli second */
  NULL, /* No callback */
  /* Only channel 3 enabled */
  {
    {PWM_OUTPUT_ACTIVE_HIGH, NULL},
    {PWM_OUTPUT_ACTIVE_HIGH, NULL},
    {PWM_OUTPUT_ACTIVE_HIGH, NULL},
    {PWM_OUTPUT_ACTIVE_HIGH, NULL}
  },
  0
};


// Thread for controlling motor
static WORKING_AREA(waThread3, 128);
static msg_t Thread3(void *arg) {
  (void)arg;

  chRegSetThreadName("motorThread");

  while (TRUE) {
    pwmEnableChannel(&PWMD3, 2, speedMotor);
    chThdSleepMilliseconds(10);  
  }
  return (msg_t)0;
}

// Thread for steering motor
static WORKING_AREA(waThread4, 128);
static msg_t Thread4(void *arg) {
  (void)arg;

  chRegSetThreadName("steeringThread");

  while (TRUE) {
    pwmEnableChannel(&PWMD3, 3, steeringMotor);
    chThdSleepMilliseconds(10);
  }
  return (msg_t)0;
}

void motorInit(void) {
	//Pin selection PC8 PC9	
  palSetPadMode(GPIOC, 8, PAL_MODE_ALTERNATE(2));
  palSetPadMode(GPIOC, 9, PAL_MODE_ALTERNATE(2));//650

  // hardware confg
  pwmStart(&PWMD3, &pwmcfg);
  //drive motor enable
  pwmEnableChannel(&PWMD3, 2, drive_pulse);
  pwmEnableChannel(&PWMD3, 3, defaultSteering);
  //wait until it enables drive motor
  chThdSleepMilliseconds(100);
  //start threed

  

  chThdCreateStatic(waThread3, sizeof(waThread3),NORMALPRIO + 10, Thread3, NULL);
  chThdCreateStatic(waThread4, sizeof(waThread4),NORMALPRIO + 10, Thread4, NULL);

}  

void setMotorData(int steer, int speed){

  // steeringMotor = steer;
  steeringMotor = defaultSteering + (steer * 11.75);

  //call method to convert
   speedMotor = speed;//convertMotorData(speed);	
}
//method for translating the data to pulse for moving the engine

int convertMotorData(int speed){
	
   //speed conf

   int new_speed;
   int no_speed = 1478;
   // because of the problems with the voltage I created the variable pulse. It work
   // as a constant to adjust the other limits of the signals I sent. This value works for around 7,2 v at thr battery
    // if the battery has more power just increase the pulse variable
   int pulse = 1485;

//	new_speed = (((speed - old_min) * (new_max - new_min)) / (old_max - old_min)) + new_min;
 	
//	speed = new_speed;/
//	return speed;

	if( 31 > speed && speed > -31){

	if (speed <= -1){
             // BACKWARDS;
                     int old_min = -30;  
                     int old_max = -1;  
                     int new_min = pulse - 100;
                     int new_max = pulse - 10;
		              new_speed= (((speed - old_min) * (new_max - new_min)) / (old_max - old_min)) + new_min;

	                        speedMotor = new_speed;
		
			return;
		}
	 else if (speed >= 1){
             // FORWARD;
                    int old_min = 1; 
                    int old_max = 30;  
	            int new_min = pulse + 5;
                    int new_max = pulse + 100;
			     new_speed = (((speed - old_min) * (new_max - new_min)) / (old_max - old_min)) + new_min;
 	
	                        speedMotor = new_speed;
		return;
		}
        else {
           // STRAIGH;
		speedMotor  = no_speed;
              return;	
	}

      }else {
		speedMotor  = no_speed;
              return;	
	}
	
}
int convertSteeringData(int steer){

  int old_min,old_max,new_min,new_max,new_steer,steeringMotor;
  int straigh = 1450;

	if( 27 > steer && steer >= -26){
  	
	if (steer <= -1){
             // LEFT;
                     int old_min = -26; // left
                     int old_max = -1;  //right
                     int new_min = 1800;
                     int new_max = 1400;
		               new_steer = (((steer - old_min) * (new_max - new_min)) / (old_max - old_min)) + new_min;

	                        steeringMotor = new_steer;
		
			return;
		}
	 else if (steer >= 1){
             // RIGHT;
                    int old_min = 1; // left
                    int old_max = 26;  //right
	            int new_min = 1500;
                    int new_max = 1200;
			     new_steer = (((steer - old_min) * (new_max - new_min)) / (old_max - old_min)) + new_min;
 	
	                        steeringMotor = new_steer;
		return;
		}
        else {
           // STRAIGH;
		steeringMotor  = straigh;
              return;	
	}
    } else {
		steeringMotor  = straigh;
              return;	
	}
}



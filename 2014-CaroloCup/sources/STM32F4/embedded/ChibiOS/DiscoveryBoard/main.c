#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "ch.h"
#include "hal.h"

#include "shell.h"
#include "chprintf.h"

#include "PWM/PWM.h"
#include "USB/USB.h"
#include "Misc/Misc.h"
#include "SPI/SPI.h"
#include "msv/include/RAZOR.h"
#include "msv/include/protocol_byte.h"
#include "msv/include/motor.h"
#include "msv/include/ultrasonic.h"
#include "msv/include/IMU.h"
#include "msv/include/ir.h"

int rcvData[3]={0,0,0}; 

const SerialConfig portConfig2 = {
    115000,
    0,
    USART_CR2_STOP1_BITS | USART_CR2_LINEN,
    USART_CR3_CTSE
};

void parse(char *str) {
  char tmp[4][4];
  int len =strlen(str);
  int x=0,y=0,i=0,j=0;
  //delete all in temp
  for (i=0;i<4;i++)
      for (j=0;j<4;j++)
          tmp[i][j]=' ';
  //pharse to array of string
  for (i=0;i<len;i++)
  {
    if (str[i]!=',')tmp[x][y]=str[i];
    y++;	  
    if (str[i]==','){
        tmp[x][y]='\0';
        x++;
	y=0; 
    }
  }
  //convert small string to array
  for (i=0;i<4;i++) rcvData[i]=atoi (tmp[i]);
}

/*
* Application entry point.
*/
int main(void) {
  int8_t accelData[2]={0,0};     	   // Discovery Board's Accelerometer
  uint8_t receivedBuff[4]={0,0,0,0}; 	   // Received request/information from PandaBoard
  uint8_t sentData[4] = {0,0,0,0}; // Returned Information (reply) to the PandaBoard
  float imuData[7]={0,0,0,0,0,0,0};        	   // IMU calculated data based on Razor Boad
  int* razorInfo;                  // Razor Board Data
  int steering = 0;
  int speed = 0;
  int ir_data[3]={0,0,0}; 
  int16_t us_data[3]={0,0,0};

  /*
   * System initializations.
   * - HAL initialization, this also initializes the configured device drivers
   *   and performs the board-specific initializations.
   * - Kernel initialization, the main() function becomes a thread and the
   *   RTOS is active.
   */
  halInit();
  chSysInit(); 
  
  mypwmInit();
   
 // Initializing Motor
  motorInit();

  // Initializing IR Thread
  ADCinit();

  // Initializing US Thread
 // myUltrasonicInit();
  
  // Initializing Discovery Board's Accelerometer
  //mySPIinit();

  // Initializing Razor Board
  myRazorInit();

  // Activates the USB driver and then the USB bus pull-up on D+.
  myUSBinit();

  // Initializing IMU Calculations. 
  initIMU();

  //Starting the usb configuration
  sdStart(&SDU1,&portConfig2);
  char receivedInfo[11];
 
  /*
   * Main loop, it takes care of reciving the requests from Panda Board using USB protocol,
   * and reply with the requested data.
   */
  while (TRUE) {
   receivedInfo[0]='T';
   sdRead(&SDU1, receivedInfo, 10);
   
  // getImuValues(imuData);
   //   getAccel(accelData);
  // getIR(ir_data);  
   //  getUS(us_data);

   if(receivedInfo[0] != 'T'){
	receivedInfo[11]='\0';
	parse(receivedInfo);
      
	//setMotorData(-(rcvData[1]-28),rcvData[2]-2);
        setMotorData(rcvData[1],1550);
	translate(rcvData[0],ir_data,us_data,razorInfo,imuData,accelData,sentData);
       	sdWrite(&SDU1, sentData, 4);
    }
  }   
}

#include "stdlib.h"
#include "ch.h"
#include "hal.h"
#include "string.h"
#include "chprintf.h"
#include "msv/include/RAZOR.h"
#include "msv/include/IMU.h"

char word[64]; //has to be here

const SerialConfig portConfig = {
    57600,
    0,
    USART_CR2_STOP1_BITS | USART_CR2_LINEN,
    USART_CR3_CTSE
};

void phrase(char *str) {
	char tmp[12][5];
	int len =strlen(str);
	int x=0,y=0,i=0,j=0;
	//delete all in temp
	for (i=0;i<12;i++)
		for (j=0;j<5;j++)
			tmp[i][j]=' ';
	//pharse to array of string
	for (i=0;i<len;i++)
	{
		if (str[i]!=',')tmp[x][y]=str[i];
		y++;	  
		if (str[i]==','){
			tmp[x][y]='\0';
			x++;
			y=0; }
		
	}
	//convert small string to array
	for (i=0;i<12;i++) razorData[i]=atoi (tmp[i]);
}
void cmd_printDataFromRazor(BaseSequentialStream *chp, int argc, char *argv[]) {
  (void)chp;
  (void)argc;
  (void)argv;
  chprintf(chp, "Angle data in degree");
  chprintf(chp, "\r\n");
  chprintf(chp, "yaw=%d,pitch=%d,roll=%d\n",razorData[0],razorData[1],razorData[2]);
  chprintf(chp, "\r\n");
  chprintf(chp, "Magnetometer(Compass) raw data (-250 - 250)");
  chprintf(chp, "\r\n");
  chprintf(chp, "X=%d,Y=%d,Z=%d\n",razorData[3],razorData[4],razorData[5]);
  chprintf(chp, "\r\n");
  chprintf(chp, "Gyro raw data (-250 - 250)");
  chprintf(chp, "\r\n");
  chprintf(chp, "X=%d,Y=%d,Z=%d\n",razorData[6],razorData[7],razorData[8]);
  chprintf(chp, "\r\n");
  chprintf(chp, "Accelerometer raw data (-250 - 250)");
  chprintf(chp, "\r\n");
  chprintf(chp, "X=%d,Y=%d,Z=%d",razorData[9],razorData[10],razorData[11]);
  chprintf(chp, "\r\n");  
}
static WORKING_AREA(waThread2, 128);
static msg_t Thread2(void *arg) {
  
  (void)arg;
  chRegSetThreadName("razorthreed");
  // Reader thread loop.
  while (TRUE) {
      	int j=0,i=0;
        int* speed; 
	uint8_t buf[64];
	int z=0,x;
	//empty word from junk
	strcpy(word,"");
	//empty bufer from junk
	for(i=0;i<64;i++) buf[i]=' ';
	//read serail port
	sdRead(&SD3, buf, 64);
	i=0;
        speed = 1;
	while(TRUE)
	{		
		//find begin of sentance send from razor board 		
		if (buf[i]=='|') z=i;
		//find end of sentance send from razor board 			
		if (buf[i]=='!'){
			x=i;
			//put in word real sentance from rezor board
			for(j=0;j<(x-z);j++) word[j]=buf[j+z+1];			
			//make word recognized as string			
			word[j-1]='\0';
			//send word to phrase mechanism
			phrase(word);
			break;
			
		}
		//in case if it is not a real sentance from razor board
		if (i >=63) break;
		i++;
	}
    //sleep for a while
    chThdSleepMilliseconds(10);
  }
  return (msg_t)0;
}

void myRazorInit(void) {
   
  //start serial mode
  sdStart(&SD3,&portConfig);
  //configure ports to use serial protocol
  palSetPadMode(GPIOD, 8, PAL_MODE_ALTERNATE(7));
  palSetPadMode(GPIOD, 9, PAL_MODE_ALTERNATE(7));
 //start thread in background
	chThdCreateStatic(waThread2, sizeof(waThread2),
                    NORMALPRIO + 10, Thread2, NULL);

}
int* getRazorValues(void) {
	return razorData;
}

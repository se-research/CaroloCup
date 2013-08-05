/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuation.
///////////////////////////////////////////////////////////////////////////////

static Thread *ThreadRazor9DoFIMU = NULL;

// Configuration of the Razor 9DoF IMU communication.
const SerialConfig portConfig = {
    57600,
    0,
    USART_CR2_STOP1_BITS | USART_CR2_LINEN,
    USART_CR3_CTSE
};

// Buffer for the data samples.
static int razorData[12] = {0,0,0,0,0,0,0,0,0,0,0,0};
static char word[64]; //has to be here
static uint8_t buf[64];

///////////////////////////////////////////////////////////////////////////////
// Interface methods.
///////////////////////////////////////////////////////////////////////////////

Thread* getThreadRazor9DoFIMU(void) {
    return ThreadRazor9DoFIMU;
}

void getRazor9DoFIMUData(int data[3]) {
    (void)data;
}

void commandPrintRazor9DoFIMU(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    chprintf(chp, "Reading 100 cycles:\r\n");
    int i = 100;
    while (i>0) {
        buf[0] = ' ';
        systime_t tmo = MS2ST(20);
        sdReadTimeout(&SD3, buf, 1, tmo);
        chprintf(chp, "%s", buf);
        i--;
    }
    chprintf(chp, "\r\nDone.\r\n");
}

void commandPrintRazor9DoFIMUORG(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;
    chprintf(chp, "buf: %s", buf);
    chprintf(chp, "word: %s", word);
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

///////////////////////////////////////////////////////////////////////////////
// Sensor reading methods.
///////////////////////////////////////////////////////////////////////////////

void phrase(char *str) {
    char tmp[12][5];
    int len =strlen(str);
    int x=0,y=0,i=0,j=0;
    //delete all in temp
    for (i=0;i<12;i++)
	    for (j=0;j<5;j++)
		    tmp[i][j]=' ';
    //pharse to array of string
    for (i=0;i<len;i++) {
	    if (str[i]!=',')tmp[x][y]=str[i];
	    y++;	  
	    if (str[i]==','){
		    tmp[x][y]='\0';
		    x++;
		    y=0;
        }
    }
    //convert small string to array
    for (i=0;i<12;i++) razorData[i]=atoi (tmp[i]);
}

void readRazor9DoFIMU(void) {
    int j=0,i=0;
//    int* speed; 
    int z=0,x;
    //empty word from junk
    strcpy(word,"");
    //empty bufer from junk
//    for(i=0;i<64;i++) buf[i]=' ';
    //read serail port
//    sdRead(&SD3, buf, 64);
    sdRead(&SD3, buf, 1);

    return;

    i=0;
//    speed = 1;
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
}

static WORKING_AREA(workingAreaThread_Razor9DoFIMU, 512);
static msg_t Thread_Razor9DoFIMU(void *arg) {
    (void)arg;
    chRegSetThreadName("Razor9DoFIMU");

    waitForCompletingInitialization();

    while (TRUE) {
        readRazor9DoFIMU();
        chThdSleepMilliseconds(10);
    }

    return (msg_t)0;
}

///////////////////////////////////////////////////////////////////////////////
// Initialization method.
///////////////////////////////////////////////////////////////////////////////

void initializeRazor9DoFIMU(void) {
    int i=0;
    for(i=0;i<64;i++) buf[i]=' ';

    // Initializes the serial driver UART3 in order to access the values from the Razor 9DoF IMU sensor.
    sdStart(&SD3, &portConfig);

    // PD8 (TX) transmit data to the Razor 9DoF IMU.
    palSetPadMode(GPIOD, 8, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_USART3));
    // PD9 (RX) receive data from the Razor 9DoF IMU.
    palSetPadMode(GPIOD, 9, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_USART3));

    // Start infrared reading thread.
    ThreadRazor9DoFIMU = chThdCreateStatic(workingAreaThread_Razor9DoFIMU,
                                           sizeof(workingAreaThread_Razor9DoFIMU),
                                           NORMALPRIO + 10, Thread_Razor9DoFIMU, NULL);
}


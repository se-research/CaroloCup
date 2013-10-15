/*
 * 
Ziyu Liu: Integrated vision. bug fixed

V1.02  Ultrasonic Integrated 
 
You need to change the I2C address of SRF08 before testing the code. To change the I2C address of the SRF08 you must have only one sonar on the bus. Then you can use changeAddress(uint8_t current, uint8_t moveto) to change address. Plz use 0xE2(ultrasonic1) 0xE4(ultrasonic2) 0xE6(ultrasonic3) as your new address. This change will be permanent. 
 */







#include <stdlib.h>
#include "ch.h"
#include "hal.h"
#include "chprintf.h"
#include "msv/include/ultrasonic.h"





#define SRF08_addr_0 0b1110000 //slave device address (7 bits without R/W bit For SRF08 in  Hex: 0xE0/0b1110000)
#define SRF08_addr_1 0b1110001 //0xE2
#define SRF08_addr_2 0b1110010 //0xE4
#define SRF08_addr_3 0b1110011 //oxE6
#define SRF08_TX_DEPTH 6
#define SRF08_RX_DEPTH 2
#define SRF08_RG_DEPTH 3


msg_t status = RDY_OK;
systime_t tmo = MS2ST(20);
static int16_t range[SRF08_RG_DEPTH];
static int8_t lightInstensity[SRF08_RG_DEPTH];


/* I2C interface  */
static const I2CConfig i2cfg1 = {
    OPMODE_I2C,
    400000,
    FAST_DUTY_CYCLE_2,
};


/**
 * Converts data from 2complemented representation to signed integer
 */
int16_t complement2signed(uint8_t msb, uint8_t lsb){
    uint16_t word = 0;
    word = (msb << 8) + lsb;
    if (msb > 0x7F){
        return -1 * ((int16_t)((~word) + 1));
    }
    return (int16_t)word;
}


/*
 * Function:    changeAddress
 * Args:        current moveto
 * Returns:     void
 * Description: Change the slave address on the I2C bus. To change a slave address, we write the following to current address(0xA0, 0xAA, 0xA5, new address).              
 */


void changeAddress(uint8_t current, uint8_t moveto) {

    uint8_t txbuf[SRF08_TX_DEPTH];
    uint8_t rxbuf[SRF08_RX_DEPTH];
    txbuf[0] = 0x00;
    txbuf[1] = 0xA0;       /* register address */
    i2cAcquireBus(&I2CD1);
    status = i2cMasterTransmitTimeout(&I2CD1, current, txbuf, 2, rxbuf, 0, tmo);
    i2cReleaseBus(&I2CD1);
    chThdSleepMilliseconds(50);
    txbuf[0] = 0x00;
    txbuf[1] = 0xAA;       /* register address */
    i2cAcquireBus(&I2CD1);
    status = i2cMasterTransmitTimeout(&I2CD1, current, txbuf, 2, rxbuf, 0, tmo);
    i2cReleaseBus(&I2CD1);
    chThdSleepMilliseconds(50);
    txbuf[0] = 0x00;
    txbuf[1] = 0xA5;       /* register address */
    i2cAcquireBus(&I2CD1);
    status = i2cMasterTransmitTimeout(&I2CD1, current, txbuf, 2, rxbuf, 0, tmo);
    i2cReleaseBus(&I2CD1);
    chThdSleepMilliseconds(50);
    txbuf[0] = 0x00;
    txbuf[1] = moveto;       /* new slave address */
    i2cAcquireBus(&I2CD1);
    status = i2cMasterTransmitTimeout(&I2CD1, current, txbuf, 2, rxbuf, 0, tmo);
    i2cReleaseBus(&I2CD1);
    chThdSleepMilliseconds(50);
}


/*
 * Function:    setMaxGainRegister
 * Args:        SRF08_addr rangeVal
 * Returns:     void
 * Description: Set the maximum gain registers of the SRF08. The defult maximum registers is 31.
 *              The SRF08 appears as a set of up to 36 registers.（36 in hex 0x24) 
 */


void setMaxGainRegister(uint8_t SRF08_addr, uint8_t registerVal){
    uint8_t txbuf[SRF08_TX_DEPTH];
    uint8_t rxbuf[SRF08_RG_DEPTH];
    txbuf[0] = 0x01;
    txbuf[1] = registerVal;  
    i2cAcquireBus(&I2CD1);
    status = i2cMasterTransmitTimeout(&I2CD1, SRF08_addr, txbuf, 2, rxbuf, 0, tmo);
    i2cReleaseBus(&I2CD1);
}



/*
 * Function:    setRange
 * Args:        SRF08_addr rangeVal
 * Returns:     void
 * Description: Set the maximum range of the SRF08. The defult ma
 *              The max range is about 11 meters. (255 in hex 0xFF)
 */


void setRange(uint8_t SRF08_addr, uint8_t rangeVal){
  uint8_t txbuf[SRF08_TX_DEPTH];
  uint8_t rxbuf[SRF08_RG_DEPTH];
  txbuf[0] = 0x02;       
  txbuf[1] = rangeVal;  
  i2cAcquireBus(&I2CD1);
  status = i2cMasterTransmitTimeout(&I2CD1, SRF08_addr, txbuf, 2, rxbuf, 0, tmo);
  i2cReleaseBus(&I2CD1);
}

/*
 * Function:    sendData
 * Args:        SRF08_addr
 * Returns:     void
 * Description: Send command to trigger measurement
 */

void sendData(uint8_t SRF08_addr){
  
  uint8_t txbuf[SRF08_TX_DEPTH];
  uint8_t rxbuf[SRF08_RG_DEPTH];
  txbuf[0] = 0x00;       /* register address */
  txbuf[1] = 0x51;
  i2cAcquireBus(&I2CD1);
  status = i2cMasterTransmitTimeout(&I2CD1, SRF08_addr, txbuf, 2, rxbuf, 0, tmo);
  i2cReleaseBus(&I2CD1);
}


/*
 * Function:    receiveRange
 * Args:        SRF08_addr,count
 * Returns:     void
 * Description: Read the Range. Result in cm. This function should only be called after triggering measurement, otherwise previous value is returned
 */

void receiveRange(uint8_t SRF08_addr, uint8_t count){
  uint8_t txbuf[SRF08_TX_DEPTH];
  uint8_t rxbuf[SRF08_RX_DEPTH];
  //uint16_t i =0;
  //uint16_t j =0;
   txbuf[0] = 0x02 ;
   i2cAcquireBus(&I2CD1);
   status=i2cMasterTransmitTimeout(&I2CD1, SRF08_addr, txbuf, 1, rxbuf, 2 , tmo);
   range[count] = complement2signed(rxbuf[0], rxbuf[1]);
   i2cReleaseBus(&I2CD1);
}


/*
 * Function:    receiveLightIntensity
 * Args:        SRF08_addr,count
 * Returns:     void
 * Description: Read the Light Instensity. The reading increases as the brightness increases. It should get close to 2-3 in complete darkness and up to about
 *              248 (0xF8) in bright light. This value is only updated after triggering measurement.
 */


void receiveLightIntensity(uint8_t SRF08_addr, uint8_t count){
    uint8_t txbuf[SRF08_TX_DEPTH];
    uint8_t rxbuf[SRF08_RG_DEPTH];
    txbuf[0] = 0x01; //read data from location 1
    i2cAcquireBus(&I2CD1);
    status=i2cMasterTransmitTimeout(&I2CD1, SRF08_addr, txbuf, 1, rxbuf, 1, tmo);
    lightInstensity[count] = rxbuf[0];
    i2cReleaseBus(&I2CD1);
}


static WORKING_AREA(myThreadWorkingArea, 128);
static msg_t Thread2(void *arg) {
  
  (void)arg;
  chRegSetThreadName("ultrasonic");
  systime_t time = chTimeNow(); 
//loop for get range.
    
            while (TRUE) {
               time += MS2ST(100);   //use a timer to solve the thread conflict issue  

                sendData(SRF08_addr_1);     //Trigger measurement for US1

                chThdSleepMilliseconds(1);  //wait 1ms
		
                sendData(SRF08_addr_2);     //trigger measurement for US2

                chThdSleepMilliseconds(1);  //wait 1ms 

                sendData(SRF08_addr_3);     //trigger measurement for US3

                chThdSleepMilliseconds(65); //wait 65ms
                
                receiveLightIntensity(SRF08_addr_1, 0);//receive Light Intensity

                receiveRange(SRF08_addr_1,0);//read measurement from US1

                chThdSleepMilliseconds(5);  //wait 1ms 
      
                receiveRange(SRF08_addr_2,1);//read measurement from US2
                
                //receiveLightIntensity(SRF08_addr_2, 1); //receive Light Intensity

                chThdSleepMilliseconds(5);   //wait 1ms 

                receiveRange(SRF08_addr_3,2); //read measurement from US3
                
                chThdSleepUntil(time);   //Suspends the invoking thread until the system time arrives to the specified value
                
                //receiveLightIntensity(SRF08_addr_1, 2) //receive Light Intensity
            }
         
  chThdSleepMilliseconds(10);
  return (msg_t)0;
}


//print fuction
void cmd_printDataFromUltrasonic(BaseSequentialStream *chp, int argc, char *argv[]) {
  (void)chp;
  (void)argc;
  (void)argv;
  
  chprintf(chp, "Distance data in centimeter");
  chprintf(chp, "\r\n");
  chprintf(chp, "Range1=%d\n",getRange(1));
  chprintf(chp, "\r\n");
  //chprintf(chp, "Light1=%d\n",getLightInstensity(1));
  //chprintf(chp, "\r\n");
  chprintf(chp, "Range2=%d\n",getRange(2));
  chprintf(chp, "\r\n");
  chprintf(chp, "Range3=%d\n",getRange(3));
  chprintf(chp, "\r\n");

}



void myUltrasonicInit(void) {

  i2cStart(&I2CD1, &i2cfg1);
  palSetPadMode(GPIOB, 6, PAL_MODE_ALTERNATE(4) | PAL_STM32_OTYPE_OPENDRAIN);   /* SCL */
  palSetPadMode(GPIOB, 9, PAL_MODE_ALTERNATE(4) | PAL_STM32_OTYPE_OPENDRAIN);   /* SDA */
 //changeAddress(SRF08_addr_1,0xE2);
 //setRange(SRF08_addr_1,0xFF)
 //setMaxGainRegister(SRF08_addr_1, 0x24)
 chThdCreateStatic(myThreadWorkingArea, sizeof(myThreadWorkingArea),
                    NORMALPRIO + 10, Thread2, NULL);
 /* Thread *tp = chThdCreateFromHeap(NULL, THD_WA_SIZE(128), NORMALPRIO+1,
                                 Thread2, NULL); */
 // if (tp == NULL)
   // chSysHalt();    /* Memory exausted. */

 // msg_t msg = chThdWait(tp);
}



int16_t getLightInstensity(int SRF_num) {
    
    return lightInstensity[SRF_num-1];
}

int16_t getRange(int SRF_num) {
	return range[SRF_num-1];
}

void getUS(int16_t *usData){
    usData[0] = getRange(1);
    usData[1] = getRange(2);
    usData[2] = getRange(3);
}

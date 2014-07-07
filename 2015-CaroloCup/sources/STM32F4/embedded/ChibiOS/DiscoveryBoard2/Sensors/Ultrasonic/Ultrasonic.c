/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuation.
///////////////////////////////////////////////////////////////////////////////

static Thread *ThreadUltrasonic = NULL;

#define MAX_NUMBER_OF_ULTRASONICS         5

// I2C interface configuration.
static const I2CConfig i2cConfiguration = {
    OPMODE_I2C,
    400000,
    FAST_DUTY_CYCLE_2,
};

// Addresses for the attached ultrasonic devices.
// These addresses need to be changed to the manually configured values.
static int addressUS0 = 226;
static int addressUS1 = 228;
static int addressUS2 = 230;

// Buffer for the data samples.
static int16_t dUS0 = 0;
static int16_t dUS1 = 0;
static int16_t dUS2 = 0;

// This flag indicates manually triggered continuous readings.
static int continuousMeasurements = 0;

// Prototype declarations.
void readUltrasonic(void);
void setUltrasonicAddress(BaseSequentialStream *chp, int oldAddress, int newAddress);

///////////////////////////////////////////////////////////////////////////////
// Interface methods.
///////////////////////////////////////////////////////////////////////////////

Thread * getThreadUltrasonic(void) {
    return ThreadUltrasonic;
}

void getUltrasonicData(UltrasonicDataT *data) {
    if (data != NULL) {
        int8_t i = 0;
        UltrasonicDataT *curr = data;

        for(i = 0; i < MAX_NUMBER_OF_ULTRASONICS; i++) {
            switch (i) {
                case 0:
                    curr->address = addressUS0;
                    curr->distance = (int)dUS0;
                break;
                case 1:
                    curr->address = addressUS1;
                    curr->distance = (int)dUS1;
                break;
                case 2:
                    curr->address = addressUS2;
                    curr->distance = (int)dUS2;
                break;
            }
            
            if (curr->next == NULL)
                break;
            curr = curr->next;
        }
    }
}

void commandPrintUltrasonicDistances(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    chprintf(chp, "Ultrasonic distances: d0 = %d, d1 = %d,  d2 = %d", dUS0, dUS1, dUS2);
    chprintf(chp, "\r\n");
}

void commandSetUltrasonicAddress(BaseSequentialStream *chp, int argc, char *argv[]) {
    if (argc == 2) {
        int oldAddress = atoi(argv[0]);
        int newAddress = atoi(argv[1]);

        int validOldAddress = 0;
        int validNewAddress = 0;

        int i = 224;
        for(;i < 256; i+=2) {
            validOldAddress |= (oldAddress == i);
            validNewAddress |= (newAddress == i);
        }

        if (!validNewAddress) {
            chprintf(chp, "Ultrasonic: Specified address %s is invalid.\r\n", argv[0]);
        }
        if (!validOldAddress) {
            chprintf(chp, "Ultrasonic: Specified address %s is invalid.\r\n", argv[0]);
        }
        if (validOldAddress && validNewAddress) {
            chprintf(chp, "Ultrasonic: Changing ultra sonic device from %d to %d", oldAddress, newAddress);
            setUltrasonicAddress(chp, oldAddress, newAddress);
        }
    }
    else {
        chprintf(chp, "To change the ultrasonic address, disconnect STM32F4 Discovery Board\r\n");
        chprintf(chp, "completely, and connect only one ultrasonic sensor to I2C bus.\r\n");
        chprintf(chp, "Then, restart the STM32F4 Discovery Board again and invoke this\r\n");
        chprintf(chp, "function again like this: sua oldAddress newAddress (for example sua 224 226).\r\n");
    }
}

void commandTriggerSingleUltrasonicMeasurement(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    readUltrasonic();
    chprintf(chp, "Ultrasonic distance %d, %d, %d.\r\n", dUS0, dUS1, dUS2);
}

void commandStartContinuousUltrasonicMeasurements(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    continuousMeasurements = 1;
    chprintf(chp, "Ultrasonic: Start continous measurements. Read values by calling pdu.\r\n");
}

void commandStopContinuousUltrasonicMeasurements(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    continuousMeasurements = 0;
    chprintf(chp, "Ultrasonic: Stop continous measurements.\r\n");
}

///////////////////////////////////////////////////////////////////////////////
// Sensor handling methods.
///////////////////////////////////////////////////////////////////////////////

msg_t commandUltrasonic(int address, uint8_t command) {
    msg_t retCode = RDY_OK;

    uint8_t TX[2];
    TX[0] = 0x00;
    TX[1] = command;

    i2cAcquireBus(&I2CD1);
        retCode &= i2cMasterTransmit(&I2CD1, address>>1, TX, 2, NULL, 0);
    i2cReleaseBus(&I2CD1);

    return retCode;
}

msg_t readUltrasonicRegister(int address, uint8_t reg, int16_t *range) {
    msg_t retCode = RDY_OK;

    uint8_t RX[2];
    RX[0] = 0;
    RX[1] = 0;

    i2cAcquireBus(&I2CD1);
        retCode &= i2cMasterTransmit(&I2CD1, address>>1, &reg, 1, NULL, 0);
        retCode &= i2cMasterReceiveTimeout(&I2CD1, address>>1, RX, 1, TIME_INFINITE);
        retCode &= i2cMasterReceiveTimeout(&I2CD1, address>>1, RX+1, 1, TIME_INFINITE);
    i2cReleaseBus(&I2CD1);

    // Measured distance is stored in second byte.
    *range = RX[1];

    return retCode;
}

void setUltrasonicAddress(BaseSequentialStream *chp, int oldAddress, int newAddress) {
    msg_t retCode = RDY_OK;

    // Reprogramming sequence is: 0xA0, 0xAA, 0xA5, newAddress.
    retCode &= commandUltrasonic(oldAddress, 0xA0);
    chThdSleepMilliseconds(50);
    chprintf(chp, ".");

    retCode &= commandUltrasonic(oldAddress, 0xAA);
    chThdSleepMilliseconds(50);
    chprintf(chp, ".");

    retCode &= commandUltrasonic(oldAddress, 0xA5);
    chThdSleepMilliseconds(50);
    chprintf(chp, ".");

    retCode &= commandUltrasonic(oldAddress, newAddress);
    chThdSleepMilliseconds(50);

    if (retCode == RDY_OK)
        chprintf(chp, " Success.\r\n");
    else
        chprintf(chp, " Failed.\r\n");
}

void readUltrasonic(void) {
    msg_t retCode = RDY_OK;

    // Trigger sequence for measuring is: 0x51 - wait 70ms - , read two bytes from 0x02.
    retCode &= commandUltrasonic(addressUS0, 0x51);
    retCode &= commandUltrasonic(addressUS1, 0x51);
    retCode &= commandUltrasonic(addressUS2, 0x51);
    chThdSleepMilliseconds(70);

    retCode &= readUltrasonicRegister(addressUS0, 0x2, &dUS0);
    retCode &= readUltrasonicRegister(addressUS1, 0x2, &dUS1);
    retCode &= readUltrasonicRegister(addressUS2, 0x2, &dUS2);
}

static WORKING_AREA(workingAreaThread_Ultrasonic, 512);
static msg_t Thread_Ultrasonic(void *arg) {
    (void)arg;
    chRegSetThreadName("Ultrasonic");

    waitForCompletingInitialization();

    while (TRUE) {
        if (!hasShell() || continuousMeasurements) {
            readUltrasonic();
        }
        else {
            chThdSleepMilliseconds(70);
        }
    }

    return (msg_t)0;
}

///////////////////////////////////////////////////////////////////////////////
// Initialization method.
///////////////////////////////////////////////////////////////////////////////

void initializeUltrasonic(void) {
    // Initializes the I2C driver 1 in order to access the distance values via I2C bus.
    i2cStart(&I2CD1, &i2cConfiguration);

    palSetPadMode(GPIOB, 6, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_I2C1) | PAL_STM32_OTYPE_OPENDRAIN); // SCL
    palSetPadMode(GPIOB, 9, PAL_MODE_ALTERNATE(STM32F4GPIO_AF_I2C1) | PAL_STM32_OTYPE_OPENDRAIN); // SDA

    // Start ultrasonic reading thread.
    ThreadUltrasonic = chThdCreateStatic(workingAreaThread_Ultrasonic,
                                         sizeof(workingAreaThread_Ultrasonic),
                                         NORMALPRIO + 10, Thread_Ultrasonic, NULL);
}


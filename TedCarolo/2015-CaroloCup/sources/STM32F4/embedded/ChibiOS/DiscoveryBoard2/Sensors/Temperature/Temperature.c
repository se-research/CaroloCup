/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

///////////////////////////////////////////////////////////////////////////////
// Data structures and configuation.
///////////////////////////////////////////////////////////////////////////////

static Thread *ThreadTemperature = NULL;

#define TEMPERATURE_ADC_GRP_NUM_CHANNELS    1
#define TEMPERATURE_ADC_GRP_BUF_DEPTH       8
#define V25                                 0.760
#define AVG_SLOPE                           25.0

// Buffer for the data samples.
static adcsample_t temperatureSamples[TEMPERATURE_ADC_GRP_NUM_CHANNELS * TEMPERATURE_ADC_GRP_BUF_DEPTH];
static float temp = 0;

static const ADCConversionGroup temperatureSensor = {
    FALSE, // No circular buffer mode.
    TEMPERATURE_ADC_GRP_NUM_CHANNELS, // Number of the analog channels.
    NULL, // No callback function.
    NULL, // No error callback function.
    0, // CR1.
    ADC_CR2_SWSTART, // CR2.
    ADC_SMPR1_SMP_SENSOR(ADC_SAMPLE_144), // Sample times for channels 10 - 18, here sample time for channel SENSOR.
    0, // Sample times for channels 0 - 9.
    ADC_SQR1_NUM_CH(TEMPERATURE_ADC_GRP_NUM_CHANNELS), // ADC SQR1 register initialization data for conversion group sequence 13 - 16 + sequence length.
    0, // ADC SQR2 register initialization data for conversion group sequence 7 - 12.
    ADC_SQR3_SQ1_N(ADC_CHANNEL_SENSOR) // ADC SQR3 register initialization data for conversion group sequence 1 - 6.
};

///////////////////////////////////////////////////////////////////////////////
// Interface methods.
///////////////////////////////////////////////////////////////////////////////

Thread* getThreadTemperature(void) {
    return ThreadTemperature;
}

void getTemperatureData(TemperatureDataT *data) {
    if (data != NULL) {
        data->T = (int8_t)temp;
    }
}

void commandPrintTemperature(BaseSequentialStream *chp, int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    chprintf(chp, "Temperature: t = %f", temp);
    chprintf(chp, "\r\n");
}

///////////////////////////////////////////////////////////////////////////////
// Sensor reading methods.
///////////////////////////////////////////////////////////////////////////////

void readTemperature(void) {
    adcConvert(&ADCD1, &temperatureSensor, temperatureSamples, TEMPERATURE_ADC_GRP_BUF_DEPTH);

    int i = 0;
    int temperature = 0;
    for(; i < TEMPERATURE_ADC_GRP_NUM_CHANNELS * TEMPERATURE_ADC_GRP_BUF_DEPTH; i++) {
        temperature += (int) temperatureSamples[i];
    }

    // Average the last TEMPERATURE_ADC_GRP_BUF_DEPTH sampled values.
    temperature /= TEMPERATURE_ADC_GRP_BUF_DEPTH;

    // See http://www.st.com/web/en/resource/technical/document/reference_manual/DM00031020.pdf, page 289.
    temperature = (temperature * 3300 / 65535);
    temp = (temperature / 1000.0);
    temp = ((temp - V25) / AVG_SLOPE) + 25.0 ;
}

static WORKING_AREA(workingAreaThread_Temperature, 512);
static msg_t Thread_Temperature(void *arg) {
    (void)arg;
    chRegSetThreadName("Temperature");

    waitForCompletingInitialization();

    while (TRUE) {
        readTemperature();
        chThdSleepMilliseconds(1000);
    }

    return (msg_t)0;
}

///////////////////////////////////////////////////////////////////////////////
// Initialization method.
///////////////////////////////////////////////////////////////////////////////

void initializeTemperature(void) {
    // Initializes the ADC driver 1 in order to access the analog values from the onboard temperature sensor.
    adcStart(&ADCD1, NULL);

    // Temperature sensor initialization.
    adcSTM32EnableTSVREFE();

    // Start temperature reading thread.
    ThreadTemperature = chThdCreateStatic(workingAreaThread_Temperature,
                                          sizeof(workingAreaThread_Temperature),
                                          NORMALPRIO + 5, Thread_Temperature, NULL);
}


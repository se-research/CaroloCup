#include <stdlib.h>
#include <math.h>

#include "ch.h"
#include "hal.h"

#include "chprintf.h"

#include "msv/include/ir.h"

#define IR_ADC_GRP1_NUM_CHANNELS   3
#define IR_ADC_GRP1_BUF_DEPTH      32

static adcsample_t irSamples[IR_ADC_GRP1_NUM_CHANNELS * IR_ADC_GRP1_BUF_DEPTH]; //sample buffer
static float IR_array[3] = {0, 0 , 0};  //last readings saved here

//Conversion group config
static const ADCConversionGroup adcgrpcfg1 = {
	FALSE, // circular buffer mode
	IR_ADC_GRP1_NUM_CHANNELS, // number of the analog channels
	NULL, // callback function (not needed here)
	NULL, // error callback
	0, // CR1
	ADC_CR2_SWSTART, // CR2
	//Relevant
	//Channels
	ADC_SMPR1_SMP_AN11(ADC_SAMPLE_3) | ADC_SMPR1_SMP_AN14(ADC_SAMPLE_3) | ADC_SMPR1_SMP_AN15(ADC_SAMPLE_3),// sample times for channel 10-18
	0,// sample times for channel 0-9
	//Conversion groups	
	//ADC_SQR1_NUM_CH(IR_ADC_GRP1_NUM_CHANNELS) = Sequence length, must always be present
	ADC_SQR1_NUM_CH(IR_ADC_GRP1_NUM_CHANNELS),// ADC SQR1 register initialization data Conversion group sequence 13-16 + sequence length.
	0, // ADC SQR2 Conversion group sequence 7-12
	ADC_SQR3_SQ1_N(ADC_CHANNEL_IN11) | ADC_SQR3_SQ2_N(ADC_CHANNEL_IN14) | ADC_SQR3_SQ3_N(ADC_CHANNEL_IN15) // ADC SQR3 Conversion group sequence 1-6
};

// This macro creates the working space and stack space for the ir thread
static WORKING_AREA(irThreadWA, 128); 

//Function for running the thread
msg_t irThread(void *arg)
{
    (void)arg;
    chRegSetThreadName("IR thread");
    while (TRUE)
    {
	//IR sensor updated every 60ms
        readIR();
        chThdSleepMilliseconds(60);
    }
    return 0;
}

//Can be used to test the sensors in the shell
//***remember to add to shell commands in main.c***
void printIR(BaseSequentialStream *chp, int argc, char *argv[]) {
	(void)argc;
	(void)argv;
	int ir1 = getIR1();
	int ir2 = getIR2();
	int ir3 = getIR3();
	chprintf(chp, "IR1: %d, IR2: %d, IR3: %d \r\n", ir1, ir2, ir3);  
}

void readIR(void) {
	int i = 0;
 	float ir_sensor_1 = 0;
	float ir_sensor_2 = 0;
	float ir_sensor_3 = 0;

	adcConvert(&ADCD1, &adcgrpcfg1, irSamples, IR_ADC_GRP1_BUF_DEPTH);

	//Taking the samples to use for mapping
	for(i = 0; i < (IR_ADC_GRP1_NUM_CHANNELS * IR_ADC_GRP1_BUF_DEPTH); i+=3) {
		ir_sensor_1 += irSamples[i];
		ir_sensor_2 += irSamples[i+1];
		ir_sensor_3 += irSamples[i+2];
	} 

	//Map voltages for correct reading using formula Voltage = (irSamples * (3.0 / 4096)) 
	//Map voltages to cm using formula 12.174 * V ^ ⁻¹,⁰⁵¹ 
	ir_sensor_1 = 12.174 * powf((ir_sensor_1/32) * (3.0 / 4096), -1.051);
	ir_sensor_2 = 12.174 * powf((ir_sensor_2/32) * (3.0 / 4096), -1.051);
	ir_sensor_3 = 12.174 * powf((ir_sensor_3/32) * (3.0 / 4096), -1.051);


	//Flagging if ir sensor is below or more than its range of 4-30cm
	if(ir_sensor_1 < 4){
		IR_array[0] = 0;
	}
	else if(ir_sensor_1 > 30){
		IR_array[0] = -1;
	} else {
		IR_array[0] = ir_sensor_1;
	}

	if(ir_sensor_2 < 4){
		IR_array[1] = 0;
	}
	else if(ir_sensor_2 > 30){
		IR_array[1] = -1;
	}else {
		IR_array[1] = ir_sensor_2;
	}

	if(ir_sensor_3 < 4){
		IR_array[2] = 0;
	}
	else if(ir_sensor_3 > 30){
		IR_array[2] = -1;
	}else {
		IR_array[2] = ir_sensor_3;
	}
}



int getIR1(void){
 return IR_array[0];	
}

int getIR2(void){
 return IR_array[1];
}

int getIR3(void){
 return IR_array[2];
}

//Allocating a float array of length 3 and pass as pointer to this function
void getIR(int* int_array) {
 int_array[0] = (int)IR_array[0];
 int_array[1] = (int)IR_array[1];
 int_array[2] = (int)IR_array[2];
}

void ADCinit(void){	
	int i = 0;
   // Initialize the buffer
	for (i=0;i<(IR_ADC_GRP1_NUM_CHANNELS * IR_ADC_GRP1_BUF_DEPTH);i++){
		irSamples[i] = 0;
	}

	// PC1 IN11
	palSetGroupMode(GPIOC, PAL_PORT_BIT(1), 0, PAL_MODE_INPUT_ANALOG);
	
	//PC4 IN14
	palSetGroupMode(GPIOC, PAL_PORT_BIT(4), 0, PAL_MODE_INPUT_ANALOG);
	
	//PC5 IN15	
	palSetGroupMode(GPIOC, PAL_PORT_BIT(5), 0, PAL_MODE_INPUT_ANALOG);

	//Starts the adc driver
	adcStart(&ADCD1, NULL);

	//Creates and starts the thread
	chThdCreateStatic(irThreadWA, sizeof(irThreadWA), NORMALPRIO, irThread, NULL);
}

/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */


#include "core/data/Container.h"
#include "core/io/ContainerConference.h"
#include <string.h>  /* String function definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include <cstdio>   /* Standard input/output definitions */
#include <linux/serial.h>
#include <sys/ioctl.h>
#include <pthread.h>
#include <iostream>
#include <sstream>

#include <stdio.h> 
#include <stdlib.h> 
#include <assert.h> 
#include <sys/mman.h> 
#include <sys/types.h> 
#include <sys/stat.h> 
#include <signal.h> 


#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <math.h>
#include "SensorData.h"
#include "Sensors.h"
#include "gpio_functions.h"



#include <bitset>

#define XV11_CHECKSUM_INDEX 20

using namespace std;

unsigned char starter[1];
unsigned char infra[13];
unsigned char ultra[9];
//unsigned short int lenght = 2;

int fd;
int timeCount = 0;
allSensors sensors;


///////////////////////////////////////Hall Effect
int pins[3] = {0, 0, 0};
int pinsNew[3] = {0, 0, 0};
int movement = 0;

void *map_base, *virt_addr;
int fm;

int *results; 
int resultSaved[3] = {0, 0, 0}; 
////////////////////////////////////////Hall Effect

int converter(char* arrayInput, int lenght);
unsigned long accMovement = 0;


namespace carolocup {

    void  *sensorGathering(void *argument);

    using namespace std;
    using namespace core::base;
    using namespace core::data;

        void initialize_pin_reading();    //Hall Effect
	int get_movement_data();          //Hall Effect
	int map_pins();			  //Hall Effect
	void setup_gpiopin(int channel, int bit, int value, int pullval); //Hall Effect
	int read_gpio_pin(int channel, int bit );	//Hall Effect
	void* loop_retrieving(void *arg);	//Hall Effect
	int is_movement();		//Hall Effect
	int *get_gpio_data();		//Hall Effect
	int pins_state(int pin_data[]);		//Hall Effect
	int calculate_movement(int before, int after);


   Sensors::Sensors(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "Sensors"),
        firstInfraredDistance(0),
        secondInfraredDistance(0),
        thirdInfraredDistance(0),
        fourthInfraredDistance(0),

        firstUltrasonicDistance(0),
        secondUltrasonicDistance(0),
        m_fifo(){}

    Sensors::~Sensors() {}

    void Sensors::setUp() {
        // This method will be call automatically _before_ running body().
    }

    void Sensors::tearDown() {
        // This method will be call automatically _after_ return from body().
    }

    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE Sensors::body() {

		   fd = open("/dev/ttyUSB0", O_RDWR | O_NOCTTY | O_NDELAY);
		   //fd = open("/dev/ttyACM0", O_RDWR | O_NOCTTY | O_NDELAY);

		    if (fd == -1){
			perror("cannot open");
		     }

		    else
		    	fcntl(fd, F_SETFL, 0);
			struct termios options;
			tcgetattr(fd, &options);
			cfsetispeed(&options, B115200);
			cfsetospeed(&options, B115200);
			options.c_cflag |= (CLOCAL | CREAD);
			//tcsetattr(fd, TCSANOW, &options);
			options.c_cflag &= ~CSIZE;
			options.c_cflag &= ~PARENB;
			options.c_cflag &= ~CSTOPB;
			options.c_cflag &= ~CSIZE;
			options.c_cflag |= CS8;
		 	tcflush(fd, TCIFLUSH);
		   	tcflush(fd, TCIOFLUSH);

		  if(tcsetattr(fd, TCSANOW, &options) != 0){

			cout << "Error " << errno << " from tcsetattr" << endl;
		   }		

		   SensorData gatherData;

		   initialize_pin_reading();

		   pthread_t t1;
                   pthread_create(&t1, NULL, sensorGathering, NULL);

		   int lastTime = 0; 
		   while (getModuleState() == ModuleState::RUNNING) {
			
			accMovement = accMovement + get_movement_data();
			//cout << "Hall effect: " << accMovement << endl;
		    	gatherData.setMovement(accMovement);
			gatherData.setInfraredDistance(1, sensors.firstInfraredDistance);
			gatherData.setInfraredDistance(2, sensors.secondInfraredDistance);
			gatherData.setInfraredDistance(3, sensors.thirdInfraredDistance);
			gatherData.setInfraredDistance(4, sensors.fourthInfraredDistance);

			gatherData.setUltrasonicDistance(1, sensors.firstUltrasonicDistance);
			//cout << "FC Ultra: " << sensors.firstUltrasonicDistance; 
			gatherData.setUltrasonicDistance(2, sensors.secondUltrasonicDistance);

			cout << "gatherData: " << gatherData.toString() << endl;
/*
			stringstream buffer;
			buffer << gatherData;

			SensorData data2;
			buffer >> data2;

			cout << "data2: " << data2.toString() << endl;
*/
	                //tcflush(fd, TCIOFLUSH);
			///////////////////////READ FROM PORT//////////////////////////////////
			Container contSensor(Container::USER_DATA_3, gatherData);
			// Send containers.
			getConference().send(contSensor);
		        //  } //If statement controlled by Time count
			//cout << "Inside" << endl;
			//cout << "Time: " << timeCount << endl;


		  } //End of ModuleState::RUNNING

    		return ModuleState::OKAY;
    }


int converter(char* arrayInput, int lenght){
     	int num = 0;

	for(int i = 0;i < lenght; i++){
		if(arrayInput[i] < '0' || arrayInput[i] > '9') {
			arrayInput[i] = '0';
		}
		arrayInput[i] = arrayInput[i] - '0';
		num = (arrayInput[i]*pow(10, lenght-i-1)+num);

	}

    return num;  

}//End of Converter function  


/* Initializes the the pins and opens a thread for continual reading. To be
 * called by Erlang 
*/
void initialize_pin_reading()
{

  map_pins();

  setup_gpiopin(PIN31Channel, PIN31Bit, PULLDS, INPUT); 
  setup_gpiopin(PIN29Channel, PIN29Bit, PULLDS, INPUT); 
  setup_gpiopin(PIN27Channel, PIN27Bit, PULLDS, INPUT); 

  pthread_t loop; 
  pthread_create(&loop, NULL, &loop_retrieving,NULL);

}      //End of initialize_pin_reading


int get_movement_data()
{

  int return_value = movement;
  movement = 0;
//  printf("(%d, %d, %d )", pins[0], pins[1], pins[2]); 
  return return_value;
}    // End of get_movement_data


int map_pins()
{
  off_t target = EXYNOS;
  if((fm = open("/dev/mem", O_RDWR | O_SYNC)) == -1) {

      printf("Error opening /dev/mem, can't setup file descriptors for GPIO\n");
      return 1;
  } 

  map_base = mmap(0, MAP_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, fm, target & ~MAP_MASK);
  if(map_base == (void *) -1) {
        
        printf("Mapping of GPIO space failed!!\n");
        return 1;
  }   

}	//End of Map_Pins

/* Sets up a specific pin for reading 
*/


void setup_gpiopin(int channel, int bit, int value, int pullval) {
	div_t div_res;
	unsigned char val, tmp, hld;
	unsigned char * base;
                               
	base = (unsigned char *)(map_base + channel) - GPIO_GPCONREG;
	div_res = div (bit, 2);		// 2 nibbles per byte so divide by 2
	base += div_res.quot;
	val  = *(unsigned char *) base;
	if (value) {				// non-zero means set 0001=output
		if (div_res.rem) {		// if remainder then its upper nibble
			val &= 0b00011111;	// upper nibble, not always def to zero
			val |= 0b00010000;	// set upper nibble as output
		} else {				// otherwise its lower nibble
			val &= 0b11110001;	// not always def to zero on boot
			val |= 0b00000001;	// set lower nibble as output
		}
	} else {					// otherwise set 0000=input
		if (div_res.rem) {		// if remainder then its upper nibble
			val &= 0b00001111;	// clear upper nibble to be input
		} else {				// otherwise its lower nibble
			val &= 0b11110000;	// clear lower nibble to be input
		}
	}				
	*(unsigned char *) base = val;	
	base = (unsigned char *)(map_base + channel) + GPIO_UPDOWN;
	if      (pullval == PULLUP) {
		tmp = 0b00000010;		// pullup enabled
	}
	else if (pullval == PULLDN) {
		tmp = 0b00000001;		// pulldown enabled
	} 
	else {
		tmp = 0;				// disable pullup/down
	}
	if (bit < 4) {
		hld = tmp << (bit*2);	// shift the 2 bits to their proper location
	} else {
		bit = bit - 4;
		hld = tmp << (bit*2);	// shift the 2 bits to their proper location
		base++;					// move up to next byte
	}
	val = *(unsigned char *) base;

	val |= hld;
}    //////End of setup_gpiopin

/* Reads a specific pin e.g. pin 27, pin29, pin31 etc
*/


int read_gpio_pin(int channel, int bit )
{ 
  int input;
  input = *(unsigned char *) (map_base + channel);
	if (input & (1 << bit)) {
    return 1;	
	} else {
    return 0;
	}
} ////End of read_gpio_pin

/* Thread that runs as a loop, continually reading pinws 27, 29 and 31 and
 * saving their data
*/


void* loop_retrieving(void *arg)
{

  while(1)
  {
    
    pinsNew[0] = read_gpio_pin(PIN27Channel, PIN27Bit);
    pinsNew[1] = read_gpio_pin(PIN31Channel, PIN31Bit);
    pinsNew[2] = read_gpio_pin(PIN29Channel, PIN29Bit);

//cout<<"Pin 0:  "<<pinsNew[0] <<endl;
    if(is_movement())
    {
      movement += calculate_movement(pinsNew[0], pins[0]);
      //printf("pinstate: %d, pinstateNew: %d", pins_state(pins),pins_state(pinsNew));
      //if(pins[0] != pinsNew[0] || pins[1] != pinsNew[1] || pins[2] != pins[2])
      //  printf("(%d, %d, %d)\n", pins[0], pins[1], pins[2]);
      pins[0] = pinsNew[0];
      pins[1] = pinsNew[1];
      pins[2] = pinsNew[2];
      //printf("(%d, %d, %d )\n", pins[0], pins[1], pins[2]); 
    }

    usleep(100);

  }
 
}     /////End of loop_retrieving

/* Checks if a pin's value has changed, indicating movement
*/

int is_movement()
{
    if(pins[0] != pinsNew[0])
      return 1;
    else if(pins[1] != pinsNew[1])
      return 1;
    else if(pins[2] != pinsNew[2])
      return 1;
    else
      return 0;
}   //////End of is_movement



int *get_gpio_data()
{ 
  return pins;
}

int pins_state(int pin_data[])
{
  if (pin_data[0] == 1 && pin_data[1] == 0 && pin_data[2] == 0)  
    return 1; 
  else if (pin_data[0] == 1 && pin_data[1] == 1 && pin_data[2] == 0) 
    return 2; 
  else if (pin_data[0] == 0 && pin_data[1] == 1 && pin_data[2] == 0)  
    return 3; 
  else if (pin_data[0] == 0 && pin_data[1] == 1 && pin_data[2] == 1)  
    return 4; 
  else if (pin_data[0] == 0 && pin_data[1] == 0 && pin_data[2] == 1)  
    return 5; 
  else if (pin_data[0] == 1 && pin_data[1] == 0 && pin_data[2] == 1)  
    return 6; 

  return 0;

}   ///End of *get_gpio_data




/* Calculates distance that the car has moved
*/
int calculate_movement(int before, int after)
{

 if (before >= after)
    return 0;
 else
    return (after - before);
 //else if (after < before)     
    //return after - before + 6;
}  /////End of  calculate_movement;

void *sensorGathering(void *argument){
    cout << "\nShowing Frame 2 From Thread 2"  << endl;
   while(1) {
      sleep(0.01);

  //timeCount++;
//////////////////////Hall Effect/////////////////////////////////////////////////
/*results = get_gpio_data(); 
 
      if(results[0] != resultSaved[0] || results[1] != resultSaved[1] ||results[2]!=resultSaved[2]){ 
        //printf("OldReadings: (%d, %d, %d)\n", resultSaved[0], resultSaved[1],resultSaved[2]); 
        //printf("NewReadings: (%d, %d, %d)\n", results[0], results[1], results[2]); 
        resultSaved[0] = results[0]; 
        resultSaved[1] = results[1]; 
        resultSaved[2] = results[2]; 
        //printf("(%d, %d, %d)\n", resultSaved[0], resultSaved[1],resultSaved[2]); 
        //printf("movement: %d\n", get_movement_data()); 
      }*/
////////////////////////Hall Effect/////////////////////////////////////////////////////


    //if(timeCount > 20){
    //   timeCount = 0;
       int n = read(fd, starter, 1);

        if(n < 0){
        fputs("read failed!\n", stderr);

        }


    if(starter[0] == 'i'){

        int n = read(fd, infra, 12);
	char firstInfra[2]; 
	firstInfra[0] = infra[0];
	firstInfra[1] = infra[1];

        char secondInfra[2];
	secondInfra[0] = infra[3];
	secondInfra[1] = infra[4];

 	char thirdInfra[2];
	thirdInfra[0] = infra[6];
	thirdInfra[1] = infra[7];

 	char fourthInfra[2];
	fourthInfra[0] = infra[9];
	fourthInfra[1] = infra[10];
	char three = infra[2];     //the ',' symbol
	char six = infra[5];      //the ',' symbol
	char nine = infra[8];		//the ',' symbol
	char twelve = infra[11];  //the '.' symbol
    
	int firstInfraDist;
	int secondInfraDist;
	int thirdInfraDist;
	int fourthInfraDist;

   if(three == ',' && six == ',' && nine == ',' && twelve == '.'){

  	int one = 1;
	int two = 2;
	int threes = 3;
	int four = 4;
   	//if(firstInfra[0] == '0'){
	firstInfraDist = converter(firstInfra, 2);
//	cout<<"Found First: "<<firstInfraDist<<endl;
	sensors.firstInfraredDistance = firstInfraDist;

	secondInfraDist = converter(secondInfra, 2);	
//	cout<<"Found Second: "<<secondInfraDist<<endl;
   	sensors.secondInfraredDistance = secondInfraDist;

	thirdInfraDist = converter(thirdInfra, 2);	
//	cout<<"Found Third: "<<thirdInfraDist<<endl;
   	sensors.thirdInfraredDistance = thirdInfraDist;

	fourthInfraDist = converter(fourthInfra, 2);
//	cout<<"Found Fourth: "<<fourthInfraDist<<endl;
   	sensors.fourthInfraredDistance = fourthInfraDist;

    }  //End of main if

      }   //end of if statement when start byte is found
   
  
    if(starter[0] == 'u'){


        int n = read(fd, ultra, 8);

        //cout<<ultra<<endl;

	char firstUltra[3];
	firstUltra[0] = ultra[0];
	firstUltra[1] = ultra[1];
	firstUltra[2] = ultra[2];

        char secondUltra[3];
	secondUltra[0] = ultra[4];
	secondUltra[1] = ultra[5];
	secondUltra[2] = ultra[6];

        char three = ultra[3];        //The ',' symbol
	char six = ultra[7];          //The '.' symbol

	int firstUltraDist;
	int secondUltraDist;
   
   if(three == ',' && six == '.'){

	int one = 1;
	int two = 2;

	
	firstUltraDist = converter(firstUltra, 3);
	sensors.firstUltrasonicDistance = firstUltraDist;
//	cout<<"Found First Ultra: "<<firstUltraDist<<endl;

	secondUltraDist = converter(secondUltra, 3);
	sensors.secondUltrasonicDistance = secondUltraDist;
//	cout<<"Found Second Ultra: "<<secondUltraDist<<endl;

    }


      }   //end of if statement when start byte is found
	} // end while

    return 0;
}

} // carolocup



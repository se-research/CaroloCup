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


pthread_t t1, t2;

unsigned char starter[1];
unsigned char infra[13];
unsigned char ultra[7];
unsigned short int lenght = 2;

int fd;
int timeCount = 0;


///////////////////////////////////////Hall Effect
int pins[3] = {0, 0, 0};
int pinsNew[3] = {0, 0, 0};
int movement = 0;

void *map_base, *virt_addr;
int fm;

int *results; 
int resultSaved[3] = {0, 0, 0}; 
////////////////////////////////////////Hall Effect


 typedef struct{

   int firstInfraredDistance;
   int secondInfraredDistance;
   int thirdInfraredDistance;
   int fourthInfraredDistance;

   int firstUltrasonicDistance;
   int secondUltrasonicDistance;

 }allSensors;

//////////////////Shared Memory//////////////
    int shmid;
    key_t key = 5678;
    allSensors *getData;
     int follow = 0;



namespace carolocup {

    using namespace std;
    using namespace core::base;
    using namespace core::data;


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

   if ((shmid = shmget(key, sizeof(allSensors), IPC_CREAT | 0666)) < 0) {
        cerr<<"Couldn't Create Shared Memory"<<endl;
    }

    if ((getData = (allSensors *)shmat(shmid, (void *)0, 0)) == (allSensors *) -1) {
        cerr<<"Couldn't Attach to Memory"<<endl;
    }

	fd = open("/dev/ttyACM0", O_RDWR | O_NOCTTY | O_NDELAY);
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
     
   while (getModuleState() == ModuleState::RUNNING) {

    
    tcflush(fd, TCIOFLUSH);
///////////////////////READ FROM PORT//////////////////////////////////

  while(1){

  sleep(0.005);

  timeCount++;
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


int n;
    if(timeCount == 20){
       n = read(fd, starter, 1);

///////////////////////////////////////////////////gatherData.setUltrasonicDistance(1,  10);
///////////////////////////////////////////////////gatherData.setUltrasonicDistance(2,  20);

gatherData.setUltrasonicDistance(1,  10);
gatherData.setUltrasonicDistance(2,  20);

}

        if(n < 0){
        fputs("read failed!\n", stderr);


        getData->firstInfraredDistance = 5;
   	getData->secondInfraredDistance = 10;
   	getData->thirdInfraredDistance = 15;
   	getData->fourthInfraredDistance = 20;

   	getData->firstUltrasonicDistance = 25;
   	getData->secondUltrasonicDistance = 30;
//////////////////////////////////////////////////cout<<"\nGOT ULTRASONIC BACK  "<<gatherData.getUltrasonicDistance(2)<<endl;
cout<<"\nGOT ULTRASONIC BACK  "<<gatherData.getUltrasonicDistance(2)<<endl;

Container contSensor(Container::USER_DATA_3, gatherData);
        // Send containers.
        getConference().send(contSensor);
        }


    if(starter[0] == 'i'){

    int n = read(fd, infra, 12);


	//char firstInfra[2] = {(infra[0]),(infra[1])};
 	//string lala(firstInfra, 2);
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
	int three = 3;
	int four = 4;
   	if(firstInfra[0] == '0'){
	cout<<"Found First: "<<firstInfra[1]<<endl;
	firstInfraDist = firstInfra[1];
        gatherData.setInfraredDistance(one, firstInfraDist);
	getData->firstInfraredDistance = firstInfraDist;
 	}

	else if(firstInfra[0] != '0'){

	firstInfraDist = atoi(firstInfra);
        gatherData.setInfraredDistance(one, firstInfraDist);
        cout<<"More than Ten First: "<<firstInfraDist<<endl;
	getData->firstInfraredDistance = firstInfraDist;
         }

        if(secondInfra[0] == '0'){
	cout<<"Found Second: "<<secondInfra[1]<<endl;
	secondInfraDist = secondInfra[1];
        gatherData.setInfraredDistance(two, secondInfraDist);
   	getData->secondInfraredDistance = secondInfraDist;
 	}

	else if(secondInfra[0] != '0'){
	secondInfraDist = atoi(secondInfra);
        gatherData.setInfraredDistance(two, secondInfraDist);
        cout<<"More than Ten Second: "<<secondInfraDist<<endl;
   	getData->secondInfraredDistance = secondInfraDist;
         }

        if(thirdInfra[0] == '0'){
	cout<<"Found Third: "<<thirdInfra[1]<<endl;
	thirdInfraDist = thirdInfra[1];
        gatherData.setInfraredDistance(three, thirdInfraDist);
   	getData->thirdInfraredDistance = thirdInfraDist;
 	}

	else if(thirdInfra[0] != '0'){
	thirdInfraDist = atoi(thirdInfra);
        gatherData.setInfraredDistance(three, thirdInfraDist);
        cout<<"More than Ten Third: "<<thirdInfraDist<<endl;
   	getData->thirdInfraredDistance = thirdInfraDist;
         }

        if(fourthInfra[0] == '0'){
	cout<<"Found Fourth: "<<fourthInfra[1]<<endl;
	fourthInfraDist = fourthInfra[1];
        gatherData.setInfraredDistance(four, fourthInfraDist);
   	getData->fourthInfraredDistance = fourthInfraDist;
 	}

	else if(fourthInfra[0] != '0'){
	fourthInfraDist = atoi(fourthInfra);
        gatherData.setInfraredDistance(four, fourthInfraDist);
        cout<<"More than Ten Fourth: "<<fourthInfraDist<<endl;
   	getData->fourthInfraredDistance = fourthInfraDist;
         }
    }  //End of main if

    //  }   //end of if statement when start byte is found
    
    if(starter[0] == 'u'){

    int n = read(fd, ultra, 6);

	char firstUltra[2];
	firstUltra[0] = ultra[0];
	firstUltra[1] = ultra[1];

        char secondUltra[2];
	secondUltra[0] = ultra[3];
	secondUltra[1] = ultra[4];

        char three = ultra[2];        //The ',' symbol
	char six = ultra[5];          //The '.' symbol

	int firstUltraDist;
	int secondUltraDist;
   
   if(three == ',' && six == '.'){

	int one = 1;
	int two = 2;

   	if(firstUltra[0] == '0'){
	cout<<"Found First Ultra: "<<firstUltra[1]<<endl;
	firstUltraDist = firstUltra[1];
	//gatherData.setUltrasonicDistance(&one, &firstUltraDist);
   	getData->firstUltrasonicDistance = firstUltraDist;
 	}
        else if(firstUltra[0] != '0'){
	firstUltraDist = atoi(firstUltra);
        gatherData.setUltrasonicDistance(one, firstUltraDist);
        cout<<"More than Ten First Ultra: "<<firstUltraDist<<endl;
   	getData->firstUltrasonicDistance = firstUltraDist;
         }
        if(secondUltra[0] == '0'){
	cout<<"Found Second Ultra: "<<secondUltra[1]<<endl;
	secondUltraDist = secondUltra[1];
	gatherData.setUltrasonicDistance(two, secondUltraDist);
   	getData->secondUltrasonicDistance = secondUltraDist;
 	}
        else if(secondUltra[0] != '0'){
	secondUltraDist = atoi(secondUltra);
        gatherData.setUltrasonicDistance(two, secondUltraDist);
        cout<<"More than Ten Second Ultra: "<<secondUltraDist<<endl;
   	getData->secondUltrasonicDistance = secondUltraDist;
         }

    }


      }   //end of if statement when start byte is found

        Container contSensor(Container::USER_DATA_3, gatherData);
        // Send containers.
        getConference().send(contSensor);

        } //If statement controlled by Time count

    } //End of infinite while loop


  } //End of ModuleState::RUNNING

    	return ModuleState::OKAY;
    }


/* int Sensors::converter(char* arrayInput){
     	int num = 0;

	for(int i = 0;i < lenght; i++){

	num = (arrayInput[i]*pow(10, lenght-i)+num);

	}

    return num/10;  

   } *///End of Converter function  


/* Initializes the the pins and opens a thread for continual reading. To be
 * called by Erlang 
*/
/*void initialize_pin_reading()
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

}*/	//End of Map_Pins

/* Sets up a specific pin for reading 
*/


/*void setup_gpiopin(int channel, int bit, int value, int pullval) {
	div_t div_res;
	unsigned char val, tmp, hld;
	unsigned char * base;
                               
	base = (map_base + channel) - GPIO_GPCONREG;
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
	base = (map_base + channel) + GPIO_UPDOWN;
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
} */    //////End of setup_gpiopin

/* Reads a specific pin e.g. pin 27, pin29, pin31 etc
*/


/*int read_gpio_pin(int channel, int bit )
{ 
  int input;
  input = *(unsigned char *) (map_base + channel);
	if (input & (1 << bit)) {
    return 1;	
	} else {
    return 0;
	}
}*/ ////End of read_gpio_pin

/* Thread that runs as a loop, continually reading pinws 27, 29 and 31 and
 * saving their data
*/


/*void* loop_retrieving(void *arg)
{

  while(1)
  {
    
    pinsNew[0] = read_gpio_pin(PIN27Channel, PIN27Bit);
    pinsNew[1] = read_gpio_pin(PIN31Channel, PIN31Bit);
    pinsNew[2] = read_gpio_pin(PIN29Channel, PIN29Bit);


    if(is_movement())
    {
      movement += calculate_movement(pins_state(pins), pins_state(pinsNew));
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
 
}*/      /////End of loop_retrieving

/* Checks if a pin's value has changed, indicating movement
*/

/*int is_movement()
{
    if(pins[0] != pinsNew[0])
      return 1;
    else if(pins[1] != pinsNew[1])
      return 1;
    else if(pins[2] != pinsNew[2])
      return 1;
    else
      return 0;
}*/   //////End of is_movement



/*int *get_gpio_data()
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

}*/   ///End of *get_gpio_data




/* Calculates distance that the car has moved
*/
/*int calculate_movement(int before, int after)
{

 if (before == after)
    return 0;
 else if (after > before)
    return after - before;
 else if (after < before)     
    return after - before + 6;
}*/  /////End of  calculate_movement;


} // carolocup


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

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <math.h>
#include "SensorData.h"
#include "Sensors.h"


#include <bitset>

#define XV11_CHECKSUM_INDEX 20

using namespace std;


pthread_t t1, t2;

unsigned char starter[1];
unsigned char infra[13];
unsigned char ultra[7];
unsigned short int lenght = 2;

int fd;


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

  sleep(0.1);

       int n = read(fd, starter, 1);

///////////////////////////////////////////////////gatherData.setUltrasonicDistance(1,  10);
///////////////////////////////////////////////////gatherData.setUltrasonicDistance(2,  20);

        if(n < 0){
        fputs("read failed!\n", stderr);

//////////////////////////////////////////////////cout<<"\nGOT ULTRASONIC BACK  "<<gatherData.getUltrasonicDistance(2)<<endl;
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

   	if(firstInfra[0] == '0'){
	cout<<"Found First: "<<firstInfra[1]<<endl;
	firstInfraDist = firstInfra[1];
        gatherData.setInfraredDistance(1, firstInfraDist);
 	}

	else if(firstInfra[0] != '0'){
	firstInfraDist = converter(firstInfra);
         }

        if(secondInfra[0] == '0'){
	cout<<"Found Second: "<<secondInfra[1]<<endl;
	secondInfraDist = secondInfra[1];
        gatherData.setInfraredDistance(2, secondInfraDist);
 	}

	else if(secondInfra[0] != '0'){
	secondInfraDist = converter(secondInfra);
         }

        if(thirdInfra[0] == '0'){
	cout<<"Found Third: "<<thirdInfra[1]<<endl;
	thirdInfraDist = thirdInfra[1];
        gatherData.setInfraredDistance(3, thirdInfraDist);
 	}

	else if(thirdInfra[0] != '0'){
	thirdInfraDist = converter(thirdInfra);
         }

        if(fourthInfra[0] == '0'){
	cout<<"Found Fourth: "<<fourthInfra[1]<<endl;
	fourthInfraDist = fourthInfra[1];
        gatherData.setInfraredDistance(4, fourthInfraDist);
 	}

	else if(fourthInfra[0] != '0'){
	fourthInfraDist = converter(fourthInfra);
         }
    }  //End of main if

      }   //end of if statement when start byte is found
    
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

   	if(firstUltra[0] == '0'){
	cout<<"Found First: "<<firstUltra[1]<<endl;
	firstUltraDist = firstUltra[1];
 	}
        else if(firstUltra[0] != '0'){
	firstUltraDist = converter(firstUltra);
         }
        if(secondUltra[0] == '0'){
	cout<<"Found Second: "<<secondUltra[1]<<endl;
	secondUltraDist = secondUltra[1];
 	}
        else if(secondUltra[0] != '0'){
	secondUltraDist = converter(secondUltra);
         }

    }


      }   //end of if statement when start byte is found

    
        Container contSensor(Container::USER_DATA_3, gatherData);
        // Send containers.
        getConference().send(contSensor);

    } //End of infinite while loop


  } //End of ModuleState::RUNNING

    	return ModuleState::OKAY;
    }


 int Sensors::converter(char* arrayInput){
     	int num = 0;

	for(int i = 0;i < lenght; i++){

	num = (arrayInput[i]*pow(10, lenght-i)+num);

	}

    return num/10;  

   } //End of Converter function    


} // carolocup


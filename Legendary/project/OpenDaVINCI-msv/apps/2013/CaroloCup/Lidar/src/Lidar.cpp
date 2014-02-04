/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

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
#include "LidarData.h"
#include "Lidar.h"


#include <bitset>

#define XV11_CHECKSUM_INDEX 20



using namespace std;


unsigned char buffer[22];

unsigned char starter[1];

unsigned char byte1, byte2, byte3, byte4, byte5, 
	      byte6, byte7, byte8, byte9, byte10, 
              byte11, byte12, byte13, byte14, byte15, byte16, 
              byte17, byte18, byte19, byte20, byte21, byte22;

unsigned char buffing[22] = {byte1, byte2, byte3, byte4, byte5, 
	      byte6, byte7, byte8, byte9, byte10, 
              byte11, byte12, byte13, byte14, byte15, byte16, 
              byte17, byte18, byte19, byte20, byte21, byte22};

unsigned int sid, sra1, sra2, srb1, srb2, src1, src2, srd1, srd2;

int fd; 			//File descriptor for the port
bool portState = false;

int fow = 0;


typedef struct{

  unsigned int readingIndex;
  unsigned int degree;
  unsigned int distance;
}reading;

reading Distance1;			//The first distance info from the packet 
reading *Pointer1 = &Distance1;	//Pointer to the first distance

reading Distance2;			//The second distance info from the packet 
reading *Pointer2 = &Distance2;	//Pointer to the second distance

reading Distance3;			//The third distance info from the packet 
reading *Pointer3 = &Distance3;	//Pointer to the third distance

reading Distance4;			//The fourth distance info from the packet 
reading *Pointer4 = &Distance4;	//Pointer to the fourth distance

typedef struct {

  unsigned int readingIndex;
  unsigned int firstDegree;
  unsigned int firstDistance;
  unsigned int secondDegree;
  unsigned int secondDistance;
  unsigned int thirdDegree;
  unsigned int thirdDistance;
  unsigned int fourthDegree;
  unsigned int fourthDistance;
} lidarForward;

//////////////////Shared Memory//////////////
    int Lidarshmid;
    key_t Lidarkey = 5555;
    lidarForward *getLidarData;
     int follow = 0;




namespace carolocup {

    using namespace std;
    using namespace core::base;
    using namespace core::data;


    Lidar::Lidar(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "Lidar"),
        m_fifo()
{}

    Lidar::~Lidar() {}

    void Lidar::setUp() {
        // This method will be call automatically _before_ running body().
    }

    void Lidar::tearDown() {
        // This method will be call automatically _after_ return from body().
    }

    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE Lidar::body() {

   if ((Lidarshmid = shmget(Lidarkey, sizeof(lidarForward), IPC_CREAT | 0666)) < 0) {
        cerr<<"Couldn't Create Shared Memory"<<endl;
    }

    if ((getLidarData = (lidarForward *)shmat(Lidarshmid, (void *)0, 0)) == (lidarForward *) -1) {
        cerr<<"Couldn't Attach to Memory"<<endl;
    }
 

   fd = open("/dev/ttyUSB0", O_RDWR | O_NOCTTY | O_NDELAY);

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

    	while (getModuleState() == ModuleState::RUNNING) {
   //LidarData data;
   LidarData sendData;

///////////////////////READ FROM PORT//////////////////////////////////
 // while(1){
   
  //sleep(0.00001);
 // cout << "Waiting..." <<endl;
//count++;
/////////////////////////////////////////////////////
  clock_t start = clock(); 

       int n = read(fd, starter, 1);
/*data.setIndex(1);
data.setFirstDeg(2);
data.setFirstDist(3);
data.setSecondDeg(4);
data.setSecondDist(5);
data.setThirDeg(6);
data.setThirdDist(7);
data.setFourthDeg(8);
data.setFourthDist(9);*/

        if(n < 0){
        fputs("read failed!\n", stderr);
   fow++;

/*cout<<"LALALALALAALALAALALALAAL:   "<<data.getDistance().readingIndex<<endl;
cout<<"LALALALALAALALAALALALAAL:   "<<data.getDistance().firstDegree<<endl;
cout<<"LALALALALAALALAALALALAAL:   "<<data.getDistance().firstDistance<<endl;
cout<<"LALALALALAALALAALALALAAL:   "<<data.getDistance().secondDegree<<endl;
cout<<"LALALALALAALALAALALALAAL:   "<<data.getDistance().secondDistance<<endl;
cout<<"LALALALALAALALAALALALAAL:   "<<data.getDistance().thirdDegree<<endl;
cout<<"LALALALALAALALAALALALAAL:   "<<data.getDistance().thirdDistance<<endl;
cout<<"LALALALALAALALAALALALAAL:   "<<data.getDistance().fourthDegree<<endl;
cout<<"LALALALALAALALAALALALAAL:   "<<data.getDistance().fourthDistance<<endl;*/

if(fow<100 || fow > 500){
	getLidarData->readingIndex = 10;
        getLidarData->firstDegree = 20;
        getLidarData->firstDistance = 30;
        getLidarData->secondDegree = 40;
	getLidarData->secondDistance = 50;
	getLidarData->thirdDegree = 60;
	getLidarData->thirdDistance = 70;
	getLidarData->fourthDegree = 80;
	getLidarData->fourthDistance = 90;
}

if(fow > 100 && fow < 500){
	getLidarData->readingIndex = 10;
        getLidarData->firstDegree = 20;
        getLidarData->firstDistance = 3000;
        getLidarData->secondDegree = 40;
	getLidarData->secondDistance = 5000;
	getLidarData->thirdDegree = 60;
	getLidarData->thirdDistance = 7000;
	getLidarData->fourthDegree = 80;
	getLidarData->fourthDistance = 9000;
}


 //Create container for sending the Lidar data
        Container contData(Container::USER_DATA_2, sendData);
        // Send containers.
        getConference().send(contData);


        }

    if(starter[0] == 0xFA){

         int p = read(fd, buffer, sizeof(buffer));
 
                int i = 0;

	byte1 = starter[0]; byte2 = buffer[i]; byte3 = buffer[i+1]; byte4 = buffer[i+2];
	byte5 = buffer[i+3]; byte6 = buffer[i+4]; byte7 = buffer[i+5]; byte8 = buffer[i+6];
	byte9 = buffer[i+7]; byte10 = buffer[i+8]; byte11 = buffer[i+9]; byte12 = buffer[i+10];
	byte13 = buffer[i+11]; byte14 = buffer[i+12]; byte15 = buffer[i+13]; byte16 = buffer[i+14];
    	byte17 = buffer[i+15]; byte18 = buffer[i+16]; byte19 = buffer[i+17]; byte20 = buffer[i+18];
	byte21 = buffer[i+19]; byte22 = buffer[i+20];	


	if(validate_buffer() == true){

	sid = byte2;
	sra1 = byte5; sra2 = byte6;
	srb1 = byte9; srb2 = byte10;
	src1 = byte13; src2 = byte14;
	srd1 = byte17; srd2 = byte18;

	getDistances(sid, sra1, sra2, srb1, srb2, src1, src2, srd1, srd2);
	//cout<<"YESSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS"<<endl;
	}

	else{
	//cout<<"NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"<<endl;
	}


	printf("Index: %d   Degree: %d   Distance: %d\n", Pointer1->readingIndex, Pointer1->degree, Pointer1->distance);
	printf("Index: %d   Degree: %d   Distance: %d\n", Pointer2->readingIndex, Pointer2->degree, Pointer2->distance);
	printf("Index: %d   Degree: %d   Distance: %d\n", Pointer3->readingIndex, Pointer3->degree, Pointer3->distance);
	printf("Index: %d   Degree: %d   Distance: %d\n\n\n", Pointer4->readingIndex, Pointer4->degree, Pointer4->distance);

      
	sendData.setIndex(Pointer1->readingIndex);
	sendData.setFirstDeg(Pointer1->degree);
	sendData.setFirstDist(Pointer1->distance);
	sendData.setSecondDeg(Pointer2->degree);
	sendData.setSecondDist(Pointer2->distance);
   	sendData.setThirDeg(Pointer3->degree);
	sendData.setThirdDist(Pointer3->distance);
   	sendData.setFourthDeg(Pointer4->degree);
	sendData.setFourthDist(Pointer4->distance);



 unsigned int readingIndex;
  unsigned int firstDegree;
  unsigned int firstDistance;
  unsigned int secondDegree;
  unsigned int secondDistance;
  unsigned int thirdDegree;
  unsigned int thirdDistance;
  unsigned int fourthDegree;
  unsigned int fourthDistance;

        getLidarData->readingIndex = Pointer1->readingIndex;
        getLidarData->firstDegree = Pointer1->degree;
        getLidarData->firstDistance = Pointer1->distance;
        getLidarData->secondDegree = Pointer2->degree;
	getLidarData->secondDistance = Pointer2->distance;
	getLidarData->thirdDegree = Pointer3->degree;
	getLidarData->thirdDistance = Pointer3->distance;
	getLidarData->fourthDegree = Pointer4->degree;
	getLidarData->fourthDistance = Pointer4->distance;

        //Create container for sending the Lidar data
        Container contData(Container::USER_DATA_2, sendData);
        // Send containers.
        getConference().send(contData);

////////////////////////////////////////////////////
//if(count == 90){
//clock_t end = clock();
//clock_t duration = end - start;
//cout<<"This is the number of clicks " << duration<<"\n"<<endl;
//cout<<"This is the duration " << (((float)duration)/CLOCKS_PER_SEC) * 1000.0<<"\n"<<endl;
//}

      }   //end of if statement when start byte is found

   // } //End of infinite while loop


  } //End of ModuleState::RUNNING

    	return ModuleState::OKAY;
    }

bool Lidar::validate_buffer(void){
    
    int incomming_checksum = (buffing[XV11_CHECKSUM_INDEX + 1] << 8) + buffing[XV11_CHECKSUM_INDEX];    
    
    int chk32 = 0;
    for(int i = 0; i < 10; i++)
        chk32 = (chk32 << 1) + (buffing[2*i + 1] << 8) + buffing[2*i + 0];
    
    int checksum = (chk32 & 0x7FFF) + (chk32 >> 15);
    checksum = checksum & 0x7FFF;
    
    return (checksum == incomming_checksum);
} //End of validate_buffer function

void Lidar::getDistances(unsigned int id, unsigned int ra1, unsigned int ra2, unsigned int rb1, unsigned int rb2, unsigned int rc1, unsigned int rc2, unsigned int rd1, unsigned int rd2){

  if(id < 160 || id > 249){
        return;
  }


	Pointer1->readingIndex = id;
	Pointer2->readingIndex = id;
	Pointer3->readingIndex = id;
	Pointer4->readingIndex = id;

	unsigned int firstDeg = (id - 160)*4;

///////////////////////////First Reading////////////////////////////////////

          if(ra2 & 0x80){
	  Pointer1->degree = firstDeg;
	  Pointer1->distance = 7000;
	   } //Low quality reading  
    
           else if(ra2 & 0x40){
	  Pointer1->degree = firstDeg;
	  Pointer1->distance = 8000;
            } //Questionable quality reading

            else{
	  Pointer1->degree = firstDeg;
          Pointer1->distance = (((ra2 & 0x3F) << 8) + ra1) / 10.0;
             } //Good reading

///////////////////////////Second Reading//////////////////////////////////

 	 if(rb2 & 0x80){
	Pointer2->degree = firstDeg+1;
	Pointer2->distance = 7000;
	   } //Low quality reading   
   
           else if(rb2 & 0x40){
	Pointer2->degree = firstDeg+1;
	Pointer2->distance = 8000;
            } //Questionable quality reading

            else{
	Pointer2->degree = firstDeg+1;
        Pointer2->distance = (((rb2 & 0x3F) << 8) + rb1) / 10.0;
             } //Good reading

////////////////////////Third Reading///////////////////////////////////

 	if(rc2 & 0x80){
	  Pointer3->degree = firstDeg+2;
	  Pointer3->distance = 7000;		
	   } //Low quality reading      
           else if(rc2 & 0x40){
	  Pointer3->degree = firstDeg+2;
	  Pointer3->distance = 8000;		
            } //Questionable quality reading

            else{
	  Pointer3->degree = firstDeg+2;
          Pointer3->distance = (((rc2 & 0x3F) << 8) + rc1) / 10.0;     
             } //Good reading

//////////////////////Fourth Reading///////////////////////////////////

 	if(rd2 & 0x80){
	  Pointer4->degree = firstDeg+3;
	  Pointer4->distance = 7000;		
	 } //Low quality reading      

           else if(rd2 & 0x40){
	  Pointer4->degree = firstDeg+3;
	  Pointer4->distance = 8000;		
            } //Questionable quality reading

            else{
	 Pointer4->degree = firstDeg+3;
         Pointer4->distance = (((rd2 & 0x3F) << 8) + rd1) / 10.0;     
             } //Good reading

     }//End of get distance function

} // carolocup


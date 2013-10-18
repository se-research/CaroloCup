/********************************************************
 * Author:    Zlatan Habul
 * Created:   18.10.2013
 * 
 * main() is only for testing,
 * starusb() shuld be called first.
 ********************************************************/

#include <stdio.h>
#include <wchar.h>
#include <string.h>
#include <stdlib.h>
#include<pthread.h>
#include "hidapi.h"
#include "hidtest.h"
#include <unistd.h>
#include <math.h>
#include <time.h>
#include <sys/time.h>

pthread_t tid;

struct tetrixCar {
	bool run;
	bool signaLightOn;
  	int speed;
	int angle;
	int leftSignalLight;
	int rightSignalLight;
	int brakeLight;
	int modeSwitch;
	int remoteStatus;
	double axelSpeed;
	double voltage;
	double current;
	int heading;
	int irSensor0;
	int irSensor1;
	int ultraSonic;
	char *displayRow1;
	char *displayRow2;
	char output[64];
	long mTime;
	long oldTime;
	hid_device *handle;
};


char signalOnOff(void *mem){
  char leds = 0;
  struct tetrixCar *ptr =(struct tetrixCar*)mem;
  ptr->mTime=current_timestamp();
  if ((ptr->mTime)>(ptr->oldTime)+10) {
	ptr->signaLightOn = not ptr->signaLightOn;
	ptr->oldTime=ptr->mTime;
	}
	
  if (ptr->brakeLight==0) leds = leds & 254;
  else if (ptr->brakeLight==1) leds=leds | 1;

  if (ptr->rightSignalLight==0) leds = leds & 253;
  else if (ptr->rightSignalLight==1) {
	if (ptr->signaLightOn==true) leds=leds | 2;
	else leds=leds & 253;
  }
	
  if (ptr->leftSignalLight==0)leds=leds & 251;
  else if (ptr->leftSignalLight==1) {
	if (ptr->signaLightOn==true) leds=leds | 4;
	else leds=leds & 251;
  }
  return leds;
}
long current_timestamp() {
    struct timeval te; 
    gettimeofday(&te, NULL); // get current time
    long long milliseconds = te.tv_sec*1000LL + te.tv_usec/1000; // caculate milliseconds
    
    milliseconds=(milliseconds-1381000000000)/100;
    //printf("milliseconds: %lld\n", milliseconds);
    return milliseconds;
}


void controll(void *mem) {
        struct tetrixCar *ptr =(struct tetrixCar*)mem;
	int y =0,z=0,res=0;
	char cSpeed[6],cAngle[6],cLeds[6];
	unsigned char buf[64],words[32][32];
	char leds =signalOnOff(ptr); 
	if (!(ptr->handle)) printf("\nDevice not found\n");
	leds=leds&7;
	sprintf (cSpeed, "%d", ptr->speed); //convert int to string
	sprintf (cAngle, "%d", ptr->angle);
	sprintf (cLeds, "%d", leds);
	strcpy(ptr->output,cSpeed);
	strcat (ptr->output,","); 
	strcat (ptr->output,cAngle); //put srting together
	strcat (ptr->output,",");
	strcat (ptr->output,ptr->displayRow1);  
	strcat (ptr->output,",");
	strcat (ptr->output,ptr->displayRow2);
	strcat (ptr->output,",");
	strcat (ptr->output,cLeds);    
	hid_write((ptr->handle), (unsigned char*)(ptr->output), 64);

	while (res == 0) {	
		res=hid_read((ptr->handle), buf, 64);
	}
	for (int i = 0;i<64;i++){ //pharse string into small strings
	
		if (buf[i]==','){
			words[y][i-z]='\0';				
			y++;
			z=i+1;
		} else {
		words[y][i-z]=buf[i];
		}
	}
	ptr->axelSpeed =(atof((const char*)words[0])/100);  //speed m/s
	ptr->voltage =(atof((const char*)words[1])/10); //voltage V
	ptr->current =(atof((const char*)words[2])/10); //current A
	ptr->heading =atoi((const char*)words[3]); //ultra sonic sensor cm
	ptr->ultraSonic =atoi((const char*)words[4]); //ultra sonic sensor cm
	ptr->remoteStatus =atoi((const char*)words[5]); //remote 1 = on
	ptr->modeSwitch=atoi((const char*)words[6]); //defaul mode = 0
	ptr->irSensor0=atoi((const char*)words[7]); //irsensor 1
	ptr->irSensor1=atoi((const char*)words[8]); //irsensor 2

}

void* communicationThread(void *mem)
{
   struct tetrixCar *ptr =(struct tetrixCar*)mem;
    while (ptr->run==true)
	{
		controll(mem);	//communicate
		usleep(5000);
	}
    hid_close((hid_device*)(ptr->handle));
    hid_exit();
    return NULL;
}

void *startUsb()
{	
	struct tetrixCar *ptr=NULL;
	//Init values inside struct
	ptr=(struct tetrixCar *) malloc(sizeof(struct tetrixCar));
	ptr->run = false;
	ptr->signaLightOn= false;
  	ptr->speed = 0;
	ptr->angle = 0;
	ptr->leftSignalLight = 0;
	ptr->rightSignalLight = 0;
	ptr->brakeLight=0;
	ptr->modeSwitch=0;
	ptr->remoteStatus=0;
	ptr->axelSpeed=0;
	ptr->voltage=0;
	ptr->current =0;
	ptr->heading = 0;
	ptr->irSensor0=0;
	ptr->irSensor1=0;
	ptr->ultraSonic=0;		
	ptr->displayRow1=(char*)malloc(16 * sizeof(char));
	ptr->displayRow2=(char*)malloc(16 * sizeof(char));
	ptr->handle =hid_open(0x0088, 0x0005, NULL);
	//start thread
	pthread_create(&tid, NULL, &communicationThread, ptr);
	return ptr;
}
void stopUsb(void *mem)
{
	struct tetrixCar *ptr =(struct tetrixCar*)mem;	
	ptr->run = false;
}
int main(int argc, char *argv[])
{	
	unsigned int nbytes = 32;
	int bytes_read;
	char *str;
	str = (char *) malloc (nbytes+1);	
	struct tetrixCar *ptr=(struct tetrixCar*)startUsb();	
	ptr->run = true;
	
	while (str[0]!='x'){
		printf ("Enter speed value, x to exit:");
  		bytes_read=getline (&str, (size_t*)&nbytes, stdin);
		str[bytes_read-1]=0;
		setDisplay("Hello",ptr);
		//setSpeed(atoi(str),ptr);
		setAngle(atoi(str),ptr);
		setBrakeLight(0,ptr);
		setRightSignalLight(1,ptr);
		setLeftSignalLight(1,ptr);
		printf("Speed(m/s): %.1f; ",getAxelSpeed(ptr));
		printf("Voltage(V): %.1f; ",getVoltage(ptr));
		printf("Current(A): %.1f; ",getCurrent(ptr));
		printf("Distance(cm): %d; ",getUltraSonic(ptr));
		printf("Remote: %d;",getRemoteStatus(ptr));
		printf("Mode: %d;",getModeSwitch(ptr));
		printf("Front IR sensor: %.1f;",getIrSensor0(ptr));
		printf("Back IR sensor: %.1f;",getIrSensor1(ptr));
		printf("Compas(Heading): %d;",getHeading(ptr));
		printf("\n");
	}
	stopUsb(ptr); //Stop usb communication
	return 0;
}
void setLeftSignalLight(int val,void *mem){
	struct tetrixCar *ptr =(struct tetrixCar*)mem;
	if (val==0) ptr->leftSignalLight = 0;
	else ptr->leftSignalLight = 1;
}
void setRightSignalLight(int val,void *mem){
	struct tetrixCar *ptr =(struct tetrixCar*)mem;
	if (val==0) ptr->rightSignalLight = 0;
	else ptr->rightSignalLight = 1;
}
void setBrakeLight(int val,void *mem){
	struct tetrixCar *ptr =(struct tetrixCar*)mem;
	if (val==0) ptr->brakeLight=0;
	else ptr->brakeLight=1;
}
void setDisplay(char *value,void *mem){
	int j =0;
	int l = 0;
	struct tetrixCar *ptr =(struct tetrixCar*)mem;
	l =strlen(value);
	for (int i = 0;i<16;i++){
	   if (i>(l-1)) ptr->displayRow1[i]=0x20;
	   else ptr->displayRow1[i]=value[i];
	}  	 
	ptr->displayRow1[16]=0;
	for (int i = 16;i<32;i++){
	   if (i>(l-1)) ptr->displayRow2[j]=0x20;
	   else ptr->displayRow2[j]=value[i];
	   j++;
	}
	ptr->displayRow2[16]=0;              	
}
void setSpeed(int val,void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 ptr->speed=val;
}
void setAngle(int val,void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 ptr->angle=val;
}
double getAxelSpeed(void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->axelSpeed;
}
int getModeSwitch(void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->modeSwitch;
}
int getRemoteStatus(void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->remoteStatus;
}
double getVoltage(void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->voltage;
}
double getCurrent(void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->current;
}
int getHeading(void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->heading;
}
double getIrSensor0(void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 double r =(6.0934*pow(10,-5)*pow(ptr->irSensor0,2)-0.07*ptr->irSensor0+24.08);
 return r;
}
double getIrSensor1(void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 double r =(6.0934*pow(10,-5)*pow(ptr->irSensor1,2)-0.07*ptr->irSensor1+24.08);
 return r;
}
int getUltraSonic(void *mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->ultraSonic;
}





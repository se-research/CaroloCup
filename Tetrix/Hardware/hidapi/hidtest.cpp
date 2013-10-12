/********************************************************
 * Author:    Zlatan Habul
 * Created:   12.10.2013
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
bool run = false;
bool signaLightOn= false;


char signalOnOff(){
  char leds = 0;
  mTime=current_timestamp();
 if (mTime>oldTime+10) {
	signaLightOn = not signaLightOn;
	oldTime=mTime;
	}
	
  if (brakeLight==0) leds = leds & 254;
  else if (brakeLight==1) leds=leds | 1;

  if (rightSignalLight==0) leds = leds & 253;
  else if (rightSignalLight==1) {
	if (signaLightOn==true) leds=leds | 2;
	else leds=leds & 253;
  }
	
  if (leftSignalLight==0)leds=leds & 251;
  else if (leftSignalLight==1) {
	if (signaLightOn==true) leds=leds | 4;
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

void controll(void *hwnd) {
	int y =0,z=0,res=0;
	char cSpeed[6],cAngle[6],cLeds[6];
	unsigned char buf[64],words[32][32];
	char leds =signalOnOff(); 
	hid_device *handle=(hid_device*)hwnd;
	if (!handle) printf("\nDevice not found\n");
	leds=leds&7;
	sprintf (cSpeed, "%d", speed); //convert int to string
	sprintf (cAngle, "%d", angle);
	sprintf (cLeds, "%d", leds);
	strcpy(output,cSpeed);
	strcat (output,","); 
	strcat (output,cAngle); //put srting together
	strcat (output,",");
	strcat (output,displayRow1);  
	strcat (output,",");
	strcat (output,displayRow2);
	strcat (output,",");
	strcat (output,cLeds);    
	hid_write(handle, (unsigned char*)output, 64);

	while (res == 0) {	
		res=hid_read(handle, buf, 64);
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
	axelSpeed =(atof((const char*)words[0])/100);  //speed m/s
	voltage =(atof((const char*)words[1])/10); //voltage V
	current =(atof((const char*)words[2])/10); //current A
	heading =atoi((const char*)words[3]); //ultra sonic sensor cm
	ultraSonic =atoi((const char*)words[4]); //ultra sonic sensor cm
	remoteStatus =atoi((const char*)words[5]); //remote 1 = on
	modeSwitch=atoi((const char*)words[6]); //defaul mode = 0
	irSensor0=atoi((const char*)words[7]); //irsensor 1
	irSensor1=atoi((const char*)words[8]); //irsensor 2

}

void* communicationThread(void *hwnd)
{
    while (run==true)
	{
		controll(hwnd);	//communicate
		usleep(5000);
	}
    hid_close((hid_device*)hwnd);
    hid_exit();
    return NULL;
}

void startUsb()
{	
	displayRow1=(char*)malloc(16 * sizeof(char));
	displayRow2=(char*)malloc(16 * sizeof(char));
	void *hwnd =hid_open(0x0088, 0x0005, NULL);
	pthread_create(&tid, NULL, &communicationThread, hwnd);
}
void stopUsb()
{
	run = false;
}
int main(int argc, char *argv[])
{	
	unsigned int nbytes = 32;
	int bytes_read;
	char *str;
	char cSpeed[6];
	str = (char *) malloc (nbytes+1);	
	run = true;
	startUsb();
	while (str[0]!='x'){
		printf ("Enter speed value, x to exit:");
  		bytes_read=getline (&str, &nbytes, stdin);
		str[bytes_read-1]=0;
		sprintf (cSpeed, "%.1f", getAxelSpeed());
		setDisplay("Hello");
		setSpeed(atoi(str));
		//setAngle(atoi(str));
		setBrakeLight(0);
		setRightSignalLight(1);
		setLeftSignalLight(1);
		printf("Speed(m/s): %.1f; ",getAxelSpeed());
		printf("Voltage(V): %.1f; ",getVoltage());
		printf("Current(A): %.1f; ",getCurrent());
		printf("Distance(cm): %d; ",getUltraSonic());
		printf("Remote: %d;",getRemoteStatus());
		printf("Mode: %d;",getModeSwitch());
		printf("Front IR sensor: %.1f;",getIrSensor0());
		printf("Back IR sensor: %.1f;",getIrSensor1());
		printf("Compas(Heading): %d;",getHeading());
		printf("\n");
	}
	stopUsb(); //Stop usb communication
	return 0;
}
void setLeftSignalLight(int val){
	if (val==0) leftSignalLight = 0;
	else leftSignalLight = 1;
}
void setRightSignalLight(int val){
	if (val==0) rightSignalLight = 0;
	else rightSignalLight = 1;
}
void setBrakeLight(int val){
	if (val==0) brakeLight=0;
	else brakeLight=1;
}
void setDisplay(char *value){
	int j =0;
	int l = 0;
	l =strlen(value);
	for (int i = 0;i<16;i++){
	   if (i>(l-1)) displayRow1[i]=0x20;
	   else displayRow1[i]=value[i];
	}  	 
	displayRow1[16]=0;
	for (int i = 16;i<32;i++){
	   if (i>(l-1)) displayRow2[j]=0x20;
	   else displayRow2[j]=value[i];
	   j++;
	}
	displayRow2[16]=0;              	
}
void setSpeed(int val){
 speed=val;
}
void setAngle(int val){
 angle=val;
}
double getAxelSpeed(){
 return axelSpeed;
}
int getModeSwitch(){
 return modeSwitch;
}
int getRemoteStatus(){
 return remoteStatus;
}
double getVoltage(){
 return voltage;
}
double getCurrent(){
 return current;
}
int getHeading(){
 return heading;
}
double getIrSensor0(){
 double r =(6.0934*pow(10,-5)*pow(irSensor0,2)-0.07*irSensor0+24.08);
 return r;
}
double getIrSensor1(){
 double r =(6.0934*pow(10,-5)*pow(irSensor1,2)-0.07*irSensor1+24.08);
 return r;
}
int getUltraSonic(){
 return ultraSonic;
}





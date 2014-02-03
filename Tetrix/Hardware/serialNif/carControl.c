/********************************************************
 * Author:    Zlatan Habul
 * Created:   27.01.2014
 * 
 * main() is only for testing,
 * startSerial() shuld be called first.
 ********************************************************/

#include <stdio.h>
#include <wchar.h>
#include <errno.h>
#include <termios.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>
#include "carControl.h"
#include <unistd.h>
#include <math.h>
#include <time.h>
#include <fcntl.h>
#include <sys/time.h>




struct tetrixCar {
	char run;
  	int speed;
	int angle;
	int dir;
	int leftSignalLight;
	int rightSignalLight;
	int remoteLight;
	int brakeLight;
	int beep;
	int modeSwitch1;
	int modeSwitch2;
	int remoteLeftRigh;
	int remoteUpDown;
	int remoteOnOff;
	double voltage;
	char *displayRow;
	char sendData[39];
	long mTime;
	long oldTime;
	int fd;
};

int set_interface_attribs (int fd, int speed, int parity)
{
        struct termios tty;
        memset (&tty, 0, sizeof tty);
        if (tcgetattr (fd, &tty) != 0)
        {
                printf ("error %d from tcgetattr", errno);
                return -1;
        }

        cfsetospeed (&tty, speed);
        cfsetispeed (&tty, speed);

        tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS8;     // 8-bit chars
        // disable IGNBRK for mismatched speed tests; otherwise receive break
        // as \000 chars
        tty.c_iflag &= ~IGNBRK;         // ignore break signal
	//tty.c_iflag &= IGNBRK;         // ignore break signal
        tty.c_lflag = 0;                // no signaling chars, no echo,
                                        // no canonical processing
        tty.c_oflag = 0;                // no remapping, no delays
        tty.c_cc[VMIN]  = 0;            // read doesn't block
        tty.c_cc[VTIME] = 5;            // 0.5 seconds read timeout

        tty.c_iflag &= ~(IXON | IXOFF | IXANY); // shut off xon/xoff ctrl

        tty.c_cflag |= (CLOCAL | CREAD);// ignore modem controls,
                                        // enable reading
        tty.c_cflag &= ~(PARENB | PARODD);      // shut off parity
        tty.c_cflag |= parity;
        tty.c_cflag &= ~CSTOPB;
        tty.c_cflag &= ~CRTSCTS;
	

        if (tcsetattr (fd, TCSANOW, &tty) != 0)
        {
                printf ("error %d from tcsetattr", errno);
                return -1;
        }
        return 0;
}

void set_blocking (int fd, int should_block)
{
        struct termios tty;
        memset (&tty, 0, sizeof tty);
        if (tcgetattr (fd, &tty) != 0)
        {
                printf ("error %d from tggetattr", errno);
                return;
        }

        tty.c_cc[VMIN]  = should_block ? 1 : 0;
        tty.c_cc[VTIME] = 5;            // 0.5 seconds read timeout

        if (tcsetattr (fd, TCSANOW, &tty) != 0)
                printf ("error %d setting term attributes", errno);
}



void controll(long mem) {
        struct tetrixCar *ptr =(struct tetrixCar*)mem;
	int i = 0;
	int light =0;
	int digVolt=0;
  	light= light | ptr->leftSignalLight;
	light= light | (ptr->rightSignalLight<<1);
  	light= light | (ptr->brakeLight<<2);
	light= light | (ptr->remoteLight<<3);
	light= light | (ptr->beep<<4);	
	ptr->sendData[1]=ptr->angle;
	ptr->sendData[2]=ptr->speed;
	ptr->sendData[3]=ptr->dir;
	ptr->sendData[4]=light;
	for (i = 5;i<37; i++) ptr->sendData[i]= ptr->displayRow[i-5];
	char recData [6]= {0,0,0,0,0,0};
	int recDataInt [6]= {0,0,0,0,0,0};		
	read (ptr->fd, recData ,6);
	for (i = 0;i<6; i++) recDataInt[i]= recData[i];
	recDataInt[0]&=0xFF;
	recDataInt[1]&=0xFF;
	recDataInt[2]&=0xFF;
	recDataInt[3]&=0xFF;
	recDataInt[4]&=0xFF;
	if ( recDataInt[0]==162 && recDataInt[4]==170) {
		write (ptr->fd, ptr->sendData, 39);
		ptr->remoteLeftRigh=(recDataInt[1]>>2)&3;
		ptr->remoteUpDown=recDataInt[1]&3;
		ptr->modeSwitch1=(recDataInt[1]>>5)&1;
		ptr->modeSwitch2=(recDataInt[1]>>6)&1;
		ptr->remoteOnOff=(recDataInt[1]>>4)&1;
		digVolt= digVolt| recDataInt[3]<<8;
		digVolt= digVolt| recDataInt[2];
		ptr->voltage= digVolt*0.015249;
	}

}

void* communicationThread(void *mem)
{
    struct tetrixCar *ptr =(struct tetrixCar*)mem;
 	while (ptr->run==1)
	{
		controll((long)mem);	//communicate
		usleep(2000);
	}
	close(ptr->fd);
    return NULL;
}

long startSerial()
{	
	long addr=0;
	char *portname= (char*) "/dev/ttyACM0";
	//char *portname = "/dev/ttySAC3";
  int i = 0;	
	struct tetrixCar *ptr=NULL;
	pthread_t tid;
	//Init values inside struct
	ptr=(struct tetrixCar *) malloc(sizeof(struct tetrixCar));
	ptr->run = 1;
  	ptr->speed = 255;
	ptr->angle = 90;
	ptr->dir = 0;
	ptr->sendData[0]=170;
	for (i = 1;i<37; i++) ptr->sendData[i]=0;
	ptr->sendData[37]=162;
	ptr->sendData[38]='0';
	ptr->leftSignalLight = 0;
	ptr->rightSignalLight = 0;
	ptr->remoteLight = 0;
	ptr->brakeLight=0;
	ptr->beep=0;
	ptr->modeSwitch1=0;
	ptr->modeSwitch2=0;
	ptr->remoteUpDown=0;
	ptr->remoteLeftRigh=0;
	ptr->remoteOnOff=0;
	ptr->voltage=0;
	ptr->displayRow=(char*)malloc(32 * sizeof(char));
	ptr->fd = open (portname, O_RDWR| O_NONBLOCK | O_NDELAY );
	if (ptr->fd < 0)
	{
        	printf ("error %d opening %s: %s", errno, portname, strerror (errno));
        	return -1;
	}
	set_interface_attribs (ptr->fd, B38400, 0);   // set speed to 115,200 bps, 8n1 (no parity)
	set_blocking (ptr->fd,0); 
	pthread_create(&tid, NULL, &communicationThread, ptr);
	addr= (long)ptr;
	return addr;
}
void stopSerial(long mem)
{
	struct tetrixCar *ptr =(struct tetrixCar*)mem;	
	ptr->run = 0;
}

void setLeftSignalLight(int val,long mem){
	struct tetrixCar *ptr =(struct tetrixCar*)mem;
	if (val==0) ptr->leftSignalLight = 0;
	else ptr->leftSignalLight = 1;
}
void setRightSignalLight(int val,long mem){
	struct tetrixCar *ptr =(struct tetrixCar*)mem;
	if (val==0) ptr->rightSignalLight = 0;
	else ptr->rightSignalLight = 1;
}
void setBrakeLight(int val,long mem){
	struct tetrixCar *ptr =(struct tetrixCar*)mem;
	if (val==0) ptr->brakeLight=0;
	else ptr->brakeLight=1;
}
void setRemoteLight(int val,long mem){
	struct tetrixCar *ptr =(struct tetrixCar*)mem;
	if (val==0) ptr->remoteLight=0;
	else ptr->remoteLight=1;
}
void setBeep(int val,long mem){
	struct tetrixCar *ptr =(struct tetrixCar*)mem;
	if (val==0) ptr->beep=0;
	else ptr->beep=1;
}
void setDisplay(char *value,long mem){
	int l = 0;
	int i = 0;
	struct tetrixCar *ptr =(struct tetrixCar*)mem;
	l =strlen(value);
	for (i = 0;i<32;i++){
	   if (i>(l-1)) ptr->displayRow[i]=0x20;
	   else ptr->displayRow[i]=value[i];
	}  	 
	ptr->displayRow[32]=0;             	
}
void setSpeed(int val,long mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 ptr->speed=val;
}
void setAngle(int val,long mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 ptr->angle=val;
}
void setDirection(int val,long mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 ptr->dir=val;
}
double getVoltage(long mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->voltage;
}
int getSwitch1(long mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->modeSwitch1;
}
int getSwitch2(long mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->modeSwitch2;
}
int getRemoteY(long mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->remoteUpDown;
}
int getRemoteX(long mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->remoteLeftRigh;
}
int getRemoteStatus(long mem){
 struct tetrixCar *ptr =(struct tetrixCar*)mem;
 return ptr->remoteOnOff;
}
int main(int argc, char *argv[])
{	
	int tmp = 49;       	
	long addr=startSerial();
	unsigned int nbytes = 32;
	int bytes_read;
	char *str;
	str = (char *) malloc (nbytes+1);	
	struct tetrixCar *ptr=(struct tetrixCar*)addr;	
	ptr->run = 1;	
	while (str[0]!='x'){

		printf ("Enter speed value, x to exit:");
  		bytes_read=getline (&str, (size_t*)&nbytes, stdin);
		str[bytes_read-1]=0;
		//setDisplay("Hello",(long)ptr);
		//setDirection(atoi(str),(long)ptr);
		//setSpeed(90,(long)ptr);
		setAngle(atoi(str),(long)ptr);
		setBeep(atoi(str),(long)ptr);
		setBrakeLight(atoi(str),(long)ptr);
		setRightSignalLight(1,(long)ptr);
		setLeftSignalLight(1,(long)ptr);
		//setDisplay(str,(long)ptr);
		printf("Voltage(V): %f; ",getVoltage((long)ptr));
		printf("\n");
		tmp++;
		setDisplay((char*)"Sta Hello",(long)ptr);
		//printf("%f\n", getVoltage((long)ptr));
		//usleep(1000);
	}
 	setDirection(4,(long)ptr);
	setAngle(90,(long)ptr);
	usleep(500000);
	stopSerial(addr);
	return 0;
}





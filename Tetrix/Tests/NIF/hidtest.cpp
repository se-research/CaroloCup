

#include <stdio.h>
#include <wchar.h>
#include <string.h>
#include <stdlib.h>
#include "hidapi.h"

void controll(int speed, int angle,void *hwnd,double *output) {
	int y =0,z=0,res=0;
	char cSpeed[64],cAngle[64];
	unsigned char buf[64],words[32][32];

	hid_device *handle=(hid_device*)hwnd;
	if (!handle) printf("Device not found");
	
	sprintf (cSpeed, "%d", speed); //convert int to string
	sprintf (cAngle, "%d", angle);
	strcat (cSpeed,","); 
	strcat (cSpeed,cAngle); //put srting together
	hid_write(handle, (unsigned char*)cSpeed, 64);
	while (res == 0) {	
		res=hid_read(handle, buf, 64);
	}
	//printf("test %s \n ",buf);
	for (int i = 0;i<64;i++){ //pharse string into small strings
	
		if (buf[i]==','){
			words[y][i-z]='\0';				
			y++;
			z=i+1;
		} else {
		words[y][i-z]=buf[i];
		}
	}
	output[0] =atof((const char*)words[0]);  //speed
	output[1] =(atof((const char*)words[1])/10); //voltage
	output[2] =(atof((const char*)words[2])/10); //current
	output[3] =atoi((const char*)words[3]); //remote
}
void *startUsb()
{	
	hid_device *handle=hid_open(0x0088, 0x0005, NULL);	
	return handle;
}
void stopUsb(void *hwnd)
{
	hid_close((hid_device*)hwnd);
	hid_exit();
}
int main(int argc, char *argv[])
{	
		
	int speed = atoi(argv[1]);
	int angle = atoi(argv[2]);
	double output[4]={0,0,0,0};

	void *hwnd=startUsb();
	

	controll(speed, angle,hwnd,output);	//communicate

	printf("Speed(m/s): %.1f; ",output[0]);
	printf("Voltage(V): %.1f; ",output[1]);
	printf("Current(A): %.1f; ",output[2]);
	printf("Remote: %.0f;",output[3]);
	printf("\n");
	stopUsb(hwnd); //Stop usb communication
	return 0;
}

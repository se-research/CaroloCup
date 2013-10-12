/********************************************************
 * Author:    Zlatan Habul
 * Created:   12.10.2013
 * This arduino MPU is for experimenting
 ********************************************************/
#include <Wire.h>
char sendData[20];
void setup()
{
  Serial.begin(9600); // Initialize the serial port.
  strcpy(sendData,"0,0,0,0,0|");
  Wire.begin(55); // Start the I2C interface.
  Wire.onRequest(requestEvent);
}

void loop()
{
  serialReceive();
  delay(10);
  //Serial.println(sendData);
}




void serialReceive()
{
   char inByte = 0;
   int index =0;
   char inData[20];
   boolean fCheck = false;
   while(Serial.available() > 0){
    
    if(index < 20) {
        inByte = Serial.read();
        if (inByte=='#') {
          fCheck = true;
        }
        else if (inByte=='|') {
          inData[index] = '\0';
          if (fCheck == true) {
            strcat(inData,"|");
            strcpy(sendData,inData);
          }
          break;
        }
        else {
           // Read a Byte
	  inData[index] = inByte; // Store it
          index++;
        }
    }
   }
}
void requestEvent()
{
  Wire.write(sendData); // respond with message of 6 bytes
                       // as expected by master
}




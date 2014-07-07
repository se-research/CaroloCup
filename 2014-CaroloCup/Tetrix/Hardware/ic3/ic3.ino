#include <Wire.h>
#define leftRight 3
#define forwardBack 2
#define doingCollect 9 //pin 15
#define switch1 4 //pin 6
#define switch2 5 //pin 11
#define voltagePin A2//pin 25
int duration=0;
byte input[3] = {0,0,0};
int voltage = 0;
int check[4];
int servo=0;
boolean remote = false;
boolean remoteOn = false;
void setup()
{
  pinMode(leftRight, INPUT);
  pinMode(forwardBack, INPUT);
  pinMode(switch1, INPUT_PULLUP);
  pinMode(switch2, INPUT_PULLUP);
  pinMode(doingCollect, OUTPUT);
  Wire.begin(100);
  Wire.onRequest(requestEvent);
  digitalWrite(doingCollect, LOW);
  Serial.begin(9600);           // start serial for output
}

void loop()
{ 
  if (digitalRead(switch1)==LOW){
    input[0]|=32;
  }
  else {
    input[0]&=223;
  }
  if (digitalRead(switch2)==LOW){
    input[0]|=64;
  }
  else {
    input[0]&=191;
  }
  
  if (digitalRead(leftRight)==HIGH) remoteOn = true;
  if (remoteOn==true){
    input[0]|=16;
    servo = pulseIn(leftRight, HIGH,22000);
    if (servo >1) {
    duration = pulseIn(forwardBack, HIGH,22000);
     if (servo >1 && duration >1650 ) { //forward
        input[0]|=2;
        input[0]&=254;
     }
     else if (servo>1 && duration < 1300 ) { //back
        input[0]|=1;
        input[0]&=253;
     }
     else if (servo>1) { //neutral
        input[0]&=252;
     }
     if (servo>1 && servo>1700  ) { //left
        input[0]|=8;
        input[0]&=251;
     }
     else if (servo>1 && servo< 1350) { //right
       input[0]|=4;
       input[0]&=247;        
     }
     else if (servo>1) { //neutral
        input[0]&=243;
     }
    }
  }

  if (servo ==0) {
    remoteOn = false; 
    input[0]&=224;
  }
  voltage = analogRead(voltagePin);
  input[1]=voltage&255;
  input[2]=(voltage>>8)&255;
   
  delay(20);
}

void requestEvent()
{
  Wire.write(input,3);  
}
/*void sendToIC1(byte spd,byte mode)
{
  byte data[2];
  data[0]=spd;
  data[1]=mode;
  Wire.beginTransmission(90);
  Wire.write(data, 2);  
  Wire.endTransmission();
}
void sendToIC2(byte servo,byte led)
{
  byte data[2];
  data[0]=servo;
  data[1]=led;
  Wire.beginTransmission(110);
  Wire.write(data, 2);  
  Wire.endTransmission();
}*/

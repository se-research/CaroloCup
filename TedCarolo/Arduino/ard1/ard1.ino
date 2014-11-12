/*
Code for arduino that only receive data
Not using same library as sending one
Functions
loop()
decodeNetstring(String s)
handleReceivedData(String s)

*/

#include <Arduino.h>
#include <Stream.h>
#include <Servo.h>
#include "pb.h"
#include "pb_encode.h"
#include "pb_decode.h"
#include "simple.pb.h"
int leftBlinkLed = 33;
int breakLightLed = 35;
int rightBlinkLed = 37;
int ledGround = 41;
Servo motor;
Servo servo;

String inData = "";
int netStringBuffer[30];

// to send to UDOO
//arduinoToUdoo message;
int us_left = 0;
int us_center = 0;
int us_right = 0;
int ir_left = 0;
int ir_right_front = 0;
int ir_right_back = 0;

// to receive from UDOO
int speed = 0;
int steering = 127;
bool leftBlink = false;
bool rightBlink = false;
bool stopLight = false;


bool sendStatus;
bool receiveStatus;
int receiveMessage_length = 0;

byte sendBuffer[40];
byte receiveBuffer[30];// = {8,1,16,1,24,1,32,1,40,1};

//Servo 
Servo myservo;
int X, angle, zero, previous;

//Protobuff
byte c = 0;
byte buffer[10];
byte index = 0;    

//Motor
//Servo motor;

void setup(){
  Serial.begin(9600);
  //make constant
  zero = 37; 

  //90 for other car. 55 for our.
  myservo.attach(10);
  myservo.write(90);

  //motor
  //motor.attach(8);
  //motor.write(90);

  delay(4000);

}


void loop(){
  while (Serial.available() > 0){
    char recieved = Serial.read();
//  delay(30);
//  Serial.print(recieved);
    if (recieved != ','){
      inData += recieved; 
    }
        else{
            decodeNetstring(inData);
            inData = ""; // Clear recieved buffer
        }
        
    } 
}

void decodeNetstring(String s){
  String netLength = "";
  String payload = "";
  
  if(s[1] == ':'){
    netLength += s[0];
  }else if(s[2 == ':']){
    netLength += s[0];
    netLength += s[1];
  }
  if(netLength.toInt() <10){
      for(int i = 0; i< netLength.toInt(); i ++){
        payload+= s[i+2]; //int(s[i+2])-48;
      }
  }else if (netLength.toInt() < 100){
    for(int i = 0; i< netLength.toInt(); i ++){
        payload+= s[i+3]; //int(s[i+2])-48;
      }
  }
  
    handleReceivedData(payload); 
}

void handleReceivedData(String s){
  for(int i = 0; i< s.length(); i++){
    receiveBuffer[i] = s[i];
  }

  receiveMessage_length = s.length();
     /*      Serial.print("BUFFER ATM: ");
                for(int i = 0; i< receiveMessage_length; i++){
      Serial.print(receiveBuffer[i]);
                  Serial.print(",");
    }
                Serial.println();      */
  
    //--------------------------------------------------------------------Decode protobuf
  
  udooToArduino rm; //name of nevv message

  pb_istream_t receiveStream = pb_istream_from_buffer(receiveBuffer, receiveMessage_length);
  receiveStatus = pb_decode(&receiveStream, udooToArduino_fields, &rm);
  if (!receiveStatus){
    Serial.print("Decoding failed: ");
    Serial.println(PB_GET_ERROR(&receiveStream));
  }else{
    speed = rm.speed;
    steering = rm.steering;
    leftBlink = rm.leftBlink;
    rightBlink = rm.rightBlink;
    stopLight = rm.stopLight;
    //end of proto
    act();
  } 
}


void act(){
  Serial.print("s: ");
  Serial.print(speed);
  Serial.print(" st: ");
  Serial.print(steering);
  Serial.print(" lb: ");
  Serial.print(leftBlink);
  Serial.print(" rb: ");
  Serial.print(rightBlink);
  Serial.print(" sl: ");
  Serial.println(stopLight);
  
  /*
  blink();
  setSteering(steering); 
  setMotor(speed);
  */
}

void setSteering(int i){
  int temp = i/(3.64)+ 55;
  if(temp >= 55 && temp<=135){
   servo.write(temp);
  }
}
void setMotor(int i){
  if(i == 1)
  motor.write(70);
  else if(i == 2)
  motor.write(75);
  else if(i == 3)
  motor.write(80);
  else if(i == 4){
  motor.write(95);
  }else if(i == 5){
  motor.write(105);
  delay(100);
  motor.write(104);
  }else if(i == 6)
  motor.write(104);
  else if(i == 7)
  motor.write(110);
}

void blink(){
  if(leftBlink == true){
    digitalWrite(leftBlinkLed, HIGH);
  }else{
    digitalWrite(leftBlinkLed, LOW);
  }
  if(rightBlink == true){
    digitalWrite(rightBlinkLed, HIGH);
  }else{
    digitalWrite(rightBlinkLed, LOW);
  }
  if(stopLight == true){
    digitalWrite(breakLightLed, HIGH);
  }else{
    digitalWrite(breakLightLed, LOW);
  }
}

/*
  SimpleMessage message;
  bool test;
  pb_istream_t stream = pb_istream_from_buffer((uint8_t*)buffer, 10);
  test = pb_decode(&stream, SimpleMessage_fields, &message); //incoming buffer decoded to protocol

  if(!test){
    Serial.println(PB_GET_ERROR(&stream));
  }else{

    X = message.speed;
    
      angle = (X / 100);
      angle = angle - zero;
      
      //find right angle
      // 90 for other car, 55 for our. *3
      if(angle <= 1 && angle >= -25){
        angle = (((float)angle * -1)/ 0.83333) + 90;    
      }else if (angle >= 1 && angle <= 25){
        angle = 90 - ((float)angle/0.83333);
      }else{
        angle = 90;
      }

        //Set speed for motor and angles
        //motor.write(97);
        myservo.write(angle);
        Serial.println(myservo.read());     
        delay(10);
    
  }
}
*/ 
 

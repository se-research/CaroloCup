#include <Wire.h>
#define tranAup 5 //pin 11 //t1
#define tranAdown 6 //pin 12 //t4
#define tranBup 4 //pin 6 //t2
#define tranBdown 7 //pin 13 //t5
#define tranCup 3 //pin 5 //t3
#define tranCdown 8 //pin 14 //t6
#define halsensorA 9
#define halsensorB 10
#define halsensorC 11
boolean Amotor= false;
boolean Bmotor= false;
boolean Cmotor= false;
boolean applaybreake = false;
int mode = 4; //back =1,forward = 2,brake = 3;
int delaya =20000;//25;
int tLeave =300;;

void setup()
{
  pinMode(tranAup, OUTPUT);
  pinMode(tranBup, OUTPUT);
  pinMode(tranCup, OUTPUT);
  pinMode(tranAdown, OUTPUT);
  pinMode(tranBdown, OUTPUT);
  pinMode(tranCdown, OUTPUT);
  pinMode(halsensorC, INPUT);
  pinMode(halsensorB, INPUT);
  pinMode(halsensorA, INPUT);
  Wire.begin(90);
  Wire.onReceive(receiveEvent);
  Serial.begin(9600);
  noTurn();
}
void loop()
{ 
  getData ();
  if (digitalRead(halsensorA)==HIGH && digitalRead(halsensorB)==LOW &&digitalRead(halsensorC)==HIGH) //1_3
  {
    unsigned long time =millis();
    while(digitalRead(halsensorA)==HIGH && digitalRead(halsensorB)==LOW &&digitalRead(halsensorC)==HIGH){
     if (time+tLeave<millis()){ break;}
    switch (mode) {
    case 1:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, HIGH);//
      digitalWrite(tranAdown, HIGH);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, LOW);
  
      break;
    case 2:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, HIGH);//
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, HIGH);
      break;
    case 3:
      breakMotor(1);
      break;
    default: 
      noTurn();
    }}
    noTurn();
    if (mode!=3) delayMicroseconds(delaya);
 }
 else if (digitalRead(halsensorA)==LOW && digitalRead(halsensorB)==LOW &&digitalRead(halsensorC)==HIGH) //3
  {
    unsigned long time =millis();
    while(digitalRead(halsensorA)==LOW && digitalRead(halsensorB)==LOW &&digitalRead(halsensorC)==HIGH){
     if (time+tLeave<millis()){ break;}
    switch (mode) {
    case 1:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, HIGH);//
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, HIGH);
      digitalWrite(tranCdown, LOW);
      break;
    case 2:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, HIGH);//
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, HIGH);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, LOW);
      break;
    case 3:
     breakMotor(2);
      break;
    default: 
      noTurn();
    }}
    noTurn();
    if (mode!=3) delayMicroseconds(delaya);
 }
 else if (digitalRead(halsensorA)==LOW && digitalRead(halsensorB)==HIGH &&digitalRead(halsensorC)==HIGH) //2_3
  {
    unsigned long time =millis();
    while(digitalRead(halsensorA)==LOW && digitalRead(halsensorB)==HIGH &&digitalRead(halsensorC)==HIGH){
      if (time+tLeave<millis()){ break;}
    switch (mode) {
    case 1:
      digitalWrite(tranAup, HIGH);//
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, HIGH);
      digitalWrite(tranCdown, LOW);
      break;
    case 2:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, HIGH);//
      digitalWrite(tranAdown, HIGH);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, LOW);
      break;
    case 3:
      breakMotor(3);
      break;
    default: 
      noTurn();
    }}
    noTurn();
    if (mode!=3) delayMicroseconds(delaya);
 }
 else if (digitalRead(halsensorA)==LOW && digitalRead(halsensorB)==HIGH &&digitalRead(halsensorC)==LOW) //2
  {
    unsigned long time =millis();
    while(digitalRead(halsensorA)==LOW && digitalRead(halsensorB)==HIGH &&digitalRead(halsensorC)==LOW){
     if (time+tLeave<millis()){ break;}
    switch (mode) {
    case 1:
      digitalWrite(tranAup, HIGH);//
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, HIGH);
      break;
    case 2:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, HIGH);//
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, HIGH);
      digitalWrite(tranCdown, LOW);
      break;
    case 3:
       breakMotor(4);
      break;
    default: 
      noTurn();
    }}
    noTurn();
    if (mode!=3) delayMicroseconds(delaya);
 }
 else if (digitalRead(halsensorA)==HIGH && digitalRead(halsensorB)==HIGH &&digitalRead(halsensorC)==LOW) //1_2
  {
    unsigned long time =millis();
    while(digitalRead(halsensorA)==HIGH && digitalRead(halsensorB)==HIGH &&digitalRead(halsensorC)==LOW){
     if (time+tLeave<millis()){ break;}
    switch (mode) {
    case 1:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, HIGH);//
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, HIGH);
      break;
    case 2:
      digitalWrite(tranAup, HIGH);//
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, HIGH);
      digitalWrite(tranCdown, LOW);
      break;
    case 3:
      breakMotor(5);
      break;
    default: 
      noTurn();
    }}
    noTurn();
    if (mode!=3) delayMicroseconds(delaya);
 }
 else if (digitalRead(halsensorA)==HIGH && digitalRead(halsensorB)==LOW &&digitalRead(halsensorC)==LOW) //1
  {
    unsigned long time =millis();
    while(digitalRead(halsensorA)==HIGH && digitalRead(halsensorB)==LOW &&digitalRead(halsensorC)==LOW){
    if (time+tLeave<millis()){ break;}
    switch (mode) {
    case 1:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, HIGH);//
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, HIGH);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, LOW);
      break;
    case 2:
      digitalWrite(tranAup, HIGH);//
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, HIGH);
      break;
    case 3:
      breakMotor(6);
      break;
    default: 
      noTurn();
    }}
    noTurn();
    if (mode!=3) delayMicroseconds(delaya);
 }
 else noTurn();
}
void getData () {
  byte inByte;
 while(Serial.available() > 0){
        inByte = Serial.read();
        if  (inByte=='f') {
            mode=1;
            //Serial.println("Back");
            applaybreake = false; }
        else if  (inByte=='b') {
            mode=2;
            //Serial.println("Forward");
            applaybreake = false; }
        else if  (inByte=='s') {
            //Serial.println("Stop");
            mode = 3;
            }
        else if  (inByte=='a') {
            //Serial.println("Stop");
            mode = 4;
            }
    } 

}

 void noTurn() {
  digitalWrite(tranAup, LOW);
  digitalWrite(tranBup, LOW);
  digitalWrite(tranCup, LOW);
  digitalWrite(tranAdown, LOW);
  digitalWrite(tranBdown, LOW);
  digitalWrite(tranCdown, LOW);

}
void breakMotor(int mode) {
  boolean tmpAmotor;
  boolean tmpBmotor;
  boolean tmpCmotor;
  if (digitalRead(halsensorA)==LOW) tmpAmotor = false; else if (digitalRead(halsensorA)==HIGH) tmpAmotor=true;
  if (digitalRead(halsensorB)==LOW) tmpBmotor = false; else if (digitalRead(halsensorB)==HIGH) tmpBmotor=true;
  if (digitalRead(halsensorC)==LOW) tmpCmotor = false; else if (digitalRead(halsensorC)==HIGH) tmpCmotor=true;
  if (Amotor==tmpAmotor && Bmotor==tmpBmotor && Cmotor==tmpCmotor) {
    noTurn();
  }
  else {
    brakeMode(mode);
    if (digitalRead(halsensorA)==LOW) Amotor = false; else if (digitalRead(halsensorA)==HIGH) Amotor=true;
    if (digitalRead(halsensorB)==LOW) Bmotor = false; else if (digitalRead(halsensorB)==HIGH) Bmotor=true;
    if (digitalRead(halsensorC)==LOW) Cmotor = false; else if (digitalRead(halsensorC)==HIGH) Cmotor=true;
    delay(15);
  }
  
  
}
void brakeMode(int mode) {
    switch (mode) {
    case 1:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, HIGH);//
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, HIGH);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, LOW);
      break;
    case 2:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, HIGH);//
      digitalWrite(tranAdown, HIGH);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, LOW);
      break;
    case 3:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, HIGH);//
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, HIGH);
      digitalWrite(tranCdown, LOW);
      break;
    case 4:
      digitalWrite(tranAup, HIGH);//
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, HIGH);
      digitalWrite(tranCdown, LOW);
      break;
    case 5:
      digitalWrite(tranAup, HIGH);//
      digitalWrite(tranBup, LOW);
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, HIGH);
      break;
    case 6:
      digitalWrite(tranAup, LOW);
      digitalWrite(tranBup, HIGH);//
      digitalWrite(tranCup, LOW);
      digitalWrite(tranAdown, LOW);
      digitalWrite(tranBdown, LOW);
      digitalWrite(tranCdown, HIGH);
      break;
    default: 
      noTurn();
    }
}
void receiveEvent(int howMany)
{
   delaya = Wire.read();
   delaya=delaya*100;
   mode = Wire.read();
}

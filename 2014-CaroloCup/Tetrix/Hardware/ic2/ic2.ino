#include <Wire.h>
#include <Servo.h>
#include <LiquidCrystal.h>
#define servoPin 3
#define brakeLight 2 //pin 4
#define remoteLight 4 //pin 6
#define yellowLeft 6 //pin 12
#define yellowRight 5 //pin 11
#define check 14 //pin A0
#define dispRS 9 //pin 15
#define dispDB4 7 //pin 13
#define dispDB5 10 //pin 16
#define dispDB6 8//pin 14
#define dispDB7 12 //pin 18
#define beepPin 11 //pin 17
#define dispE 13 //pin 19
Servo myservo;
LiquidCrystal lcd(dispRS, dispE, dispDB4, dispDB5, dispDB6, dispDB7);
String oldString;
char recvStr[33];
char empty[17];
byte inBuf[39];
byte outBuf[4]= {0,0,0,0};

void setup()
{
  //Wire.begin(110);
  pinMode(brakeLight, OUTPUT);
  pinMode(remoteLight, OUTPUT);
  pinMode(yellowLeft, OUTPUT);
  pinMode(yellowRight, OUTPUT);
  //pinMode(check, INPUT);
  //Wire.onReceive(receiveEvent);
  Wire.begin();
  myservo.attach(servoPin);
  lcd.begin(16, 2);
  lcd.noCursor();
  lcd.clear();
  digitalWrite(yellowLeft, LOW);
  digitalWrite(yellowRight, LOW);
  digitalWrite(brakeLight, LOW);
  digitalWrite(remoteLight, LOW);
  strcpy(recvStr , " ");
  writeToDisplay(recvStr);
  for (int i = 0; i<39; i++) inBuf[i]=0;
  Serial.begin(38400);
}
void loop() {
 char input = 0;
  if (Serial.available() > 0) {
    Serial.readBytes((char*)inBuf, 39);
  }
   if (inBuf[0]==170 && inBuf[37]==162) { 
     myservo.write(inBuf[1]); //servo
     sendToMotor(inBuf[2],inBuf[3]);//spd, dir
     ledCOntroll(inBuf[4]); //led
    for (int i = 0; i<32;i++) {
      recvStr[i] = inBuf[i+5];
     }
     writeToDisplay(recvStr); 
     //inBuf[0]==0;
  }
  
 if (digitalRead(check)==LOW) {
    Wire.requestFrom(100, 3);   
    if(Wire.available()>0)    
    {
      outBuf[0]=162;
      outBuf[1]= Wire.read();
      outBuf[2]= Wire.read();
      outBuf[3]= Wire.read();
      outBuf[4]=170;
      outBuf[5]=0;
    }
  }
  Serial.write(outBuf,6);
  delay(3);
}
/*void receiveEvent(int nr){
  
  byte controll =Wire.read();
  switch (controll) {
    case 124:
       servo = Wire.read();
       led = Wire.read();
       break;
    default:
       strcpy(recvStr , " ");
       recvStr[0]=controll;
       for (int i = 1; i<32;i++) {
         if (i<nr) {        
           recvStr[i] = Wire.read();
         }
         else {
           recvStr[i] =32;
         }
       }
       recvStr[32]==0;
   }
     
}*/
void ledCOntroll(byte light) {
  byte left = 0;
  byte right = 0;
  byte brake = 0;
  byte remote =0;
  byte beep =0;
  light = light & 31;
  left = light &1;
  right = (light & 2) >>1;
  brake = (light & 4) >>2;
  remote = (light & 8) >>3;
  beep = (light & 16) >>4;
  if (left ==1) digitalWrite(yellowLeft, HIGH);
  else digitalWrite(yellowLeft, LOW);
  if (right ==1) digitalWrite(yellowRight, HIGH);
  else digitalWrite(yellowRight, LOW);
  if (brake ==1) digitalWrite(brakeLight, HIGH);
  else digitalWrite(brakeLight, LOW);
  if (remote ==1) digitalWrite(remoteLight, HIGH);
  else digitalWrite(remoteLight, LOW);
  if (beep ==1) analogWrite(beepPin, 50);  
  else analogWrite(beepPin, 0);  
}
void writeToDisplay(String string)
{
  if (oldString==string) return;
  int length = string.length();
  oldString=string;
  String row1 = string.substring(0,16);
  String row2 = string.substring(16);
  lcd.clear();
  lcd.print(row1);
  lcd.setCursor(0, 1);
  lcd.print(row2);    
}
void sendToMotor(byte s,byte mode)
{
  byte data[2];
  data[0]=s;
  data[1]=mode;
  Wire.beginTransmission(90);
  Wire.write(data, 2);  
  Wire.endTransmission();
}


/********************************************************
 * Author:    Zlatan Habul
 * Created:   12.10.2013
 * This arduino MPU is
 * Controling motor, stering
 * Output is voltage, current, ultrasonic cm, heading
 * Heading is calculated using magnometer and accelerometer
 * Also i2c master 
 ********************************************************/
#include <ADXL345.h>
#include <HMC5883L.h>
#include <Servo.h>
#include <Wire.h>
#include <SonarSRF08.h>


#define dirMotorForwaPin 2
#define remoteLedPin 3
#define dirMotorReverPin 4
#define cycleMotorReverPin 5
#define cycleMotorForwaPin 6
#define steerInputPin 7
#define moveInputPin 8
#define servoPin 14
#define currentReading A1
#define voltageReading A2
#define serialBaudRate 115200

// Store our compass as a variable.
HMC5883L compass;
// Declare a global instance of the accelerometer.
ADXL345 accel;
//Servo
Servo myservo;
//UltraSonic
SonarSRF08 MySonar;

int USAddress = 248;
int areConnected = 0; // Store our connection status here.
unsigned long remoteSteer;
unsigned long remoteMove;
unsigned long timeRemoteLed =0;
unsigned long time =0;
unsigned long oldTime =0;
boolean show = false;
boolean readMode = false;
boolean steerRemoteCount = false;
boolean hallSensorCount = false;
boolean ledOnOff = false;
boolean remoteOn=false;
boolean tDelay=false;
double power =0;
double currentValue = 0;
double voltageValue =0;
int steering = 90;
int motorSpeed = 90;
int steeringmode = 0;
int countStartCycles =0;
int mode =0;
int speedMap = 13000;
int speedCycle = 90;
int maxPower = 180;
int rotationSpeed=0;
int ultraSonic=0;
byte rStatusM=0;
byte rStatusS=0;
String getHallData;






void setup()
{
  pinMode(cycleMotorReverPin, OUTPUT);
  pinMode(cycleMotorForwaPin, OUTPUT);
  pinMode(dirMotorForwaPin, OUTPUT);
  pinMode(dirMotorReverPin, OUTPUT);
  pinMode(remoteLedPin, OUTPUT);
  myservo.attach(servoPin);
  Serial.begin(serialBaudRate);
  Wire.begin();
  compass = HMC5883L(); // Construct a new HMC5883 compass.
  accel = ADXL345(); // Construct a new ADXL345 accelerometer.

  compass.EnsureConnected();
  accel.EnsureConnected();
  getHallData = "0,0,0,0,0";
  if(compass.IsConnected && accel.IsConnected)
  {
    areConnected = true;
    Serial.println("Connected to HMC5883L and ADXL345.");
  }
  else
  {
    areConnected = false;
    if(compass.IsConnected == 0)
      Serial.println("Could not connect to HMC5883L.");
    if(accel.IsConnected == 0)
      Serial.println("Could not connect to ADXL345.");
  }

  if(areConnected)
  { 
    compass.SetScale(1.3); // Set the scale of the compass.
    compass.SetMeasurementMode(Measurement_Continuous); // Set the measurement mode to Continuous

    accel.SetRange(2, true); // Set the range of the accelerometer to a maximum of 2G.
    accel.EnableMeasurements(); // Tell the accelerometer to start taking measurements.
    MySonar.connect();  
    MySonar.changeAddress(0x00, USAddress, 0x00, 0xFF);
    USAddress += 4; 
  }
}

int getPowerMode (double power) {
  if (power >11) return 1;
  else if (power >10.5) return 2;
  else if (power >10) return 3;
  else if (power >9.5) return 4;
  else if (power >9) return 5;
  else if (power >8.5) return 6;
  else return 0;
}
boolean getSerialData(int *motorData, int *steerData)
{
   int sI=0;
   int y =0;
   int steer;
   int motor;
   byte inByte = 0;
   byte index = 0;
   unsigned int endIndex = 0;
   char cSdata[4];
   char cMdata[4];
   char inData[8];
   char tmpData[8];
   boolean gotData = false;
   boolean fCheck = false;
   boolean sCheck = false;
   strcpy(inData," ");
   strcpy(cSdata," ");
   strcpy(cMdata," ");
   while(Serial.available() > 0){
    
    if(index < 10) {
        inByte = Serial.read();
        if (inByte=='%') {
          fCheck = true;
        }
        else if (inByte=='|') {
          sCheck = true;
          gotData = true;
          endIndex=index;
          break;
        }
        else {
           // Read a Byte
	  inData[index] = inByte; // Store it
          index++;
        } 
    }
    gotData = true;
   }
  for (int i = 0; i<endIndex;i++) tmpData[i]=inData[i];
  tmpData[endIndex]='\0';
  strcpy(inData,tmpData);
   if (gotData == true && fCheck == true && sCheck == true){
      for (int i = 0; i <8;i++){
        if (inData[i]!=',') {
          cMdata[i]=inData[i];
        }
        else {
          sI=i;
          cMdata[i]='\0';
          break;
        }
      }
      motor = atoi(cMdata);
      if (motor>10) motor=10; //max value is 10
      if (motor<-10) motor=-10; //min value is 10
      motor +=90;
      *motorData= motor;
      
      for (int z = sI+1; z <8;z++){
        if (inData[z]!='\0') {
          cSdata[y]=inData[z];
          y++;
        }
        else {
          cSdata[z]='\0';
          break;
        }
      }
      steer=atoi(cSdata);
      if (steer>25) steer=25; //max value is 25
      if (steer<-25) steer=-25; //min value is -25
      steer+=90; 
      *steerData=steer;
      return true;
    }
   else {
     return false;
   } 

}
  
int mapSpeed(int vSpeed) 
{
  if (vSpeed<90) vSpeed=(90-vSpeed)+90;
  switch (vSpeed) {
    case 91:
       return 38;
       break;
    case 92:
       return 48;
       break;
    case 93:
       return 58;
       break;
    case 94:
       return 68;
       break;
    case 95:
       return 78;
       break;
    case 96:
       return 88;
       break;
    case 97:
       return 98;
       break;
    case 98:
       return 108;
       break;
    case 99:
       return 118;
       break;
    case 100:
       return 128;
       break;
  }
       
}
int getSpeedCycle( int steerMode, int motorMode, double power,int vSpeed){
  int speedCyc=0;
  int powerMode= getPowerMode (power);
  int tmp = mapSpeed(vSpeed)+powerMode*5;
  if ( steerMode ==0 && motorMode ==1) speedCyc=tmp; //forward
  else if ( steerMode >0 && motorMode ==1) speedCyc=tmp+20; //forward with steering
  else if ( steerMode ==0 && motorMode ==2) speedCyc=tmp+5; //rewerse
  else if ( steerMode >0 && motorMode ==2) speedCyc=tmp+25;; //rewerse with steering 
  return speedCyc;
}
void remoteLed(long time, boolean enable)
{
  if (enable == true) {
    if (time >= (timeRemoteLed+1000)) {
      timeRemoteLed=time;
      ledOnOff= not ledOnOff;
    }
      if (ledOnOff==true) {
        digitalWrite(remoteLedPin, HIGH);
      }
      else {
        digitalWrite(remoteLedPin, LOW);
      }
    }
   else digitalWrite(remoteLedPin, LOW);    
}
void requestDataFromHall() {
  char data[20];
  int count = 0;
  Wire.requestFrom(55, 20);
  while(Wire.available()) 
  {
    char c = Wire.read();
        if (c=='|') {
        data[count]='\0';
        getHallData=data;
        count = 0;
        break;
      }
      else {
        data[count]=c;
        count++;
      }
    
  }
  int ci1 = getHallData.indexOf(',');
  rotationSpeed = (getHallData.substring(0, ci1)).toInt();
 // ultraSonic = (getHallData.substring(ci1+1)).toInt();
}
void loop()
{
  char US_Compas[10];
  int tmpMotorData=0;
  int tmpSteerData=0;
  time = millis();
  

  
   boolean test=getSerialData(&tmpMotorData,&tmpSteerData);
 // detect remote controll and get input
  if (digitalRead(steerInputPin)==HIGH) remoteOn = true;
  if (remoteOn==true){
     remoteLed(time, true);
     remoteSteer = pulseIn(steerInputPin, HIGH,50000);
     remoteMove = pulseIn(moveInputPin, HIGH,50000);
     if (remoteSteer>1 && remoteSteer>1550  ) {
       steering = 115; //left 25 degree
     }
     else if (remoteSteer>1 && remoteSteer< 1350) {
       steering = 65; //right 25 degree
     }
     else if (remoteSteer>1) {
       steering = 90; //straight 0 degree
     }
     if (remoteSteer >1 && remoteMove >1600 ) {
       motorSpeed =92;
     }
     else if (remoteSteer>1 && remoteMove < 1350 ) {
       motorSpeed = 88;
     }
     else if (remoteSteer>1) {
       motorSpeed = 90;
    }
  }
  else {
    remoteLed(time, false);
      if (test ==true){
        steering=tmpSteerData;
        motorSpeed=tmpMotorData;
      } 
      remoteSteer=0;
      remoteMove=0;
  }
  if (remoteSteer==0) remoteOn = false;
  
 if (tDelay==false) {
    tDelay=true;
    MySonar.setUnit(0x00, USAddress, 'c', 0x00, 0xFF);
    oldTime=time+200;
  }
   else if (tDelay==true && time>oldTime) {
    MySonar.setRegister(USAddress, 0x02);
    ultraSonic = MySonar.readData(USAddress, 2);
    MagnetometerScaled magnetometerReadings = compass.ReadScaledAxis();
    AccelerometerScaled accelerometerReadings = accel.ReadScaledAxis();
    float headingNTC = CalculateHeadingNotTiltCompensated(magnetometerReadings);
    float headingTC = CalculateHeadingTiltCompensated(magnetometerReadings, accelerometerReadings);
    requestDataFromHall();
    sprintf(US_Compas,"%d,%d",(int)RadiansToDegrees(headingTC),ultraSonic);
    tDelay=false;
   }
   
  //get voltage and current
  currentValue = (524-analogRead(currentReading))/20.5;
  voltageValue = analogRead(voltageReading)*0.01517; //constant based on r1 and r2
  
  //chose steer mode
  if (steering>90) steeringmode =2;
  else if (steering<90) steeringmode =1;
  else steeringmode = 0;
  
  //give kickstart from begining then continue with standard speed   
  if ( motorSpeed >90 ) {
     if (mode ==0){
          countStartCycles++;
          mode=3;
        }
     else if ( mode ==3 && countStartCycles<1) { 
          countStartCycles++;
          mode = 3;
        }
     else {
          countStartCycles = 0;
          mode =1;
        }
  }
  else if (motorSpeed <90) {
     if (mode ==0){
          countStartCycles++;
          mode=4;
        }
     else if ( mode ==4 && countStartCycles<1) { 
          countStartCycles++;
          mode = 4;
        }
     else {
          countStartCycles = 0;
          mode =2;
        }
  }
  else {
         mode = 0;
         countStartCycles =0;
  }
 speedCycle=getSpeedCycle(steeringmode,mode,voltageValue,motorSpeed);
  Serial.print("%");
  Serial.print(rotationSpeed);
  Serial.print(",");
  Serial.print(int(voltageValue*10));
  Serial.print(",");
  Serial.print(int(currentValue*10));
  Serial.print(",");
  Serial.print(US_Compas);
  Serial.print(",");
  if (remoteOn==true) Serial.print("1");
  else Serial.print("0");
  Serial.print("|");
  
  // Movement, at one moment one par of trasistors is turnet ON and other par of transistor are turnet off  
  switch (mode) {
    case 1:
       analogWrite(cycleMotorForwaPin, speedCycle); //normal mode
       analogWrite(cycleMotorReverPin, 0); 
       digitalWrite(dirMotorForwaPin, HIGH); 
       digitalWrite(dirMotorReverPin, LOW);
       break;
    case 2:
       analogWrite(cycleMotorForwaPin, 0);
       analogWrite(cycleMotorReverPin, speedCycle); //normal mode
       digitalWrite(dirMotorForwaPin, LOW);
       digitalWrite(dirMotorReverPin, HIGH); 
       break;
    case 3:
       analogWrite(cycleMotorForwaPin, maxPower); //startup mode
       analogWrite(cycleMotorReverPin, 0); //stop
       digitalWrite(dirMotorForwaPin, HIGH);
       digitalWrite(dirMotorReverPin, LOW);
       break;
    case 4:
       analogWrite(cycleMotorForwaPin,0); //stop
       analogWrite(cycleMotorReverPin, maxPower); //startup mode
       digitalWrite(dirMotorForwaPin, LOW);
       digitalWrite(dirMotorReverPin, HIGH);
       break;
    default: 
       analogWrite(cycleMotorForwaPin, 0); //stop
       analogWrite(cycleMotorReverPin, 0); //stop
       digitalWrite(dirMotorForwaPin, LOW); //stop
       digitalWrite(dirMotorReverPin, LOW); //stop
     }
     
  // Servo, send info to servo motor
  myservo.write(steering);
  delay(5);
}

float CalculateHeadingTiltCompensated(MagnetometerScaled mag, AccelerometerScaled acc)
{
  // We are swapping the accelerometers axis as they are opposite to the compass the way we have them mounted.
  // We are swapping the signs axis as they are opposite.
  // Configure this for your setup.
  float accX = -acc.YAxis;
  float accY = -acc.XAxis;

  float rollRadians = asin(accY);
  float pitchRadians = asin(accX);

  // We cannot correct for tilt over 40 degrees with this algorthem, if the board is tilted as such, return 0.
  if(rollRadians > 0.78 || rollRadians < -0.78 || pitchRadians > 0.78 || pitchRadians < -0.78)
  {
    return 0;
  }

  // Some of these are used twice, so rather than computing them twice in the algorithem we precompute them before hand.
  float cosRoll = cos(rollRadians);
  float sinRoll = sin(rollRadians);  
  float cosPitch = cos(pitchRadians);
  float sinPitch = sin(pitchRadians);

  float Xh = mag.XAxis * cosPitch + mag.ZAxis * sinPitch;
  float Yh = mag.XAxis * sinRoll * sinPitch + mag.YAxis * cosRoll - mag.ZAxis * sinRoll * cosPitch;

  float heading = atan2(Yh, Xh);
   // Your mrad result / 1000.00 (to turn it into radians).
  float declinationAngle = 48.87  / 1000.0;
  // If you have an EAST declination, use += declinationAngle, if you have a WEST declination, use -= declinationAngle
  heading += declinationAngle;
  
  // Correct for when signs are reversed.
  if(heading < 0)
    heading += 2*PI;
    
  // Check for wrap due to addition of declination.
  if(heading > 2*PI)
    heading -= 2*PI;
   
  // Convert radians to degrees for readability.
  float headingDegrees = heading * 180/M_PI;

  return heading;
}

float CalculateHeadingNotTiltCompensated(MagnetometerScaled mag)
{
  // Calculate heading when the magnetometer is level, then correct for signs of axis.
  float heading = atan2(mag.YAxis, mag.XAxis);
  return heading;
}

float RadiansToDegrees(float rads)
{
  // Correct for when signs are reversed.
  if(rads < 0)
    rads += 2*PI;

  // Check for wrap due to addition of declination.
  if(rads > 2*PI)
    rads -= 2*PI;

  // Convert radians to degrees for readability.
  float heading = rads * 180/PI;

  return heading;
}

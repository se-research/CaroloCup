// Need the Servo library
#include <Servo.h>
#include <EEPROM.h>

#define MAX_STEERING_ANGLE_R 	43
#define MAX_STEERING_ANGLE_L 	-43
#define MIN_MOTOR_SPEED 		800
#define MAX_MOTOR_SPEED 		1623
#define MIN_REVERSE_SPEED 		1100
#define BRAKE_SPEED_MAX 		1541
#define BRAKE_SPEED_MIN 		1299
#define INIT_MOTOR_SPEED 		1500


// WHEEL ENCODER
#define WHEEL_DIAMETER            65.4
#define PI                        3.141592
#define WHEEL_CIRCUMFERENCE       WHEEL_DIAMETER * PI
#define WHEEL_ENCODER_SEGMENTS    22
#define DISTANCE_PER_SEGMENT      (float)WHEEL_CIRCUMFERENCE / WHEEL_ENCODER_SEGMENTS
#define WHEEL_ENCODER_PIN         0

//Identification
char sID[7];


// This is our motor.
Servo myMotor;
Servo mySteering;

int speeds[] = {INIT_MOTOR_SPEED,1545,1230,1540,1240,1565,1567,1570,1573,1580};
int nofSpeed = 10;
int reverseSpeeds[] = {INIT_MOTOR_SPEED,1290,1270,1250,1240,1230,1200}; 
int nofReverse = 7;

int motorPin = 6;
int steeringPin = 9;
int stopLed1 = 22;
int stopLed2 = 29;
int stopLed3 = 23;
int blueLed = 41;
int greLed=40;

int rearLeftLed = 24;
int frontLeftLed = 43;
int rearRightLed = 25;
int frontRightLed = 42;

long int lastTimeStamp;


int speed = INIT_MOTOR_SPEED;
int angle = 0;
int freq = 0;
int setFreq = 0;
int propGain = 1;
int intGain = 5;
int read;
int readbyte;

boolean run = true;
boolean first = true;
boolean motor = false;
boolean steering = false;
boolean brake = false;
boolean applybrake = false;
boolean cruiseCtrl = false;
boolean applyCruiseCtrl = false;
int reverse = 1;
int multiplier = 1;
boolean takeOver = false;
boolean indicators = false;

int ms;
unsigned long time;
unsigned long int cnt=0, cntOld = 0, cntBrake = 0;
float carSpeed = 0;
int takeOverTokens = 10;

int reading = 0;
boolean blinkingLeft = false;
boolean blinkingRight = false;
boolean stopBlinking = true;

int takenOverSpeed = INIT_MOTOR_SPEED;
int revertBackSpeed = false;
boolean fullMsgRecv = false;
boolean isDirSet = false;

void setup()
{
  // Setup the servos
  myMotor.attach(motorPin);
  mySteering.attach(steeringPin);

  // Set a startup speed
  myMotor.writeMicroseconds(975);
  mySteering.write(90);

  time = 0;
  
  attachInterrupt(WHEEL_ENCODER_PIN, countRotations, CHANGE);
  pinMode(2, INPUT);
  pinMode(3, INPUT);
  // initialize serial communication
  Serial.begin(115200);
  Serial.println("initialized");

  pinMode(greLed, OUTPUT);
  pinMode(stopLed1, OUTPUT);
  pinMode(stopLed2, OUTPUT);
  pinMode(stopLed3, OUTPUT);

  pinMode(rearLeftLed, OUTPUT);
  pinMode(frontLeftLed, OUTPUT);
  pinMode(rearRightLed, OUTPUT);
  pinMode(frontRightLed, OUTPUT);

  //Identification
  for (int i=0; i<6; i++) {
    sID[i] = EEPROM.read(i);
  }
  //Serial.println(sID);     
}

void loop()
{
  //time = millis();
  if(run) {
    myMotor.writeMicroseconds(speed);
    //Serial.print("speed: ");
    //Serial.println(speed);
    //Serial.print("angle: ");
    //Serial.println(90);
    speed = 0;
    run = false;
  }
  evaluateIndicators();
  evaluateReceiver();
  //Serial.print("speed reading: ");
  if(!takeOver) {
    if((read = Serial.available()) > 0) {
      //Read data
      while(read > 0) {
        readbyte = Serial.read();
        //Serial.println(readbyte);

        if (readbyte == '/') {
          fullMsgRecv = true;
          break;
        }
        
        //Motor command
        if (!first && motor) {
          speed = speed * 10 + (readbyte - '0');
        }
        if (first && readbyte == 'm') {
          motor = true;
          first = false;
          speed = 0;
          
        }
        
        //Steering command
        if (!first && steering){
          if(readbyte < 48) {
            multiplier = -1;
          }
          else {
            angle = angle * 10 + (readbyte - '0');
          }
        }
        if(first && readbyte == 's') {
          steering = true;
          first = false;
          angle = 0;
          multiplier = 1;
        }
        
        //Brake commands
        if(!first && brake) {
          if(readbyte == '+') {
            applybrake = true;
          }
          else {
            applybrake = false;
          }
        }
        if(first && readbyte == 'b') {
          brake = true;
          first = false;
        }
        
        //Cruise control
        if (!first && cruiseCtrl) {
          if (readbyte == 'f' || readbyte == 'r') {
            isDirSet = true;
            if(!applyCruiseCtrl) {
               applyCruiseCtrl = true;
            }
            if(readbyte == 'r') {
              reverse = -1;
              if(speed < MIN_REVERSE_SPEED || speed > BRAKE_SPEED_MIN) {
                  speed = 1250;
              }
             // Serial.println("Reverse");
            } else if(readbyte == 'f') {
              reverse = 1;
              if(speed < INIT_MOTOR_SPEED || speed > MAX_MOTOR_SPEED) {
                  speed = 1530;
              }
            //  Serial.println("Forward");
            }
          } else if (readbyte == '-') {
            applyCruiseCtrl = false;
            //Serial.println(reverse);
            if(reverse == 1) {
              speed = 1300;
              controlMotor();
              delay(10);
              carSpeed = 0;
              //speed = INIT_MOTOR_SPEED;
              //controlMotor();
            } else {
              speed = 1900;
              controlMotor();
              delay(50);
              carSpeed = 0;
              speed = INIT_MOTOR_SPEED;
              controlMotor();
            }
          } else if(readbyte >= '0' && readbyte <= '9' && isDirSet) {
            //applyCruiseCtrl = true;
            setFreq = setFreq * 10 + (readbyte - '0');
            //Serial.println(setFreq);
          }
        }
        if (first && readbyte == 'f') {
          cruiseCtrl = true;
          first = false;
          setFreq = 0;
        }
        
        //Indicators
        if(!first && indicators) {
          if(readbyte == 108) {
            blinkingLeft = true;
            stopBlinking = false;
          } else if(readbyte == 114){
            blinkingRight = true;
            stopBlinking = false;
          } else if(readbyte == 115) {
            stopBlinking = true;
          } else if(readbyte == 97) {
            blinkingLeft = true;
            blinkingRight = true;
            stopBlinking = false;
          }
        }
        if (first && readbyte == 'i') {
          indicators = true;
          first = false;
        }

        read = read - 1;
      }
      if(fullMsgRecv) {
        //Process data
        if(motor) {
          //Serial.println(speed);
          revertBackSpeed = true;
          if(speed >= 0 && speed < nofSpeed) {
          
            speed = speeds[speed];
            if(speed > MIN_MOTOR_SPEED && speed < MAX_MOTOR_SPEED) {
                controlMotor();
            }
          }
        }
        if(steering) {
          //Serial.println(angle);
          if(angle > MAX_STEERING_ANGLE_L && angle < MAX_STEERING_ANGLE_R) {
            controlSteering();
          }
        }

        if(brake) {
          if(applybrake) {
            brakeLeds(true);
          }
          else {
            brakeLeds(false);
          }
        }
      }
      first = true;
      motor = false;
      steering = false;
      brake = false;
      cruiseCtrl = false;
      indicators = false;
      multiplier = 1;
      fullMsgRecv = false;
    }
  }
  else {
    multiplier = 1;
    //Serial.println(angle);
    //Serial.println(speed);
    controlMotor();
    controlSteering();
  }
  //ms = ms + 1;
  //Calculate car speed
  //unsigned long curr = millis();
  //Serial.println((curr - time));
  //int diff = cnt * 1000 / (curr - time);
  //float newSpeed = diff * 0.03;
  //if(newSpeed != carSpeed) {
  //Serial.print("Car speed:");
  //Serial.println(carSpeed);
  //carSpeed = newSpeed;
  //}
  //freq = int(diff*1.2);
  if (applyCruiseCtrl) {
    //Serial.println("Enter cruise control");
    //Serial.print("Frequency: ");
    //Serial.println(freq);
    int goalSpeed = setFreq;
    int error = goalSpeed - 10*carSpeed;
   // Serial.print("Error: ");
  //  Serial.println(error);
    if(abs(error)<35 && abs(error) > 1) {
      error = error - 1;
      //int errorSign = error < 0 ? -1 : +1;
      /*if (abs(error) > 12) {
        speed += 5*errorSign;
        }
        else if (abs(error) > 6) {
        speed += 3*errorSign;
        } else*/
      /*int step = 0;
      if(error <= 0)  {
        step = error / 10;
      } else {
        step = error / 20;
      }*/
      if (abs(error) > 0 && ms > 10) {
        speed += reverse * error;
        ms=0;
      } else if(abs(error) > 0){
        ms++;
      } else if(abs(error) <= 0){
        ms=0;
      }
  
      if(reverse == 1) {
        speed = constrain(speed, 1530, MAX_MOTOR_SPEED);
      } else {
        speed = constrain(speed, MIN_REVERSE_SPEED, 1250);
      }
    }
    //Serial.print("Speed :");
    //Serial.println(speed);
    controlMotor();
  }
  //cntOld = cnt;
  //cnt = 0;
  //ms = 0;
  //time = curr;
  //}
  //Serial.print("Car speed:");
  //Serial.println(carSpeed);
  if(speed == 1300) {
    cntBrake++;
  }
  if(cntBrake > 100){
    speed = 1500;
    cntBrake = 0;
    controlMotor();
  }
  delay(10);
  //Serial.println((millis()-time));
}

void controlMotor() {
  //Serial.print("speed: ");
//  Serial.println(speed);
  myMotor.writeMicroseconds(speed);
  if(speed < BRAKE_SPEED_MAX && speed > BRAKE_SPEED_MIN) {
    brakeLeds(true);
  }
  else {
    brakeLeds(false);
  }
    if (speed == 0) {
                digitalWrite(greLed, HIGH);
            }else {
                digitalWrite(greLed, LOW); 
             }
}

void controlSteering() {
  //Serial.print("angle: ");
  int inpAngle = angle;
  angle = 90 + angle * multiplier * -1;
  //Serial.println(angle);
  mySteering.write(angle);
}

void evaluateReceiver()
{
  int receiverSpeed, receiverSteer;
  if(!takeOver) {
     receiverSpeed = pulseIn(2, HIGH, 5000);
     receiverSteer = pulseIn(3, HIGH, 5000);
  } else {
     receiverSpeed = pulseIn(2, HIGH, 100000);
     receiverSteer = pulseIn(3, HIGH, 100000);
  }
  receiverSpeed = map(receiverSpeed, 1400,2500,1000,2000);
  //set a constant speed here
  //INIT_MOTOR_SPEED zero 
  //1400 zero reverse
  if(receiverSpeed > 1580) {
    receiverSpeed = 1580;
  } else if(receiverSpeed < 1200) {
    receiverSpeed = 1200;
  }
  //receiverSteer = map(receiverSteer, 1600,2300,819,2219);
  //Serial.println(receiverSpeed);
  //Serial.println(receiverSteer);
  //if(receiverSpeed > 1170 && receiverSpeed < 1370 && !takeOver) {
  if(receiverSteer > 2100) {
    if((takeOverTokens--) == 0) {
      takeOver = true;
      if(revertBackSpeed) {
        takenOverSpeed = speed;
      }
      revertBackSpeed = false;
      digitalWrite(blueLed, LOW);
      lastTimeStamp = millis();
    }
  } else {
    takeOverTokens = 10;
  }
  if(takeOver) {
    speed = receiverSpeed;
    receiverSteer = (receiverSteer - 1959)/10;
    angle = receiverSteer; //* (-1);
    if (millis() - lastTimeStamp >= 500) {
      digitalToggle(blueLed);
      lastTimeStamp = millis();
    }
  }
  if((angle > 150 || angle < -150) && takeOver) {
    digitalWrite(blueLed, LOW);
    angle = 0;
    controlSteering();
    speed = takenOverSpeed;
    controlMotor();
    takeOver = false;
  }
}

int blinkCntL = 0, blinkCntR = 0;
void evaluateIndicators() {
  if(stopBlinking) {
    blinkingLeft = false;
    blinkingRight = false;
  }
  if(blinkingLeft) {
    if(blinkCntL++ > 25) {
      digitalToggle(rearLeftLed);
      digitalToggle(frontLeftLed);
      blinkCntL = 0;
    }
  } else {
    digitalWrite(rearLeftLed, LOW);
    digitalWrite(frontLeftLed, LOW);
    blinkCntL = 0;
  }
  if(blinkingRight) {
    if(blinkCntR++ > 25) {
      digitalToggle(rearRightLed);
      digitalToggle(frontRightLed);
      blinkCntR = 0;
    }
  } else {
    digitalWrite(rearRightLed, LOW);
    digitalWrite(frontRightLed, LOW);
    blinkCntR = 0;
  }
}

void digitalToggle(int pin) {
  if (digitalRead(pin) == HIGH)
    digitalWrite(pin, LOW);
  else
    digitalWrite(pin, HIGH);
}

void brakeLeds(bool on) {
  if(on) {
      digitalWrite(stopLed1, HIGH);
      digitalWrite(stopLed2, HIGH);
      digitalWrite(stopLed3, HIGH);
  } else {
      digitalWrite(stopLed1, LOW);
      digitalWrite(stopLed2, LOW);
      digitalWrite(stopLed3, LOW);
  }
}

void countRotations() {
  unsigned int curr=millis();
  carSpeed = DISTANCE_PER_SEGMENT / (curr - time);
  //cnt++;
  time = curr;
  //Serial.println(carSpeed);
}

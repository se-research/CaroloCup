// Need the Servo library
#include <Servo.h>
#define MAX_STEERING_ANGLE_R 43
#define MAX_STEERING_ANGLE_L -43
#define MIN_MOTOR_SPEED 800
#define MAX_MOTOR_SPEED 1623
#define BRAKE_SPEED_MAX 1530
#define BRAKE_SPEED_MIN 900
#define INIT_MOTOR_SPEED 1520 

// This is our motor.
Servo myMotor;
Servo mySteering;

int motorPin = 6;
int steeringPin = 9;
int stopLed1 = 22;
int stopLed2 = 29;
int stopLed3 = 23;
int blueLed = 40;

int rearLeftLed = 24;
int frontLeftLed = 43;
int rearRightLed = 25;
int frontRightLed = 42;

long int lastTimeStamp;

boolean run = true;
int speed = INIT_MOTOR_SPEED;
int angle = 0;
int freq = 0;
int setFreq = 0;
int propGain = 1;
int intGain = 5;
int read;
int readbyte;
boolean first = true;
boolean motor = false;
boolean steering = false;
boolean brake = false;
boolean applybrake = false;
boolean cruiseCtrl = false;
boolean applyCruiseCtrl = false;
int multiplier = 1;
boolean takeOver = false;
boolean indicators = false;

int ms;
unsigned long time;
unsigned long int cnt=0, cntOld = 0;
float carSpeed = 0;
int takeOverTokens = 10;

int reading = 0;
boolean blinkingLeft = false;
boolean blinkingRight = false;
boolean stopBlinking = true;

void setup()
{
  // Setup the servos
  myMotor.attach(motorPin);
  mySteering.attach(steeringPin);

  // Set a startup speed
  myMotor.writeMicroseconds(975);
  mySteering.write(90);
  
  time = 0;

  attachInterrupt(3, countRotations, FALLING);
  pinMode(2, INPUT);
  pinMode(3, INPUT);
  // initialize serial communication
  Serial.begin(115200);
  Serial.println("initialized");

  pinMode(blueLed, OUTPUT);
  pinMode(stopLed1, OUTPUT);
  pinMode(stopLed2, OUTPUT);
  pinMode(stopLed3, OUTPUT);
  
  pinMode(rearLeftLed, OUTPUT);
  pinMode(frontLeftLed, OUTPUT);
  pinMode(rearRightLed, OUTPUT);
  pinMode(frontRightLed, OUTPUT);
}

void loop()
{
  //time = millis();
  if(run) {
    myMotor.writeMicroseconds(speed);
    Serial.print("speed: ");
    Serial.println(speed);
    Serial.print("angle: ");
    Serial.println(90);
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
        if(readbyte == 47) {
          break;
        }

        if (!first && motor) {
          speed = speed * 10 + (readbyte - 48);
        }
        if (!first && steering){
          if(readbyte < 48) {
            multiplier = -1;
          } 
          else {
            angle = angle * 10 + (readbyte - 48);
          }
        }
        if(!first && brake) {
          if(readbyte == 43) {
            applybrake = true;
          } 
          else {
            applybrake = false;
          }
        }
        if (!first && cruiseCtrl) {
          if (readbyte == 43) {
            applyCruiseCtrl = true;
          } else if (readbyte == 45) {
            applyCruiseCtrl = false;
            speed = 950;
            controlMotor();
          } else {
            applyCruiseCtrl = true;
            setFreq = setFreq * 10 + (readbyte - 48);                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
          }
        }
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
        if (first && readbyte == 109) {
          motor = true;
          first = false;
          speed = 0;
        }
        if(first && readbyte == 115) {
          steering = true;
          first = false;
          angle = 0;
          multiplier = 1;
        }
        if(first && readbyte == 98) {
          brake = true;
          first = false;
        }
        if (first && readbyte == 102) {
          cruiseCtrl = true;
          first = false;
          setFreq = 0;
        }
        if (first && readbyte == 105) {
          indicators = true;
          first = false;
        }
        read = read - 1;
      }
      //Process data
      if(motor) {
        //Serial.println(speed);
        if(speed > MIN_MOTOR_SPEED && speed < MAX_MOTOR_SPEED) {
          controlMotor();
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

      first = true;
      motor = false;
      steering = false;
      brake = false;
      cruiseCtrl = false;
      multiplier = 1;
    }
  } 
  else {
    multiplier = 1;
    Serial.println(angle);
    Serial.println(speed);
    controlMotor();
    controlSteering();
  }
  ms = ms + 1;
  //Calculate car speed
  if(ms > 3) {
    unsigned long curr = millis();
    //Serial.println((curr - time));
    int diff = cnt * 1000 / (curr - time);
    float newSpeed = diff * 0.03;
    if(newSpeed != carSpeed) {
      Serial.print("Car speed:");
      Serial.println(newSpeed);
      carSpeed = newSpeed;
    }
    freq = int(diff*1.2);
    if (applyCruiseCtrl) {
      //Serial.println("Enter cruise control");
      //Serial.print("Frequency: ");
      //Serial.println(freq);
      int error = setFreq - freq;
      Serial.print("Error: ");
      Serial.println(error);
      int errorSign = error < 0 ? -1 : +1;
      if (abs(error) > 12) {
        speed += 5*errorSign;
      }
      else if (abs(error) > 6) {
        speed += 3*errorSign;
      } else if (abs(error) > 3) {
        speed += errorSign;
      }
      speed = constrain(speed, 1547, 1623);
      //Serial.print("Speed :");
      //Serial.println(speed);
      controlMotor();
    }
    //cntOld = cnt;
    cnt = 0;
    ms = 0;
    time = curr;
  }
  delay(10);
  //Serial.println((millis()-time));
}

void controlMotor() {
  Serial.print("speed: ");
  Serial.println(speed);
  myMotor.writeMicroseconds(speed);
  if(speed < BRAKE_SPEED_MAX && speed > BRAKE_SPEED_MIN) {
    brakeLeds(true);
  } 
  else {
    brakeLeds(false);
  }
}

void controlSteering() {
  Serial.print("angle: ");
  int inpAngle = angle;
  angle = 90 + angle * multiplier;
  Serial.println(angle);
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
  if(receiverSpeed > 1572) {
    receiverSpeed = 1572;
  }
  //receiverSteer = map(receiverSteer, 1600,2300,819,2219);
  //Serial.println(receiverSpeed);
  //Serial.println(receiverSteer);
  //if(receiverSpeed > 1170 && receiverSpeed < 1370 && !takeOver) {
  if(receiverSteer > 2100) {
    if((takeOverTokens--) == 0) {
      takeOver = true;
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
  cnt++;
}

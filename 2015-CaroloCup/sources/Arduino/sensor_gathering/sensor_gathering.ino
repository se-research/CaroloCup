#include <Wire.h>
#include <stdio.h>
#define ADDRESSBACK 115
#define ADDRESSFRONT 112

// WHEEL ENCODER
#define WHEEL_DIAMETER            65.4
#define PI                        3.141592
#define WHEEL_CIRCUMFERENCE       WHEEL_DIAMETER * PI
#define WHEEL_ENCODER_SEGMENTS    20
#define DISTANCE_PER_SEGMENT      (float)WHEEL_CIRCUMFERENCE / WHEEL_ENCODER_SEGMENTS
#define WHEEL_ENCODER_PIN         0

// IR
#define IR_MAP_SIZE               23
#define IR_MAP_START_VALUE        3

unsigned long distanceTravelledMilli = 0;

int irLeftSide = 0;                  // analog pin used to connect the sharp sensor
int irLeftBack = 1;                  // analog pin used to connect the sharp sensor
int irRightBack = 2;                 // analog pin used to connect the sharp sensor
int irRightSide = 3;                 // analog pin used to connect the sharp sensor
int infraCount = 0;
int uf = -1;
int ub = -1;

int val = 0,cm = 0;                  // variable to store the values from sensor(initially zero)

// All the input values for each of the sensors
int irMapValues[4][23] = {{620, 489, 440, 380, 327, 308, 265, 259, 240, 225, 213, 203, 188, 180, 175, 168, 160, 159, 155, 152, 140, 127, 117}, 
                          {600, 580, 474, 414, 356, 326, 289, 260, 246, 219, 203, 190, 185, 177, 170, 161, 156, 150, 144, 140, 134, 129, 125},
                          {630, 543, 461, 400, 340, 300, 270, 243, 220, 200, 185, 175, 167, 150, 138, 128, 113, 108, 100, 95, 80, 76},
                          {630, 620, 520, 430, 376, 335, 297, 268, 240, 224, 208, 195, 180, 172, 165, 160, 146, 134, 126, 120, 116, 107, 100}};
int reading = 0;

unsigned long time;

void setup()
{
  Serial.begin(115200);
  Wire.begin();
  
  
  attachInterrupt(WHEEL_ENCODER_PIN, encoderISR, CHANGE);
}

int getDistanceIR(int val, int sensorPin)
{
  for (int i = 0; i < IR_MAP_SIZE; i++) {
    if (irMapValues[sensorPin][i] >= val && irMapValues[sensorPin][i + 1] < val) {
      return IR_MAP_START_VALUE + i;
    }
  }
  return -1;
}

int getDistanceUS(){
  
}

void loop()
{
  char irStr[13];
  if(infraCount >= 2){
    int ir1 = getDistanceIR(analogRead(irLeftSide), 0);
    int ir2 = getDistanceIR(analogRead(irLeftBack), 1);
    int ir3 = getDistanceIR(analogRead(irRightBack), 2);
    int ir4 = getDistanceIR(analogRead(irRightSide), 3);
    
    // Serial.println(getDistance(analogRead(irRightSide), 3));
    sprintf(irStr, "i%2d,%2d,%2d,%2d", ir1, ir2, ir3, ir4);
    
    infraCount = 0;
  }
  infraCount++;
  //instrumt sensor to read echoes
  Wire.beginTransmission(ADDRESSFRONT); // transmit to front sensor #112 (0x70)(It is actually 0xE0)
                               // the address specified in the datasheet is 224 (0xE0)
                               // but i2c adressing uses the high 7 bits so it's 112
  Wire.write(byte(0x00));      // sets register pointer to the command register (0x00)  
  Wire.write(byte(0x51));      // command sensor to measure in "inches" (0x50) // use 0x51 for centimeters // use 0x52 for ping microseconds
  Wire.write(byte(0x02));      // Write in location 2(Change Range)
  Wire.write(byte(0x18));      // Write the minimum value
  Wire.write(byte(0x01));      // write in location one (change gain)
  Wire.write(byte(0x05));      // Write the minimum value 
  Wire.endTransmission();      // stop transmitting
  
  Wire.beginTransmission(ADDRESSBACK); // transmit to Back sensor #115 (0x73) (It is actually 0xE6)
  Wire.write(byte(0x00));      // sets register pointer to the command register (0x00)  
  Wire.write(byte(0x51));      // command sensor to measure in "inches" (0x50)   // use 0x51 for centimeters  // use 0x52 for ping microseconds
  Wire.write(byte(0x02));      // Write in location 2(Change Range)
  Wire.write(byte(0x18));      // Write the minimum value
  Wire.write(byte(0x01));      // write in location one (change gain)
  Wire.write(byte(0x05));      // Write the minimum value 
  Wire.endTransmission();      // stop transmitting
  // wait for readings to happen
  delay(8);   // datasheet suggests at least 65 milliseconds
  

  ////////////////////////////////
  // instrumt sensor to return a particular echo reading
  Wire.beginTransmission(ADDRESSFRONT); // transmit to device #112
  Wire.write(byte(0x02));      // sets register pointer to echo #1 register (0x02)
  Wire.endTransmission(); // stop transmitting
  
  Wire.beginTransmission(ADDRESSBACK); // transmit to device #115
  Wire.write(byte(0x02));      // sets register pointer to echo #1 register (0x02)
  Wire.endTransmission();      // stop transmitting

  // request reading from sensor
  Wire.requestFrom(ADDRESSFRONT, 2);    // request 2 bytes from slave device #112
  // step 5: receive reading from sensor

  if(2<= Wire.available())      // if two bytes were received
  {
    reading = Wire.read();      // receive high byte (overwrites previous reading)
    reading = reading << 8;     // shift high byte to be high 8 bits
    reading |= Wire.read();     // receive low byte as lower 8 bits
    if(reading < 1000){
    uf = reading;
    }
  }
  
  Wire.requestFrom(ADDRESSBACK, 2);    // request 2 bytes from slave device #112

  if(2<= Wire.available())      // if two bytes were received
  {
    reading = Wire.read();      // receive high byte (overwrites previous reading)
    reading = reading << 8;     // shift high byte to be high 8 bits
    reading |= Wire.read();     // receive low byte as lower 8 bits
    if(reading < 1000){

    ub = reading;
    }
  }

  char uStr[13];
  
  sprintf(uStr, "u%3d,%3d", uf,ub);
  char wStr[5];
  sprintf(wStr, "w%4d.",distanceTravelledMilli);
  

  Serial.print(irStr);
  Serial.print(":");
  Serial.print(uStr);
  Serial.print(":");
  Serial.println(wStr);
}

void encoderISR(){
  distanceTravelledMilli += DISTANCE_PER_SEGMENT;
  if(distanceTravelledMilli > 9999){
    distanceTravelledMilli = 0;
  }
}

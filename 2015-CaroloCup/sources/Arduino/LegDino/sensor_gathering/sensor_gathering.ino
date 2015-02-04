#include <Wire.h>
#include <stdio.h>
#include <EEPROM.h>

#define ADDRESSBACK 115
#define ADDRESSFRONT 112

// WHEEL ENCODER
#define WHEEL_DIAMETER            80
#define PI                        3.141592
#define WHEEL_CIRCUMFERENCE       WHEEL_DIAMETER * PI
#define WHEEL_ENCODER_SEGMENTS    10
#define DISTANCE_PER_SEGMENT      (float)WHEEL_CIRCUMFERENCE / WHEEL_ENCODER_SEGMENTS
#define WHEEL_ENCODER_PIN         0

// IR
#define IR_MAP_SIZE               23
#define IR_MAP_START_VALUE        3

unsigned long distanceTravelledMilli = 0;



//Identification
char sID[7];


int irLeftSide = 0;                  // analog pin used to connect the sharp sensor
int irLeftBack = 1;                  // analog pin used to connect the sharp sensor
int irRightBack = 2;                 // analog pin used to connect the sharp sensor
int irRightSide = 3;                 // analog pin used to connect the sharp sensor
int infraCount = 0;
int lightSensor = 7;
int uf = -1;
int ub = -1;

int val = 0,cm = 0;                  // variable to store the values from sensor(initially zero)

// All the input values for each of the sensors

 // A normal distribution
 // out[] holds the values wanted in cm
int out[] = { 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3  };  // 28
  // note: the in array should have increasing values
// in[] holds the measured analogRead() values for defined distances
int in[]  = {27,31,35,39,43,48,56,65,77,85,93,101,113,121,135,150,165,181,203,220,245,275,310,350,400,460,560,630}; 
int size = 28;
int reading = 0;

unsigned long time;
int getDistanceIR(int val)
{
  // take care the value is within range
  // val = constrain(val, _in[0], _in[size-1]);
  if (val <= in[0]) return -1;
  if (val >= in[size-1]) return -1;
  
  // search right interval
  uint8_t pos = 1;  // _in[0] allready tested
  while(val > in[pos]) pos++;

  // this will handle all exact "points" in the _in array
  if (val == in[pos]) return out[pos];

  // interpolate in the right segment for the rest
  return map(val, in[pos-1], in[pos], out[pos-1], out[pos]);
}
void setup()
{
  Serial.begin(115200);
  Wire.begin();
  
  //Identification
  for (int i=0; i<6; i++) {
    sID[i] = EEPROM.read(i);
  }
  //Serial.println(sID); 
    
  attachInterrupt(WHEEL_ENCODER_PIN, encoderISR, FALLING);
}



int getDistanceUS(){
  
}

void loop()
{
  char irStr[13];
  if(infraCount >= 2){
    int ir1 = getDistanceIR(analogRead(irLeftSide));
    int ir2 = getDistanceIR(analogRead(irLeftBack));
    int ir3 = getDistanceIR(analogRead(irRightBack));
    int ir4 = getDistanceIR(analogRead(irRightSide));
    
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
  int lightVal= getAmbientLight();
  char lStr[4];
  sprintf(lStr, "l%3d",lightVal);

  char uStr[13];
  
  sprintf(uStr, "u%3d,%3d", uf,ub);
  char wStr[6];
  sprintf(wStr, "w%5d.",distanceTravelledMilli);
  

  Serial.print(irStr);
  Serial.print(":");
  Serial.print(uStr);
  Serial.print(":");
  Serial.print(lStr);
  Serial.print(":");
  Serial.println(wStr);
  
}

void encoderISR(){
  distanceTravelledMilli += DISTANCE_PER_SEGMENT;
  if(distanceTravelledMilli > 99999){
    distanceTravelledMilli = 0;
  }
}
int getAmbientLight()
{
    return analogRead(lightSensor) * 0.9765625;
}

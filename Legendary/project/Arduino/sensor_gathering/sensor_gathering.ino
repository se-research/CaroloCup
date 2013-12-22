#include <Wire.h>
#define ADDRESS 112

int irpin1 = 0;                 // analog pin used to connect the sharp sensor
int irpin2 = 1;                 // analog pin used to connect the sharp sensor
int irpin3 = 2;                 // analog pin used to connect the sharp sensor
int irpin4 = 3;                 // analog pin used to connect the sharp sensor

int val = 0,cm = 0;                 // variable to store the values from sensor(initially zero)
 // A normal distribution
 // out[] holds the values wanted in cm
int out[] = { 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3  };  // 28
  // note: the in array should have increasing values
// in[] holds the measured analogRead() values for defined distances
int in[]  = {27,31,35,39,43,48,56,65,77,85,93,101,113,121,135,150,165,181,203,220,245,275,310,350,400,460,560,630}; 
int size = 28;

int reading = 0;

unsigned long time;
unsigned long int cntF=0, cntR=0, cntOldF = 0, cntOldR = 0;
int ms = 0;

void setup()
{
  Serial.begin(115200);               // starts the serial monitor
  Wire.begin();
  
  attachInterrupt(0, countRotationsF, FALLING);
  attachInterrupt(1, countRotationsR, FALLING);
}

int getDistance(int val)
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

void loop()
{
  int ir1, ir2, ir3, ir4;
  ir1 = getDistance(analogRead(irpin1));
  ir2 = getDistance(analogRead(irpin2));
  ir3 = getDistance(analogRead(irpin3));
  ir4 = getDistance(analogRead(irpin4)); 
  Serial.print("i");
  Serial.print(ir1);
  Serial.print(",");
  Serial.print(ir2);
  Serial.print(",");
  Serial.print(ir3);
  Serial.print(",");
  Serial.print(ir4);
  Serial.println(".");

  // step 1: instruct sensor to read echoes
  Wire.beginTransmission(ADDRESS); // transmit to device #112 (0x70)
                               // the address specified in the datasheet is 224 (0xE0)
                               // but i2c adressing uses the high 7 bits so it's 112
  Wire.write(byte(0x00));      // sets register pointer to the command register (0x00)  
  Wire.write(byte(0x51));      // command sensor to measure in "inches" (0x50) 
                               // use 0x51 for centimeters
                               // use 0x52 for ping microseconds
  Wire.endTransmission();      // stop transmitting

  // step 2: wait for readings to happen
  delay(70);                   // datasheet suggests at least 65 milliseconds

  // step 3: instruct sensor to return a particular echo reading
  Wire.beginTransmission(ADDRESS); // transmit to device #112
  Wire.write(byte(0x02));      // sets register pointer to echo #1 register (0x02)
  Wire.endTransmission();      // stop transmitting

  // step 4: request reading from sensor
  Wire.requestFrom(ADDRESS, 2);    // request 2 bytes from slave device #112
  // step 5: receive reading from sensor
  if(2<= Wire.available())    // if two bytes were received
  {
    reading = Wire.read();  // receive high byte (overwrites previous reading)
    reading = reading << 8;    // shift high byte to be high 8 bits
    reading |= Wire.read(); // receive low byte as lower 8 bits
    Serial.print("u");
    Serial.print(reading);   // print the reading
    Serial.println(".");
  }
  ms++;
  if(ms > 2) {
    //Calculate car speed
    unsigned long curr = millis();
    /*Serial.print(cntF);
    Serial.print(",");
    Serial.println(cntR);
    Serial.println((curr - time));*/
    int diffF = cntF * 1000 / (curr - time);
    int diffR = cntR * 1000 / (curr - time);
    float carSpeedF = diffF * 0.03;
    float carSpeedR = diffR * 0.03;
    Serial.print("h");
    Serial.print((int)(carSpeedF*100));
    Serial.print(",");
    Serial.print((int)(carSpeedR*100));
    Serial.println(".");
    ms=0;
    cntF = 0;
    cntR = 0;
    time = curr;
  }
}

void countRotationsF() {
  cntF++;
}

void countRotationsR() {
  cntR++;
}


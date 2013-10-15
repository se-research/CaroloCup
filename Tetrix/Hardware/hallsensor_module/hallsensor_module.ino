/********************************************************
 * Author:    Zlatan Habul
 * Created:   12.10.2013
 * This arduino MPU is
 * halsensor speed counter
 ********************************************************/


char inData[20];
int sensorReading = 0;



#define hallInputPin 12

double rotationSpeed=0;
unsigned long hallSensor=0;
unsigned long oldTime =0;
unsigned long time =0;
char buf[20];

void setup() {
  pinMode(hallInputPin, INPUT);
  digitalWrite(hallInputPin,HIGH); //pulup on haqll sensor
  Serial.begin(9600);
}

void loop()
{
  hallSensor = pulseIn(hallInputPin, LOW,200000);
  if (hallSensor>0) {
    rotationSpeed = double((1000.00/((hallSensor/1000.00)*14.00))*100.00);
  }
  else
  {
     rotationSpeed=0;
  }
   if (rotationSpeed<0 || rotationSpeed >2000) rotationSpeed=0;
  Serial.print("#");
  Serial.print((int)rotationSpeed);
  Serial.print(",");
  Serial.print(sensorReading);
  Serial.print("|");
  
}

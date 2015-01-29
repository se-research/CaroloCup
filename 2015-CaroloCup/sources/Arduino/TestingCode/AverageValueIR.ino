const int numberOfReadings = 100;
int pin = 0;

unsigned int readings[numberOfReadings];
int index = 0;
unsigned int total = 0;
int average = 0;

void setup(){
  Serial.begin(9600);
  
  for(int i = 0; i < numberOfReadings; i++){
    //Initialize the array with 0
    readings[i] = 0;
  }
}

  void loop(){
    total -= readings[index];
    
    readings[index] = analogRead(pin);
    total += readings[index];
    
    index++;

    
/* Serial.print("Raw value: ");
    Serial.println(analogRead(pin)); */  
    average = total / numberOfReadings;
    Serial.print("Average: ");
    Serial.println(average);
    
    if(index == numberOfReadings){
      index = 0;
    /*average = total / numberOfReadings;
    
    Serial.print("Average: ");
    Serial.println(average);        */
    }
    delay(50);
 }

#include <LiquidCrystal.h>


#define ROOT 0
#define DATA 1


LiquidCrystal lcd(12, 11, 10, 5, 4, 3, 2);

int contrast = 6;
int menuLevel = 0;

int upLeft = 9;
int downLeft = 13;
int upRight = 8;
int downRight = 7;

void setup(){
  pinMode(upLeft, INPUT);
  pinMode(downLeft, INPUT);
  pinMode(upRight, INPUT);
  pinMode(downRight, INPUT);

  pinMode(contrast, OUTPUT);
  analogWrite(contrast, 140);
  lcd.begin(20,4);

  Serial.begin(9600);
}

void loop(){
  lcd.clear(); 
  switch (menuLevel) {
      case ROOT:
          lcd.setCursor(0,0);          
		  lcd.print("SCENARIOS:");
		  lcd.setCursor(0,1);
		  lcd.print("< STOP");   
		  lcd.setCursor(7,1);     
		  lcd.print("LANEFOLLOW >");
		  lcd.setCursor(10,3);           
		  lcd.print("PARKING >");
        break;
      case DATA:
          lcd.setCursor(0,0);          
		  lcd.print("TESTDATA:");   
		  lcd.setCursor(4,1);        
		  lcd.print("LANEFOLLOWING >");
		  lcd.setCursor(10,3);           
		  lcd.print("PARKING >");
        break;

      default:
        ;
  }
  if(digitalRead(upLeft)){
	Serial.print("1");  	//Sends STOP signal
  }
  if(digitalRead(downLeft)){
  	// menuLevel++;
  }
  if(menuLevel == 0){
  	if(digitalRead(upRight)){
  		Serial.print("2"); //LaneFollowing
  }
  	if(digitalRead(downRight)){
		Serial.print("3"); //Parking
  }
}
  if(menuLevel > 1 || menuLevel < 0){
  	menuLevel = 0;
  } 
  delay(100);

 /* Serial.print("upLeft: ");  
  Serial.println(digitalRead(upLeft));  
  Serial.print("upRight: ");  
  Serial.println(digitalRead(upRight)); 
    Serial.print("downRight: ");  
  Serial.println(digitalRead(downRight)); 
  */
}

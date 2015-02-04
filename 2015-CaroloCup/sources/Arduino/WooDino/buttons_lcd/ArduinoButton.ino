#include <LiquidCrystal.h>
#include <EEPROM.h>


//Identification
char sID[7];

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
  
  //Identification
  for (int i=0; i<6; i++) {
    sID[i] = EEPROM.read(i);
  }
  //Serial.println(sID); 
  
}

void loop(){
  lcd.clear(); 
screenPrint();
 

  if(digitalRead(upLeft)){
	Serial.print("0");  	//Sends STOP signal
	lcd.clear();
	lcd.setCursor(1, 1);
	lcd.print("STOPPING PROCESSES");
	delay(2000);
	//lcd.clear();
  }
  if(digitalRead(downLeft)){
  	Serial.print("3");
  }
  if(digitalRead(upRight)){
  	Serial.print("1"); //LaneFollowing
	lcd.clear();
	lcd.setCursor(1, 1);
	lcd.print("STARTING LANEFOLLOW");
	delay(2000);
	//lcd.clear();

  }
  if(digitalRead(downRight)){
		Serial.print("2"); //Parking
	lcd.clear();
	lcd.setCursor(1, 1);
	lcd.print("STARTING PARKING");
	delay(2000);
	//lcd.clear();
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

void screenPrint(){

 lcd.setCursor(0,0);          
  lcd.print("SCENARIOS:");
  lcd.setCursor(0,1);
  lcd.print("< STOP");   
  lcd.setCursor(8,1);     
  lcd.print("LANEFOLLOW >");
  lcd.setCursor(11,3);           
  lcd.print("PARKING >");
}

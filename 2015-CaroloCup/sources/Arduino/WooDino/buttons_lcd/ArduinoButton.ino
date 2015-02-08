#include <LiquidCrystal.h>
#include <EEPROM.h>

#define ROOT 0
#define SCENARIO 1


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

 switch (menuLevel) {
      case ROOT:
          lcd.setCursor(0,0);          
      lcd.print("CAR STATUS:");
      lcd.setCursor(0,1);
      lcd.print("< STOP");   
      lcd.setCursor(12,1);     
      lcd.print("READY! >");
      upLeftButton();
      upRightButton();

        break;
      case SCENARIO:
          lcd.setCursor(0,0);          
      lcd.print("SCENARIOS:");
      lcd.setCursor(0,1);
      lcd.print("< STOP");
        upLeftButton();   
      lcd.setCursor(7,1);        
      lcd.print("LANEFOLLOW >");
      upRightButton();
      lcd.setCursor(12,3);           
      lcd.print("PARKING >");
      downRightButton();
        break;

      default:
        ;
  }

  if(digitalRead(downLeft)){
    //Serial.print("3");
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

} //End of loop()


void upLeftButton(){

if(menuLevel == 0){
  if(digitalRead(upLeft)){
  Serial.print("00");   //Sends STOP signal
Serial.flush();
  lcd.clear();
  lcd.setCursor(1, 1);
  lcd.print("STOPPING PROCESSES");
  delay(2000);
  }
}if(menuLevel == 1){
  if(digitalRead(upLeft)){
  Serial.print("00");   //Sends STOP signal
Serial.flush();
  lcd.clear();
  lcd.setCursor(1, 1);
  lcd.print("STOPPING PROCESSES");
  delay(2000);
  menuLevel = 0;
  }
}
}

void upRightButton(){
if(menuLevel == 0){
  if(digitalRead(upRight)){
    Serial.print("44"); //Starting supercomponent and proxy
  Serial.flush();
  lcd.clear();
  lcd.setCursor(1, 1);
  lcd.print("GETTING READY");
  delay(2000);
  menuLevel = 1;

  }
}if(menuLevel == 1){
  if(digitalRead(upRight)){
    Serial.print("11"); //LaneFollowing
  Serial.flush();
  lcd.clear();
  lcd.setCursor(1, 1);
  lcd.print("STARTING LANEFOLLOW");
  delay(2000);
  menuLevel = 0;

  }
}
}

void downRightButton(){
if(menuLevel == 1){
  if(digitalRead(downRight)){
    Serial.print("22"); //Parking
    Serial.flush();
  lcd.clear();
  lcd.setCursor(1, 1);
  lcd.print("STARTING PARKING");
  delay(2000);
  menuLevel = 0;
  }
}
}
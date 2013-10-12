
/********************************************************
 * Author:    Zlatan Habul
 * Created:   12.10.2013
 * for erlang
 * only export function below text "Function for export"
 ********************************************************/

//Global values
int speed = 0;
int angle = 0;
int leftSignalLight = 0;
int rightSignalLight = 0;
int brakeLight=0;
int modeSwitch=0;
int remoteStatus=0;
double axelSpeed=0;
double voltage=0;
double current =0;
int heading = 0;
int irSensor0=0;
int irSensor1=0;
int ultraSonic=0;
char *displayRow1;
char *displayRow2;
char output[64];
long mTime;
long oldTime;

long current_timestamp();
char signalOnOff();

//Function for export
void setLeftSignalLight(int val );
void setRightSignalLight(int val);
void setBrakeLight(int val);
void setDisplay(char *value);
void setSpeed(int val);
void setAngle(int val);
double getAxelSpeed();
int getModeSwitch();
int getRemoteStatus();
double getVoltage();
double getCurrent();
int getHeading();
double getIrSensor0();
double getIrSensor1();
int getUltraSonic();

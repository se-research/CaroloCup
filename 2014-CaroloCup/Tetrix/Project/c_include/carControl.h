
/********************************************************
 * Author:    Zlatan Habul
 * Created:   18.10.2013
 * for erlang
 * only export function below text "Function for export"
 ********************************************************/



long current_timestamp();
char signalOnOff(long mem);

//Function for export
long startUsb();
void stopUsb(long mem);
void setLeftSignalLight(int val,long mem);
void setRightSignalLight(int val,long mem);
void setBrakeLight(int val,long mem);
void setDisplay(char *value,long mem);
void setSpeed(int val,long mem);
void setAngle(int val,long mem);
double getAxelSpeed(long mem);
int getModeSwitch(long mem);
int getRemoteStatus(long mem);
double getVoltage(long mem);
double getCurrent(long mem);
int getHeading(long mem);
double getIrSensor0(long mem);
double getIrSensor1(long mem);
int getUltraSonic(long mem);

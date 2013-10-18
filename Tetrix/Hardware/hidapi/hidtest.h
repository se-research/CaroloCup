
/********************************************************
 * Author:    Zlatan Habul
 * Created:   18.10.2013
 * for erlang
 * only export function below text "Function for export"
 ********************************************************/



long current_timestamp();
char signalOnOff(void *mem);

//Function for export
void *startUsb();
void stopUsb(void *mem);
void setLeftSignalLight(int val,void *mem);
void setRightSignalLight(int val,void *mem);
void setBrakeLight(int val,void *mem);
void setDisplay(char *value,void *mem);
void setSpeed(int val,void *mem);
void setAngle(int val,void *mem);
double getAxelSpeed(void *mem);
int getModeSwitch(void *mem);
int getRemoteStatus(void *mem);
double getVoltage(void *mem);
double getCurrent(void *mem);
int getHeading(void *mem);
double getIrSensor0(void *mem);
double getIrSensor1(void *mem);
int getUltraSonic(void *mem);

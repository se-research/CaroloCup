
/********************************************************
 * Author:    Zlatan Habul
 * Created:   27.01.2014
 * for erlang
 * only export function below text "Function for export"
 ********************************************************/


//Function for export
long startSerial();
void stopSerial(long mem);
void setLeftSignalLight(int val,long mem);
void setRightSignalLight(int val,long mem);
void setBrakeLight(int val,long mem);
void setRemoteLight(int val,long mem);
void setBeep(int val,long mem);
void setDisplay(char *value,long mem);
void setSpeed(int val,long mem);
void setAngle(int val,long mem);
void setDirection(int val,long mem);
double getVoltage(long mem);
int getSwitch1(long mem);
int getSwitch2(long mem);
int getRemoteY(long mem);
int getRemoteX(long mem);
int getRemoteStatus(long mem);

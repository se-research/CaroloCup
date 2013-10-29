#include "erl_nif.h"
#include <sys/time.h>
#include "iostream"
#include <stdio.h>
#include <wchar.h>
#include <string.h>
#include "carControl.h"



using namespace std;

extern void setAngle(int val,long mem);
extern void setSpeed(int val,long mem);



static ERL_NIF_TERM start_usb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  address = startUsb();
  return enif_make_long(env, address);
}
static ERL_NIF_TERM stop_usb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  stopUsb(address);
  return enif_make_atom(env,"ok");
}

static ERL_NIF_TERM set_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int angle;
  long address;
  enif_get_int(env, argv[0], &angle);
  enif_get_long(env, argv[1], &address);
  setAngle(angle, address);
  return enif_make_long(env,address);
}

static ERL_NIF_TERM set_speed(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int speed;
  long address;
  enif_get_int(env, argv[0], &speed);
  enif_get_long(env, argv[1], &address);
  setSpeed(speed, address);
  return enif_make_long(env,address);
}



static ERL_NIF_TERM set_display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char display[32];
  long address;
  enif_get_string(env, argv[0], display, 32, ERL_NIF_LATIN1);
  enif_get_long(env, argv[1], &address);
  setDisplay(display, address);
  return enif_make_long(env,address);
}

static ERL_NIF_TERM set_BrakeLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int brakeL;
  long address;
  enif_get_int(env, argv[0], &brakeL);
  enif_get_long(env, argv[1], &address);
  setBrakeLight(brakeL, address);
  return enif_make_long(env,address);
}

static ERL_NIF_TERM set_RightSignalLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int rightL;
  long address;
  enif_get_int(env, argv[0], &rightL);
  enif_get_long(env, argv[1], &address);
  setRightSignalLight(rightL, address);
  return enif_make_long(env,address);
}

static ERL_NIF_TERM set_LeftSignalLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int leftL;
  long address;
  enif_get_int(env, argv[0], &leftL);
  enif_get_long(env, argv[1], &address);
  setLeftSignalLight(leftL, address);
  return enif_make_long(env,address);
}
static ErlNifFunc nif_funcs[] = 
  {
    
    {"start_usb", 0, start_usb},
    {"stop_usb", 1, stop_usb},
    {"set_speed", 2, set_speed},
    {"set_angle", 2, set_angle}, 
    {"set_display", 2, set_display},
    {"set_brakeLight", 2, set_BrakeLight},
    {"set_rightLight", 2, set_RightSignalLight},
    {"set_leftLight", 2, set_LeftSignalLight}

  };

/*double getAxelSpeed(long mem);
int getModeSwitch(long mem);
int getRemoteStatus(long mem);
double getVoltage(long mem);
double getCurrent(long mem);
int getHeading(long mem);
double getIrSensor0(long mem);
double getIrSensor1(long mem);
int getUltraSonic(long mem);*/



ERL_NIF_INIT(hidnif,nif_funcs,NULL,NULL,NULL,NULL)

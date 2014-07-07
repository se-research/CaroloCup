#include "erl_nif.h"
#include <sys/time.h>
#include "iostream"
#include <stdio.h>
#include <wchar.h>
#include <string.h>
#include "../c_include/carControl.h"



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

static ERL_NIF_TERM get_accelSpeed(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_double(env,getAxelSpeed(address));
}

static ERL_NIF_TERM get_modeSwitch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_int(env,getModeSwitch(address));
}

static ERL_NIF_TERM get_remoteStatus(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_int(env,getRemoteStatus(address));
}

static ERL_NIF_TERM get_Voltage(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_double(env,getVoltage(address));
}

static ERL_NIF_TERM get_Current(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_double(env,getCurrent(address));
}

static ERL_NIF_TERM get_Heading(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_int(env,getHeading(address));
}

static ERL_NIF_TERM get_IR0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_double(env,getIrSensor0(address));
}

static ERL_NIF_TERM get_IR1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_double(env,getIrSensor1(address));
}

static ERL_NIF_TERM get_ultraSonic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_int(env,getUltraSonic(address));
}

/*
For testing purposes, crash the system with a segmentation fault
*/
static ERL_NIF_TERM cause_segmentation_fault(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char *s = "hello world";
  *s = 'H';  
  return enif_make_double(env,5);
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
    {"set_leftLight", 2, set_LeftSignalLight},
    {"get_accelSpeed", 1, get_accelSpeed},
    {"get_modeSwitch", 1, get_modeSwitch},
    {"get_remoteStatus", 1, get_remoteStatus},
    {"get_Voltage", 1, get_Voltage},
    {"get_Current", 1, get_Current},
    {"get_Heading", 1, get_Heading},
    {"get_IR0", 1, get_IR0},
    {"get_IR1", 1, get_IR1},
    {"get_ultraSonic", 1, get_ultraSonic},
    {"cause_segmentation_fault",0, cause_segmentation_fault}
  };

ERL_NIF_INIT(hidnif,nif_funcs,NULL,NULL,NULL,NULL)

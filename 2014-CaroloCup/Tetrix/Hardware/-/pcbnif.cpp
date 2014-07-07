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

static ERL_NIF_TERM start_Pcb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  address = startSerial();
  return enif_make_long(env, address);
}
static ERL_NIF_TERM stop_Pcb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  stopSerial(address);
  return enif_make_atom(env,"ok");
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
static ERL_NIF_TERM set_RightSignalLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int rightL;
  long address;
  enif_get_int(env, argv[0], &rightL);
  enif_get_long(env, argv[1], &address);
  setRightSignalLight(rightL, address);
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
static ERL_NIF_TERM set_RemoteLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int rlight;
  long address;
  enif_get_int(env, argv[0], &rlight);
  enif_get_long(env, argv[1], &address);
  setRemoteLight(rlight, address);
  return enif_make_long(env,address);
}
static ERL_NIF_TERM set_Beep(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int beep;
  long address;
  enif_get_int(env, argv[0], &beep);
  enif_get_long(env, argv[1], &address);
  setBeep(beep, address);
  return enif_make_long(env,address);
}
static ERL_NIF_TERM set_Display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char display[32];
  long address;
  enif_get_string(env, argv[0], display, 32, ERL_NIF_LATIN1);
  enif_get_long(env, argv[1], &address);
  setDisplay(display, address);
  return enif_make_long(env,address);
}

static ERL_NIF_TERM set_Speed(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int speed;
  long address;
  enif_get_int(env, argv[0], &speed);
  enif_get_long(env, argv[1], &address);
  setSpeed(speed, address);
  return enif_make_long(env,address);
}
static ERL_NIF_TERM set_Angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int angle;
  long address;
  enif_get_int(env, argv[0], &angle);
  enif_get_long(env, argv[1], &address);
  setAngle(angle, address);
  return enif_make_long(env,address);
}
static ERL_NIF_TERM set_Direction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int dir;
  long address;
  enif_get_int(env, argv[0], &dir);
  enif_get_long(env, argv[1], &address);
  setDirection(dir, address);
  return enif_make_long(env,address);
}

static ERL_NIF_TERM get_Switch1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_int(env,getSwitch1(address));
}
static ERL_NIF_TERM get_Switch2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_int(env,getSwitch2(address));
}
static ERL_NIF_TERM get_RemoteX(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_int(env,getRemoteX(address));
}
static ERL_NIF_TERM get_RemoteY(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long address;
  enif_get_long(env, argv[0], &address);
  return enif_make_int(env,getRemoteY(address));
}
static ERL_NIF_TERM get_RemoteStatus(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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


static ErlNifFunc nif_funcs[] = 
  {
    
    	{"start_pcb", 0, start_Pcb},
    	{"stop_pcb", 1, stop_Pcb},
   	{"set_leftLight", 2, set_LeftSignalLight},
   	{"set_rightLight", 2, set_RightSignalLight},
	{"set_brakeLight", 2, set_BrakeLight},
	{"set_remoteLight", 2, set_RemoteLight},
	{"set_beep", 2, set_Beep},
	{"set_display", 2, set_Display},
    	{"set_speed", 2, set_Speed},
    	{"set_angle", 2, set_Angle},
	{"set_direction", 2, set_Direction}, 
    	{"get_Switch1", 1, get_Switch1},
	{"get_Switch2", 1, get_Switch2},
    	{"get_remoteX", 1, get_RemoteX},
	{"get_remoteY", 1, get_RemoteY},
	{"get_remoteStatus", 1, get_RemoteStatus},
    	{"get_Voltage", 1, get_Voltage}
  };

ERL_NIF_INIT(pcbnif,nif_funcs,NULL,NULL,NULL,NULL)

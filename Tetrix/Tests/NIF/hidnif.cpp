#include "erl_nif.h"
#include "iostream"
#include <sys/time.h>

#include <stdio.h>
#include <wchar.h>
#include <string.h>
#include <stdlib.h>
#include "hidapi.h"


using namespace std;


ErlNifResourceType* handle_res = NULL;


typedef struct _handle_t {
  hid_device* _handle;
} handle_t;

//------------------------------------------------------------------------------
// NIF callbacks
//------------------------------------------------------------------------------

static void handle_cleanup(ErlNifEnv* env, void* arg) {
  enif_free(arg);
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{

  ErlNifResourceFlags flags = (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  handle_res = enif_open_resource_type(env, "hidnif", "handler",
				      &handle_cleanup,
				      flags, 0);
  return 0;
}


static ERL_NIF_TERM open_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{



  handle_t* handle = (handle_t*)enif_alloc_resource(handle_res, sizeof(handle_t));


  hid_device* handle_temp = hid_open(0x0088, 0x0005, NULL);
  handle->_handle = handle_temp;

  cout << "ADDRESS " <<  handle << endl;
  ERL_NIF_TERM term = enif_make_resource_binary(env, handle, handle ,6);

  //enif_release_resource(frame);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), term); 
  
}




static ERL_NIF_TERM close_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

  
  return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
  {
    {"close_handle", 1, close_handle},
    {"open_handle", 0, open_handle}
  };

ERL_NIF_INIT(hidnif,nif_funcs,load,NULL,NULL,NULL)


/* niftest.c */
#include "erl_nif.h"
#include <math.h>

static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int i =0;
  double d;
  //for(i=0; i< 100000; i++){
    d = atan((1-2)/(3-4)) +  3.14/2 - (3.14/2 * (2-1)/(2-1));
    //}
  enif_make_double(env, d);
  //  return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] =
{
    {"hello", 0, hello}
};

ERL_NIF_INIT(test,nif_funcs,NULL,NULL,NULL,NULL)

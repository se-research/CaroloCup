/* niftest.c */
#include "erl_nif.h"
#include "cv.h"
#include "iostream"
#include "highgui.h"
#include "cxcore.h"
#include <sys/time.h>

using namespace cv;
using namespace std;

long timeInMillis() 
{
  timeval tp;
  gettimeofday(&tp, 0);
  return (tp.tv_usec / 1000);
}

static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
 
  Mat img0 = imread("/home/khashayar/Downloads/rgbb.png");
  //  Mat img1, bl
  for(i=0;i<img0.cols;i++)
    {
      for(j=0;j<img0.rows;j++)
	{
	  ERL_NIF_TERM b = enif_make_int(env, (int) img0.data[i*img0.rows*3 + j*3]);
	  ERL_NIF_TERM g = enif_make_int(env, (int) img0.data[i*img0.rows*3  + j*3 +1]);
	  ERL_NIF_TERM r = enif_make_int(env, (int) img0.data[i*img0.rows*3 + j*3 +2]);
	  res[i*img0.rows + j+2] = enif_make_tuple3(env, b, g,r);
	}
    }
  
  return enif_make_tuple_from_array(env, res, img0.rows*img0.cols+2);
}

static ErlNifFunc nif_funcs[] =
{
  {"hello", 0, hello}
};

ERL_NIF_INIT(niftest,nif_funcs,NULL,NULL,NULL,NULL)

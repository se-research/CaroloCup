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

ERL_NIF_TERM convert_pic(ErlNifEnv* env , Mat img0)
{
  int i,j;
  ERL_NIF_TERM res[img0.cols*img0.rows*3+2];

  res[0] = enif_make_int(env,img0.cols);
  res[1] = enif_make_int(env,img0.rows);

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
  

  ERL_NIF_TERM image_erl = enif_make_tuple_from_array(env, res, img0.rows*img0.cols+2);
  

  return image_erl;
}

Mat convert_to_pic(ErlNifEnv* env, const ERL_NIF_TERM tuple)
{

  int i,j;

  int* arity;
  const ERL_NIF_TERM** array;
  enif_get_tuple(env, tuple , arity, array);
  
  int rows;
  int cols;

  enif_get_int(env, (*array)[0], &cols);  
  enif_get_int(env, (*array)[1], &rows); 
  

  uchar data[cols*rows*3];

  for(i=0;i<cols;i++)
    {
      for(j=0;j<rows;j++)
	{
	  const ERL_NIF_TERM** pixel;
	  enif_get_tuple(env, (*array)[i*rows+j+2] , arity, pixel);
	  int b , g, r;
	  enif_get_int(env, (*pixel)[0], &b); 
	  enif_get_int(env, (*pixel)[1], &g); 	
	  enif_get_int(env, (*pixel)[2], &r); 
	  
	  data[i*rows*3 + j*3] = b;
	  data[i*rows*3 + j*3+1] = g;
	  data[i*rows*3 + j*3+2] = r;

	}
    }

  
  
  Mat image(Size(cols,rows), CV_8UC3, data, Mat::AUTO_STEP);
   
  return image;
}



static ERL_NIF_TERM write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int i,j;

  int* arity;
  const ERL_NIF_TERM** array;
  enif_get_tuple(env, argv[0] , arity, array);
  
  int rows;
  int cols;

  enif_get_int(env, (*array)[0], &cols);  
  enif_get_int(env, (*array)[1], &rows); 
  

  uchar data[cols*rows*3];

  for(i=0;i<cols;i++)
    {
      for(j=0;j<rows;j++)
	{
	  const ERL_NIF_TERM** pixel;
	  enif_get_tuple(env, (*array)[i*rows+j+2] , arity, pixel);
	  int b , g, r;
	  enif_get_int(env, (*pixel)[0], &b); 
	  enif_get_int(env, (*pixel)[1], &g); 	
	  enif_get_int(env, (*pixel)[2], &r); 
	  
	  data[i*rows*3 + j*3] = b;
	  data[i*rows*3 + j*3+1] = g;
	  data[i*rows*3 + j*3+2] = r;

	}
    }

  
  
  Mat image(Size(cols,rows), CV_8UC3, data, Mat::AUTO_STEP);
 
  //  Mat image = convert_to_pic(env, argv[0]);

  

  Mat img1, blurr;

  cvtColor(image, img1, CV_RGB2GRAY);
  GaussianBlur(img1, blurr, Size(11, 11), 11);
  Canny(blurr, blurr, 10, 100, 3);
  
  
  imshow("test",blurr);
  waitKey();
    
  return enif_make_int(env, image.cols);
}


static ERL_NIF_TERM read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

  Mat img0 = imread("/home/khashayar/Downloads/pic.png");
  //  Mat img1, bl
  

  ERL_NIF_TERM image_erl = convert_pic(env, img0);
  
  /*
  int redid = convert_to_pic(env, image_erl);
  */
  //  return enif_make_int(env, 10);
    return image_erl;
}

static ErlNifFunc nif_funcs[] =
{
  {"read", 0, read},
  {"write", 1, write}
};

ERL_NIF_INIT(niftest,nif_funcs,NULL,NULL,NULL,NULL)


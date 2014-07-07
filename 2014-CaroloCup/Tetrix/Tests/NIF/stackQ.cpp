/* niftest.c */

#include "erl_nif.h"
#include <opencv/highgui.h>
#include <opencv/cv.h>
using namespace cv;
using namespace std;


static ErlNifResourceType* frame_res = NULL;


typedef struct _frame_t {
IplImage* _frame;
} frame_t;

//------------------------------------------------------------------------------
// NIF callbacks
//------------------------------------------------------------------------------

static void frame_cleanup(ErlNifEnv* env, void* arg) {
enif_free(arg);
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{

ErlNifResourceFlags flags = (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
frame_res = enif_open_resource_type(env, "niftest", "ocv_frame",
				      &frame_cleanup,
				      flags, 0);
return 0;
}


static ERL_NIF_TERM get_pic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

IplImage* src = cvLoadImage("/home/khashayar/Downloads/pic.png");

cout << src->width << endl;

IplImage* gray = cvCreateImage(cvGetSize(src), IPL_DEPTH_8U, 1);
cvCvtColor(src, gray, CV_RGB2GRAY);

frame_t* frame = (frame_t*)enif_alloc_resource(frame_res, sizeof(frame_t));
frame->_frame = gray ;

ERL_NIF_TERM term = enif_make_resource(env, frame);
enif_release_resource(frame);
return enif_make_tuple2(env, enif_make_atom(env, "ok"), term); 
  
}


static ERL_NIF_TERM show_pic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

frame_t* frame;
 if (!enif_get_resource(env, argv[0], frame_res, (void**) &frame)) {
   return enif_make_badarg(env);
 }

 cvShowImage("YOOHOO", frame->_frame);

 cvWaitKey(30);
  
 return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
  {
    {"show_pic", 1, show_pic},
    {"get_pic", 0, get_pic}
  };

ERL_NIF_INIT(niftest,nif_funcs,load,NULL,NULL,NULL)


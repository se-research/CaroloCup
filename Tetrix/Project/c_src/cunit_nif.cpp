#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/calib3d/calib3d.hpp>
#include <opencv2/highgui/highgui.hpp>



#include "/usr/lib/erlang/usr/include/erl_nif.h"

#ifndef __ERL__NIF_H__
#include "/usr/lib64/erlang/usr/include/erl_nif.h"
#endif
#include "camera_functions.hpp"
#include "iostream"
#include <ostream>
#include <sstream>
#include <string>
#include <sys/time.h>
#include <opencv/highgui.h>
#include <opencv/cv.h>

using namespace cv;
using namespace std;


/*ErlNifResourceType* frame_res = NULL;
int counter = 0; // counter used for naming saved .jpg files

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

	//Initializes the camera
  init_camera();

  return 0;
}
*/
/* Retrieves an image form the uEye camera, and assigns the data to the frame
 * struct
*/
static ERL_NIF_TERM open_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{


  return enif_make_atom(env, "ok"); 
  
}

/* Retrieves image data from the frame struct, and saves the image as jpg file
 * in images folder
*/
static ERL_NIF_TERM close_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  
  char c[2];
  if (!enif_get_atom(env, argv[0], c, sizeof(c) * 2, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }
  cout << "handled " << c << endl;
  return enif_make_atom(env, "ok");
}


/* Nif function definitions */
static ErlNifFunc nif_funcs[] =
  {
    {"close_handle", 1, close_handle},
    {"open_handle", 0, open_handle}
  };

ERL_NIF_INIT(cunit_nif,nif_funcs,NULL,NULL,NULL,NULL)


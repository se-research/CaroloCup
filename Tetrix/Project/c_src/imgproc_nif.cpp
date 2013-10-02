#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/calib3d/calib3d.hpp>
#include <opencv2/highgui/highgui.hpp>

#include "/usr/lib/erlang/usr/include/erl_nif.h"
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


ErlNifResourceType* frame_res = NULL;
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
  frame_res = enif_open_resource_type(env, "imgproc_nif", "ocv_frame",
				      &frame_cleanup,
				      flags, 0);

	//Initializes the camera
  bool result = init_camera();

  if (result)
    return 0;
  else
    return 1;
}

/* Retrieves an image form the uEye camera, and assigns the data to the frame
 * struct
*/
static ERL_NIF_TERM get_pic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

	// Retrieving a pointer of the image, and assigning it to an IplImage
  char* imgPointer;
  bool img_retrieved = get_image(imgPointer);
  IplImage* src = cvCreateImage(cvSize(752,480), IPL_DEPTH_8U, 1);
  src -> imageData = imgPointer;

	// Assigning  IplImage data to the frame struct
  frame_t* frame = (frame_t*)enif_alloc_resource(frame_res, sizeof(frame_t));
  frame->_frame = src ;

  ERL_NIF_TERM term = enif_make_resource_binary(env, frame, frame ,6);

  //enif_release_resource(frame);

  // returns tuple with ok if successful, otherwise returns image_not_retrieved
  if(img_retrieved)
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), term); 
  else
    return enif_make_tuple2(env, enif_make_atom(env, "image_not_retrieved"), term); 
  
}

/* Retrieves image data from the frame struct, and saves the image as jpg file
 * in images folder
*/
static ERL_NIF_TERM show_pic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

  frame_t* frame;
  if (!enif_get_resource(env, argv[0], frame_res, (void**) &frame)) {
    return enif_make_badarg(env);
 } 
  /*
  counter++;
  string basename = "images/fish_image";
  ostringstream filename;
  filename << basename << counter << ".jpg";
  cvSaveImage(filename.str().c_str(), frame -> _frame);  
  cvWaitKey(30);
  */
	
  cvShowImage("YOOHOO", frame->_frame);

  cvWaitKey(30);
	
  
  return enif_make_atom(env, "ok");
}

/* Deinitializes the uEye camera. If Erlang code runs get_pic, and exits
 * without running this function (deinit_camera), this can cause the camera's 
 * bus to crash and will require the system to be restarted
*/
static ERL_NIF_TERM deinit_camera(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

  deinit_camera();

  return enif_make_atom(env, "ok");
 
}

/* Nif function definitions */
static ErlNifFunc nif_funcs[] =
  {
    {"show_pic", 1, show_pic},
    {"get_pic", 0, get_pic},
		{"deinit_camera", 0, deinit_camera}
  };

ERL_NIF_INIT(imgproc_nif,nif_funcs,load,NULL,NULL,NULL)


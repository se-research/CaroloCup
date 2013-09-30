/* niftest.c */
#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/calib3d/calib3d.hpp>
#include <opencv2/highgui/highgui.hpp>

#include "erl_nif.h"
#include "iostream"
#include <sys/time.h>
#include <opencv/highgui.h>
#include <opencv/cv.h>
using namespace cv;
using namespace std;


ErlNifResourceType* frame_res = NULL;


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

  //  cout << src->width << endl;

  IplImage* gray = cvCreateImage(cvGetSize(src), IPL_DEPTH_8U, 1);
  cvCvtColor(src, gray, CV_RGB2GRAY);


  frame_t* frame = (frame_t*)enif_alloc_resource(frame_res, sizeof(frame_t));

  frame->_frame = gray ;

  ERL_NIF_TERM term = enif_make_resource_binary(env, frame, frame ,6);

  //enif_release_resource(frame);

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


static ERL_NIF_TERM read_complete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

  frame_t* frame;
  if (!enif_get_resource(env, argv[0], frame_res, (void**) &frame)) {
    return enif_make_badarg(env);
  }

  //cout << "PIC : " << frame->_frame->width << endl;

  //cout << "SEGMENT : " << segment << endl;

  ERL_NIF_TERM res[480*752];

  int count = 0; 
  int i;
  for( i = 0; i < 480*752; i++)
    {

      if(frame->_frame->imageData[i] > 100){
	int y = i / 752;
	int x = i % 752;
	ERL_NIF_TERM erl_x = enif_make_int(env, x);
	ERL_NIF_TERM erl_y = enif_make_int(env, y);
	res[count] = enif_make_tuple2(env, erl_x, erl_y);
	count++;
      }
    }

  //cout << endl << "DONE WITH FOR : " << count << endl;

  // enif_release_resource(frame);

  ERL_NIF_TERM points_erl = enif_make_list_from_array(env, res, count);
  //free(res);

  //cout << "ARRAY HERE READY" << endl;
  return points_erl;
}


static ERL_NIF_TERM read_part(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

  frame_t* frame;
  if (!enif_get_resource(env, argv[0], frame_res, (void**) &frame)) {
    return enif_make_badarg(env);
  }

  //cout << "PIC : " << frame->_frame->width << endl;

  int segment;
  enif_get_int(env, argv[1], &segment);
  
  //cout << "SEGMENT : " << segment << endl;

  ERL_NIF_TERM res[480*752/4];

  int count = 0; 
  int i;

  for( i = (segment -1) * (480*752/4); i < segment * 480*752/4; i++)
    {

      if(frame->_frame->imageData[i] > 100){
	int y = i / 752;
	int x = i % 752;
	ERL_NIF_TERM erl_x = enif_make_int(env, x);
	ERL_NIF_TERM erl_y = enif_make_int(env, y);
	res[count] = enif_make_tuple2(env, erl_x, erl_y);
	count++;
      }
    }
  //cout << endl << "DONE WITH FOR : " << count << endl;

  // enif_release_resource(frame);

  ERL_NIF_TERM points_erl = enif_make_list_from_array(env, res, count);
  //free(res);

  //cout << "ARRAY HERE READY" << endl;
  return points_erl;
}

static ERL_NIF_TERM process_complete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

  frame_t* frame;
  if (!enif_get_resource(env, argv[0], frame_res, (void**) &frame)) {
    return enif_make_badarg(env);
  }

  //cout << "PIC : " << frame->_frame->width << endl;


  ERL_NIF_TERM res[480*752];

  int count = 0; 
  int i,j;

  int min_x=751, max_x=0;
  
  int plus_minus = 10;

  int row = 479*752;
  bool found = false;
  while(!found)
    {
      for( i = 0; i< 752; i++)
	{
	  if((uchar)(frame->_frame->imageData[row + i]) > 100)
	    {
	      int y = row / 752;
	      int x = i ;
	      ERL_NIF_TERM erl_x = enif_make_int(env, x);
	      ERL_NIF_TERM erl_y = enif_make_int(env, y);
	      cvLine(frame->_frame,cvPoint(x,y),cvPoint(x,y),CV_RGB(0,0,0),1,8,0);	      
	      res[count] = enif_make_tuple2(env, erl_x, erl_y);
	      count++;
	      if(min_x > x)
		min_x = x;
	      if(max_x < x)
		max_x = x;
	      found = true;
	      break;
	    }
	}
      row -= 752;
    }
  
  for(i=row; i>250*752; i=i-752)
    {
      for(j= MAX(0,min_x - plus_minus); j< MIN(752, min_x + (2*plus_minus) ); j++)
	{
	  if((uchar) (frame->_frame->imageData[i+j]) > 100)
	    {
	      int y = i / 752;
	      int x = j ;
	      ERL_NIF_TERM erl_x = enif_make_int(env, x);
	      ERL_NIF_TERM erl_y = enif_make_int(env, y);
	      cvLine(frame->_frame,cvPoint(x,y),cvPoint(x,y),CV_RGB(0,0,0),1,8,0);	      
	      res[count] = enif_make_tuple2(env, erl_x, erl_y);
	      count++;
	      if(min_x > x)
		min_x = x;
	      if(max_x < x)
		max_x = x;
	    }
	} 
    }

  

 
  //cout << endl << "DONE WITH FOR : " << count << endl;

  // enif_release_resource(frame);

  ERL_NIF_TERM points_erl = enif_make_list_from_array(env, res, count);
  //free(res);
    cvShowImage("Drawing_and_Text", frame->_frame);
  cvWaitKey();

  //cout << "ARRAY HERE READY" << endl;
  return points_erl;
}


static ERL_NIF_TERM trace_pic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

  frame_t* frame;
  if (!enif_get_resource(env, argv[0], frame_res, (void**) &frame)) {
    return enif_make_badarg(env);
  }
  int current_most_left = 751;
  int current_most_right = 0;
  int current_most_right_temp = 0;

  vector<vector<Point2i> > lanes;
  vector<Point2i> current_line;
  int plus_minus = 10;
  int row = 479, column;

  int current_row;
  int current_column;

  //cvLine(src,cvPoint(0,479),cvPoint(751,479), CV_RGB(0,0,255));
  bool bottom_found;
  while (row > 250/*most_covered_right - most_covered_left < 500*/) {
    //color++;
    bottom_found = false;
    column = 0;

    while (!bottom_found) {
      //			cout << "HERE1" << endl;
      for (column = 0; column < 751; column++) {
	if ((uint) (frame->_frame->imageData[row * 752 + column]) > 100) {
	  current_line.push_back(Point2i(column, row));
	  //					gray->imageData[current_row * 752 + current_column] = (char) 0;
	  /*
	  cvLine(gray, cvPoint(current_column, current_row),
	  	 cvPoint(current_column, current_row),
		 CV_RGB(0, 0, 0));
	  cvLine(src, cvPoint(column, row), cvPoint(column, row),
		 colors[color%3]);
	  */
	  current_row = row;
	  current_most_left = column;
	  current_most_right = column;
	  current_most_right_temp = column;
	  bottom_found = true;
	  break;
	}
      }
      row--;
    }

    //		cout << column << "," << row << endl;
    bool valid_row = true;
    while (valid_row && current_row > 250) {
      //			cout << "HERE2" << endl;
      valid_row = false;
      current_column = current_most_left - plus_minus;
      current_most_right = current_most_right_temp;
      current_most_right_temp = 0;
      int on_white = 0;
      bool hit_white = false;
      while ((on_white > 0 || !hit_white)
	     && current_column < current_most_right + plus_minus) {
	if ((uint) (frame->_frame->imageData[current_row * 752 + current_column])
	    > 100) {
	  //					cout << current_column << ", " << current_row << endl;
	  //					gray->imageData[current_row * 752 + current_column] = (char)0;
	  current_line.push_back(Point2i(column, row));
	  /*
	  cvLine(src, cvPoint(current_column, current_row),
		 cvPoint(current_column, current_row),
		 colors[color%3]);
	  cvLine(gray, cvPoint(current_column, current_row),
		 cvPoint(current_column, current_row),
		 CV_RGB(0, 0, 0));
	  */
	  if(current_column< current_most_left)
	    current_most_left = current_column;
	  if(current_column> current_most_right_temp)
	    current_most_right_temp = current_column;
	  hit_white = true;
	  on_white = 3;
	  valid_row = true;
	} else {
	  on_white--;
	}
	current_column++;
      }
      current_row--;
    }
  }
  return enif_make_int(env, current_line.size());
}


static ErlNifFunc nif_funcs[] =
  {
    {"show_pic", 1, show_pic},
    {"trace_pic", 1, trace_pic},
    {"get_pic", 0, get_pic},
    {"read_part", 2, read_part},
    {"read_complete", 1, read_complete},
    {"process_complete", 1, process_complete}
  };

ERL_NIF_INIT(niftest,nif_funcs,load,NULL,NULL,NULL)


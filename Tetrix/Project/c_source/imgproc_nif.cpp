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
#include "../c_include/imgproc_nif.hpp"

using namespace cv;
using namespace std;


ErlNifResourceType* frame_res = NULL;
int counter = 0; // counter used for naming saved .jpg files



//------------------------------------------------------------------------------
// C++ functions
//------------------------------------------------------------------------------

RotatedRect findrect(vector<vector<Point2i> > group) {
  vector<Point2i> set;
  for (unsigned int i = 0; i < group.size(); i++) {
    for (unsigned int j = 0; j < group[i].size(); j++) {
      set.push_back(group[i][j]);
    }
  }

  return minAreaRect(set);
}

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

static ERL_NIF_TERM process_pic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  // GET IMG FROM CAMERA
  frame_t* frame;
  if (!enif_get_resource(env, argv[0], frame_res, (void**) &frame)) {
    return enif_make_badarg(env);
  } 
  int image_counter;
  enif_get_int(env, argv[1], &image_counter);

  IplImage* gray = frame->_frame;
  //   GET IMG FROM CAMERA 

  //   if(image_counter % 5 == 0)
  //   {
  //     stringstream ss;//create a stringstream
  //     ss << "/home/tetrix/images/image" << image_counter/5 << ".jpg" ;//add number to the stream
  //     cvSaveImage(ss.str().c_str() , gray);
  //   }


  // IplImage* src = cvLoadImage("/home/robin/Downloads/pic.png");
  // IplImage* gray = cvCreateImage(cvGetSize(src), IPL_DEPTH_8U, 1);
  // cvCvtColor(src, gray, CV_RGB2GRAY);

  int row = 440, column;

  int current_row;

  range line_range;
  line_range.length = 25;

  vector<vector<vector<Point2i> > > lanes;

   
  while (row > LINECOVERAGE) {
    bool bottom_found;
    bottom_found = false;
    vector<vector<Point2i> > current_line;
    while (!bottom_found) {
      for (column = 151; column < 600; column++) {
	if ((uint) (gray->imageData[row * 752 + column]) > 100) {
	  gray->imageData[row * 752 + column] = (char) 0;
	  current_row = row;
	  bottom_found = true;
	  line_range.mid = column;
	  break;
	}
      }
      row -= 5;
    }
      
    int valid_row = 3;
    while (valid_row > 0) {
      valid_row = false;
      int on_white = 0;
      bool hit_white = false;

      line_range.length = (((current_row - LINECOVERAGE) / (480.0 - LINECOVERAGE)) * 30.0) + 10;

      line_range.left = MAX(151,line_range.mid - line_range.length);
      line_range.right = MIN(600,line_range.mid + line_range.length);

      column = line_range.left;

      vector<Point2i > single_line;

      while ( (!hit_white && column < line_range.right) || (on_white > 0 && column < 600)) {
	if ((uint) (gray->imageData[current_row * 752 + column]) > 100) {
	  gray->imageData[current_row * 752 + column] = (char) 0;
	  single_line.push_back(Point2i(column, current_row));
	  hit_white = true;
	  on_white = 3;
	  valid_row = 3;
	} else {
	  on_white--;
	}
	column++;
      }
      if (valid_row ==  3) {

	CvPoint center;
	center.x = (single_line[0].x + single_line[single_line.size() - 1].x) / 2;
	center.y = current_row;

	vector<Point2i> left_right_points;
	left_right_points.push_back(Point2i(single_line[0].x ,current_row));
	left_right_points.push_back(Point2i(single_line[single_line.size() - 1].x ,current_row));

	current_line.push_back(left_right_points);
	line_range.mid = center.x;
      } else {
	valid_row--;
      }
      current_row--;
    }
    if (current_line.size() > 1 )
      lanes.push_back(current_line);

  } // end of frame while


  unsigned int lanes_size = lanes.size();
  for (unsigned int i = 0; i < lanes.size(); i++) {
    if(lanes[i].size() > 11){
      for (unsigned int j = 5; j < lanes[i].size()-6; j++) {
	Point2f start;
	start.x = (lanes[i][j-5][0].x + lanes[i][j-5][1].x) / 2;
	start.y = lanes[i][j-5][0].y;

	Point2f end;
	end.x = (lanes[i][j+5][0].x + lanes[i][j+5][1].x) / 2;
	end.y = lanes[i][j+5][0].y;

	Point2f center;
	center.x = (lanes[i][j][0].x + lanes[i][j][1].x) / 2;
	center.y = lanes[i][j][0].y;

	float alpha = get_angle(end,center,start);
	if((alpha > 80) || ( alpha < -40) ){
	}else{
	  vector<vector<Point2i> > temp;
	  int c= 0;
	  for (unsigned int k = j; k < lanes[i].size(); k++) {
	    temp.push_back(lanes[i][k]);
	    c++;
	  }
	  lanes.push_back(temp);
	  for (int k = 0; k < c; k++) {
	    lanes[i].pop_back();
	  }
	  j = lanes[i].size();
	  break;
	}
      }
    }
  }

  vector<vector<vector<vector<Point2i> > > > grouped;

  vector<vector<vector<Point2i> > > temp;
  temp.push_back(lanes[0]);
  grouped.push_back(temp);
  
  for (unsigned int i = 1; i < lanes_size; i++) {
    bool connected = false;
    Point2f lanes_center = Point2f((lanes[i][0][0].x  + lanes[i][0][1].x)/2 , lanes[i][0][0].y);
    for (unsigned int j = 0; j < grouped.size(); j++) {
      Point2f grouped_point_left = 
	grouped[j][grouped[j].size()-1][grouped[j][grouped[j].size()-1].size()-1][0];
      Point2f grouped_point_right = 
	grouped[j][grouped[j].size()-1][grouped[j][grouped[j].size()-1].size()-1][1];
      Point2f grouped_point_center = 
	Point2f((grouped_point_left.x + grouped_point_right.x)/2 , grouped_point_left.y);
      float distance_group = dist(grouped_point_center , lanes_center);

      if(grouped_point_center.y >= lanes_center.y && distance_group < 100){

	int first  = div(grouped[j][grouped[j].size()-1].size() , 4).quot;
	int last = grouped[j][grouped[j].size()-1].size() - first -1;
	Point2f start = 
	  center_point(grouped[j][grouped[j].size()-1][first][0], 
		       grouped[j][grouped[j].size()-1][first][1]);
	Point2f end = 
	  center_point(grouped[j][grouped[j].size()-1][last][0], 
		       grouped[j][grouped[j].size()-1][last][1]);
	int x = (((start.x - end.x) * (lanes_center.y - start.y)) / (start.y - end.y)) + start.x;
	
	float distance = dist(lanes_center , Point2f(x,lanes_center.y));
	if(distance < 25){
	  if(!is_trash(lanes[i])){
	    grouped[j].push_back(lanes[i]);
	  }
	  connected = true;
	}
      }//end of if(grouped_point_center... )
	

    }//end of for(unsigned int j... )
    if(!connected){
      if(!is_trash(lanes[i])){
	temp.clear();
	temp.push_back(lanes[i]);
	grouped.push_back(temp);
      }
    }
  }//end of for(unsigned int i... )


  int dash_index = find_dashed(grouped);

  if(dash_index == -1) 
    return enif_make_atom(env, "not_found");

  

  ERL_NIF_TERM final_result[grouped[dash_index].size()];

  for(unsigned int i = 0; i< grouped[dash_index].size() ; i++)
    {

      //============================================================      
      RotatedRect rr = findrect(grouped[dash_index][i]);	
      Point2f rect_points[4];
      rr.points(rect_points);
      
      Point2f bl;
      Point2f br;
      Point2f tl;
      Point2f tr;
      Point2f tc;
      Point2f bc;
      Point2f cc;
      
      if(dist(rect_points[0], rect_points[1]) > dist(rect_points[0], rect_points[3])){
	bl = rect_points[0];
	br = rect_points[3];
	tl = rect_points[1];
	tr = rect_points[2];
      }else{
	bl = rect_points[1];
	br = rect_points[0];
	tl = rect_points[2];
	tr = rect_points[3];
      }
      
      tc = center_point(tl,tr);
      bc = center_point(bl,br);
      cc = rr.center;

      ERL_NIF_TERM tl_x = enif_make_int(env, (int) tl.x);
      ERL_NIF_TERM tl_y = enif_make_int(env, (int) tl.y);

      ERL_NIF_TERM tr_x = enif_make_int(env, (int) tr.x);
      ERL_NIF_TERM tr_y = enif_make_int(env, (int) tr.y);

      ERL_NIF_TERM bl_x = enif_make_int(env, (int) bl.x);
      ERL_NIF_TERM bl_y = enif_make_int(env, (int) bl.y);

      ERL_NIF_TERM br_x = enif_make_int(env, (int) br.x);
      ERL_NIF_TERM br_y = enif_make_int(env, (int) br.y);

      ERL_NIF_TERM tc_x = enif_make_int(env, (int) tc.x);
      ERL_NIF_TERM tc_y = enif_make_int(env, (int) tc.y);

      ERL_NIF_TERM cc_x = enif_make_int(env, (int) cc.x);
      ERL_NIF_TERM cc_y = enif_make_int(env, (int) cc.y);

      ERL_NIF_TERM bc_x = enif_make_int(env, (int) bc.x);
      ERL_NIF_TERM bc_y = enif_make_int(env, (int) bc.y);
      

      ERL_NIF_TERM c_point = enif_make_tuple2(env, cc_x, cc_y);
      
      ERL_NIF_TERM box = enif_make_tuple4(env, 
					  enif_make_tuple2(env, bl_x, bl_y),
					  enif_make_tuple2(env, tl_x, tl_y),
					  enif_make_tuple2(env, tr_x, tr_y),
					  enif_make_tuple2(env, br_x, br_y));

      ERL_NIF_TERM points = enif_make_tuple3(env, 
					     enif_make_tuple2(env, bc_x, bc_y), 
					     enif_make_tuple2(env, cc_x, cc_y),
					     enif_make_tuple2(env, tc_x, tc_y));

      ERL_NIF_TERM result = enif_make_tuple2(env,
					     c_point, 
					     enif_make_tuple2(env, box, points));
					     

      final_result[i] = result;
      
    }//end of for
  


  ERL_NIF_TERM points_erl = enif_make_list_from_array(env, final_result, grouped[dash_index].size());

  
  return points_erl;
}

/* Nif function definitions */
static ErlNifFunc nif_funcs[] =
  {
    {"show_pic", 1, show_pic},
    {"get_pic", 0, get_pic},
    {"process_pic" , 2, process_pic},
    {"deinit_camera", 0, deinit_camera}
  };

ERL_NIF_INIT(imgproc_nif,nif_funcs,load,NULL,NULL,NULL)


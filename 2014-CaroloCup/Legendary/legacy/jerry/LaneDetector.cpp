#ifdef PANDABOARD
#include<stdc-predef.h>
#endif


#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/data/image/SharedImage.h"
#include "core/io/ContainerConference.h"
#include "core/wrapper/SharedMemoryFactory.h"


// Data structures from msv-data library:
#include "SteeringData.h"
#include "SensorBoardData.h"
#include "LaneDetector.h"
#include <time.h>

namespace msv
{

  using namespace std ;
  using namespace cv ;
  using namespace core :: base ;
  using namespace core :: data ;
  using namespace core :: data :: image ;

  LaneDetector :: LaneDetector(const int32_t&argc,char**argv):
    ConferenceClientModule(argc,argv,"lanedetector"),
    m_hasAttachedToSharedImageMemory(false),
    m_sharedImageMemory(),
    m_image(NULL),
    m_cameraId(-1),
    m_debug(false),
    stopSeconds(0),
    lastAngle(0)
  {
    for (int i = 0; i< 180; i++) prevSteerAngle[i] = 0;
  }

  LaneDetector ::~LaneDetector()
  {
  }

  void LaneDetector :: setUp()
  {
    // This method will be call automatically _before_ running body().
    if(m_debug)
    {
      // Create an OpenCV-window.
      cvNamedWindow("WindowShowImage",CV_WINDOW_AUTOSIZE);
      cvMoveWindow("WindowShowImage",300,100);
    }
  }

  void LaneDetector :: tearDown()
  {
    // This method will be call automatically _after_ return from body().
    if(m_image!=NULL)
    {
      cvReleaseImage(&m_image);
    }

    if(m_debug)
    {
      cvDestroyWindow("WindowShowImage");
    }
  }

  bool LaneDetector :: readSharedImage(Container&c)
  {
    bool retVal=false ;

    if(c.getDataType()==Container :: SHARED_IMAGE)
    {
      SharedImage si=c.getData<SharedImage>();

      // Check if we have already attached to the shared memory.
      if(!m_hasAttachedToSharedImageMemory)
      {
        m_sharedImageMemory
          =core :: wrapper :: SharedMemoryFactory :: attachToSharedMemory(
              si.getName());
      }

      // Check if we could successfully attach to the shared memory.
      if(m_sharedImageMemory->isValid())
      {
        // Lock the memory region to gain exclusive access. REMEMBER!!! DO NOT FAIL WITHIN lock() / unlock(), otherwise, the image producing process would fail.
        m_sharedImageMemory->lock();
        {
          const uint32_t numberOfChannels=3 ;
          // For example, simply show the image.
          if(m_image==NULL)
          {
            m_image=cvCreateImage(cvSize(si.getWidth(),si.getHeight()),IPL_DEPTH_8U,numberOfChannels);
          }

          // Copying the image data is very expensive...
          if(m_image!=NULL)
          {
            memcpy(m_image->imageData,
                m_sharedImageMemory->getSharedMemory(),
                si.getWidth()*si.getHeight()*numberOfChannels);
          }
        }

        // Release the memory region so that the image produce (i.e. the camera for example) can provide the next raw image data.
        m_sharedImageMemory->unlock();

        // Mirror the image.
        cvFlip(m_image,0,-1);

        retVal=true ;
      }
    }
    return retVal ;
  }

  ///////According to the lines' angle from pictures from camera, to calculate steer data which the car need to steer.
  double get_steer_data_by_angle(double Angle)
  {
    double Angle1 = fabs(Angle); /////angles we need to steer when angles < 0 . (turning left)
    double steerdata[][3] = {
      {0,   5, 0},
      {5,  10, 0.3},
      {10, 20, 1},
      {20, 30, 1.5},
      {30, 40, 2},
      {40, 50, 2.5},
      {50, 60, 3},
      {60, 70, 3.5},
      {70, 75, 4},
      {75, 80, 8.5},
      {80, 85, 20},
      {85, -1, 0}};

    for (int i = 0; i < 20; i++)
    {
      //////When angle >0 (turning right), we need to optimize the angles based on the real time experiments on simulation.
      if (Angle1 >= steerdata[i][0] && (Angle1 <= steerdata[i][1] || steerdata[i][1] < 0))
      {
        double Acc=0;
        if (i == 0) return 0;
        if (Angle > 30) Acc = 0.5;
        if (Angle > 50) Acc = 1.5;
        if (Angle > 70) Acc = 2.0;
        if (Angle > 85) Acc = 0;
        return Angle/Angle1 * steerdata[i][2] + Acc; //////angles we need to steer when angles >0.
      }
    }

    return 0;
  }

  ////////according to the offset position of the car, get the offset steer data we need to adjust.
  double get_offset_steer_data(int Offset)
  {
    double Offsetdata[][3] =
    {
      {0, 5, 0},
      {0, 30, -5},
      {0, 40, -2},
      {0, 90, 0},
      {90, 95, -8},
      {95, -1, 0}
    };

    for (int i = 0; i < 20; i++)
    {
      if (Offset >= Offsetdata[i][0] && (Offset <= Offsetdata[i][1] || Offsetdata[i][1] < 0))
      {
        return Offsetdata[i][2];
      }
    }

    return 0;
  }

  /////////According to the pictures, get the offset coordinate  of the vehicle.
  int get_offset_data(Mat frontLine, double Angle)
  {
    int offset=0;

    for (int i = frontLine.rows - 1; i >= 0; i--) {
      if (Angle > 0) ///////turning right. search from right to left, get the rightmost lines as offset value.
      {
        for (int j = frontLine.cols - 1; j >= 0; j--)
        {
          if (frontLine.at<Vec3b>(Point(j,i))[2]==255) return j;//////get white points.
        }
      }
      else
      {
        for (int j = 0; j < frontLine.cols; j++)/////////// search from left to right, get the leftmost lines as offset value.
        {

          if (frontLine.at<Vec3b>(Point(j,i))[2]==255) return j;//////get the white points.
        }
      }
    }

    return offset;

  }


  bool check_crossrode(Mat src, int mode)
  {
    ///////get lines from the pictures.
    Mat dst,cdst ;
    ///////edge process.
    Canny(src.reshape(1),dst,50,200,3);
    /////// pixel process.
    cvtColor(dst,cdst,CV_GRAY2BGR);

    vector<Vec2f>lines ;
    /////// get line after Hough transform.
    HoughLines(cdst.reshape(1),lines,1,CV_PI/180,20,0,0);

    //imshow("check_crossrode", cdst);
    //cvWaitKey(10);

    return lines.size()>0 && mode == 0;
  }

  // You should start your work in this method.
  void LaneDetector :: processImage() /////reference to group 6's
  {
    double max_speed=2 ; /////reference to group 6's
    int delayY=18/max_speed ;/////reference to group 6's

    //////// Convert m_image to Mat source.
    Mat source(m_image,false); /////reference to group 6's

    //////// Get the birdview.
    Size taille=source.size(); /////reference to group 6's
    Mat transfo = translate(source); /////reference to group 6's
    Mat birdView ; /////reference to group 6's
    warpPerspective(source,birdView,transfo,taille,INTER_CUBIC|WARP_INVERSE_MAP);

    int mode=0 ;
    int callMode=0;
    ///////Get summuation angles of vehicle when driving on the lane on the previous time. (when the vehicle driving, the system would save driving angles into Array preAngles everytime.)
    /////// according to the summation of angles, we can define the vehicle on which driving mode; 0 (go straight) ; 1( turn left); 2( tunring right).
    for(int i=0;i<delayY;i++)callMode=callMode+prevSteerAngle[i];
    if(callMode>20)mode=2 ;
    else if(callMode<-20)mode=1 ;
    else mode=0 ;

    ///////Get different birdviews according to different dirving mode.
    Mat leftcorrec ;
    switch (mode)
    {
      case 1: leftcorrec=birdView(cv :: Rect(200,380,100,100));
      case 0: leftcorrec=birdView(cv :: Rect(225,380,100,100));
      case 2: leftcorrec=birdView(cv :: Rect(250,380,100,100));
              break;
    }

    ////////after imagine processing, get coordinates of two points on the line.
    Point pt1(0,0);
    Point pt2(0,0);
    Mat midleLine=GetLines(leftcorrec,&pt1,&pt2,20);
    /////// Get vaules of angles according to coordinates of two points.
    double midleAngle=GetSlope(pt1,pt2);

    Point pt5(0,0);
    Point pt6(0,0);
    Mat frontInters = birdView(cv :: Rect(225,380,100,100));
    Mat frontLine=GetLines(frontInters,&pt5,&pt6,20);
    int offset=get_offset_data(frontInters, midleAngle);

    imshow("midleLine", frontLine);
    cvWaitKey(10);

    double steerAngle=0 ;
    /////////Get the actual steer angles according to summuation of steerdata angles and offset steer data angle.
    steerAngle = get_steer_data_by_angle(midleAngle);
    double offsetAngle = get_offset_steer_data(offset);
    cerr<<__LINE__<<".."<<steerAngle<<"..."<<offsetAngle<<"...."<<offset<<"...."<<midleAngle<<endl;
    steerAngle += offsetAngle;

    double speed=-1;
    time_t rawtime;
    time ( &rawtime );
    if (stopSeconds > float(rawtime)) //////// Vehicle pause when checked the cross road.
    {
      cerr<<__LINE__<<"2222cross...."<<stopSeconds<<"..."<<rawtime<<endl;
      steerAngle = 0;
      speed = 0;
    }
    /////////
    bool cross = false; //check_crossrode(birdView(cv::Rect(300,400,40,10)), mode);////////Check the cross road.
    if (cross)
    {
      if (fabs(stopSeconds) < 0.01) /////// The first time get into intersection.
      {
        steerAngle = 0;
        speed = 0;
        Container containerSensorBoardData=getKeyValueDataStore().get(Container :: USER_DATA_0);
        SensorBoardData sbd=containerSensorBoardData.getData<SensorBoardData>();
        if ((sbd.getDistance(3) > 0) || (sbd.getDistance(0) > 0 && sbd.getDistance(0) < 10)) stopSeconds = float(rawtime) + 30;
        else stopSeconds = float(rawtime) + 10;

        cerr<<__LINE__<<"11111cross........"<<sbd.getDistance(3)<<"..."<<sbd.getDistance(0)<<endl;
      }
      else if (fabs(stopSeconds - double(rawtime)) < 5)
      {
        cerr<<__LINE__<<"3333cross................cross.........."<<endl;
        ///////After passed the intersection in 5 seconds, if still detected in the cross road, then keep drving.
        steerAngle = 0;
        speed = -1;
      }
      else
      {
        cerr<<__LINE__<<"4444ffffffffffff..."<<double(rawtime) - 20<<endl;
        //////// get into intersection again.
        steerAngle = 0;
        speed = 0;
        stopSeconds = double(rawtime) + 5;
      }
    }

    /////////Save the current angle that vehicle need to steer into Array prevAngles.
    for(int i=delayY;i>0;i--)prevSteerAngle[i]=prevSteerAngle[i-1];
    prevSteerAngle[0]=steerAngle ;//////// replace the oldest data.

    SteeringData steerData ;
    steerData.setExampleData(steerAngle);
    // Create containers for finally sending the data.
    Container contSteer(Container :: USER_DATA_1,steerData);

    SteeringData speedData;
    speedData.setExampleData(speed);
    Container contSpeed(Container::USER_DATA_2, speedData);

    // Send containers.
    getConference().send(contSteer);
    getConference().send(contSpeed);
  }

  Mat LaneDetector :: GetLines(Mat src,Point*pt1,Point*pt2,int res)
  {
    ///////Get lines from pictures.
    Mat dst,cdst ;
    //////Edge process.
    Canny(src.reshape(1),dst,50,200,3);
    //////Pixel process.
    cvtColor(dst,cdst,CV_GRAY2BGR);

    vector<Vec2f>lines ;
    ///////Get lines after Hough transform.
    HoughLines(cdst.reshape(1),lines,1,CV_PI/180,res,0,0);

    if(lines.size()>0)
    {
      /////////This part is from openCV sources, mainly to help get lines from pictures, and get points on the lines.

      float rho=lines[0][0],theta=lines[0][1];
      //cerr<<__LINE__<<"..midleLine..("<<lines[0][0]<<","<<lines[lines.size() - 1][0]<<")"<<endl;
      double a=cos(theta),b=sin(theta);
      double x0=a*rho,y0=b*rho ;
      (*pt1).x=cvRound(x0+1000*(-b));
      (*pt1).y=cvRound(y0+1000*(a));
      (*pt2).x=cvRound(x0-1000*(-b));
      (*pt2).y=cvRound(y0-1000*(a));
      //cerr<<__LINE__<<"..midleLine..("<<(*pt1).x<<","<<(*pt1).y<<"), ("<<(*pt2).x<<","<<(*pt2).y<<").."<<endl;
    }

    return cdst ;
  }

  float LaneDetector :: GetSlope(Point pt1,Point pt2)
  {
    float fSlope ;
    float fAngle=90 ;
    float fRun=pt2.x-pt1.x ;

    if(fabs(fRun) > 0.001)
    {
      fSlope=(pt2.y-pt1.y)/fRun ;
      fAngle=(float)(180.0*atan((double)fSlope)/CV_PI);
    }

    if(fAngle>0) fAngle = fAngle - 90;
    else fAngle=(fAngle+90) ;

    return fAngle ;
  }

  Mat LaneDetector :: translate(Mat source)
  {
    ///////// the function below , mainly used to convert pictures from camera to birdview. (Reference to group 6th, already informed to the group 6th)
    /////////  the function below is pictures conversion method from OpenCV sources.
    int alpha_=8.,beta_=90.,gamma_=90. ;
    int f_=548,dist_=338 ;
    double f,dist ;
    double alpha,beta,gamma ;
    alpha=((double)alpha_-90.)*CV_PI/180 ;
    beta=((double)beta_-90.)*CV_PI/180 ;
    gamma=((double)gamma_-90.)*CV_PI/180 ;
    f=(double)f_ ;
    dist=(double)dist_ ;
    Size taille=source.size();
    double w=(double)taille.width,h=(double)taille.height ;
    // Projection 2D -> 3D matrix
    Mat A1=(Mat_<double>(4,3)<<
        1,0,-w/2,
        0,1,-h/2,
        0,0,0,
        0,0,1);

    // Rotation matrices around the X,Y,Z axis
    Mat RX=(Mat_<double>(4,4)<<
        1,0,0,0,
        0,cos(alpha),-sin(alpha),0,
        0,sin(alpha),cos(alpha),0,
        0,0,0,1);

    Mat RY=(Mat_<double>(4,4)<<
        cos(beta),0,-sin(beta),0,
        0,1,0,0,
        sin(beta),0,cos(beta),0,
        0,0,0,1);

    Mat RZ=(Mat_<double>(4,4)<<
        cos(gamma),-sin(gamma),0,0,
        sin(gamma),cos(gamma),0,0,
        0,0,1,0,
        0,0,0,1);

    // Composed rotation matrix with (RX,RY,RZ)
    Mat R=RX*RY*RZ ;

    // Translation matrix on the Z axis change dist will change the height
    Mat T=(Mat_<double>(4,4)<<
        1,0,0,0,
        0,1,0,0,
        0,0,1,dist,
        0,0,0,1);

    // Camera Intrisecs matrix 3D -> 2D
    Mat A2=(Mat_<double>(3,4)<<
        f,0,w/2,0,
        0,f,h/2,0,
        0,0,1,0);

    // Final and overall transformation matrix
    Mat transfo=A2*(T*(R*A1));

    return transfo;
  }

  // This method will do the main data processing job.
  // Therefore, it tries to open the real camera first. If that fails, the virtual camera images from camgen are used.
  ModuleState :: MODULE_EXITCODE LaneDetector :: body()
  {
    // Get configuration data.
    KeyValueConfiguration kv=getKeyValueConfiguration();
    m_cameraId=kv.getValue<int32_t>("lanedetector.camera_id");
    m_debug=kv.getValue<int32_t>("lanedetector.debug")==1 ;

    bool use_real_camera=true ;

    // Try to open the camera device.
    CvCapture*capture=cvCaptureFromCAM(m_cameraId);
    if(!capture)
    {
      cerr<<"Could not open real camera; falling back to SHARED_IMAGE."<<endl ;
      use_real_camera=false ;
    }

    while(getModuleState()==ModuleState :: RUNNING)
    {
      bool has_next_frame=false ;

      // Use the shared memory image.
      if(!use_real_camera)
      {
        // Get the most recent available container for a SHARED_IMAGE.
        Container c=getKeyValueDataStore().get(Container :: SHARED_IMAGE);

        if(c.getDataType()==Container :: SHARED_IMAGE)
        {
          // Example for processing the received container.
          has_next_frame=readSharedImage(c);
        }
      }
      else
      {
        // Use the real camera.
        if(cvGrabFrame(capture))
        {
          m_image=cvRetrieveFrame(capture);
          has_next_frame=true ;
        }
      }

      // Process the read image.
      if(true==has_next_frame)
      {
        processImage();
      }

      if(use_real_camera)
      {
        // Unset m_image only for the real camera to avoid memory leaks.
        m_image=NULL ;
      }
    }

    if(capture!=NULL)
    {
      cvReleaseCapture(&capture);
    }

    return ModuleState :: OKAY ;
  }


}
// msv

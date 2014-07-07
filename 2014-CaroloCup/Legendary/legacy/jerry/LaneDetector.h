#ifndef LANEDETECTOR_H_
#define LANEDETECTOR_H_

#ifdef PANDABOARD
#include<stdc-predef.h>
#endif
/*
#include "opencv/opencv.hpp"
#include "opencv/objdetect/objdetect.hpp"
#include "opencv/highgui/highgui.hpp"
#include "opencv/imgproc/imgproc.hpp"
*/
#include<iostream>
#include<opencv/highgui.h>
#include<cmath>
#include<time.h>

#include<opencv/cv.h>
#include "core/SharedPointer.h"
#include "core/base/ConferenceClientModule.h"
#include "core/wrapper/SharedMemory.h"

namespace msv
{

  using namespace std ;

  /**
   * This class is an exemplary skeleton for processing video data.
   */
  class LaneDetector :
    public core :: base :: ConferenceClientModule
  {
    private :
      /**
       * "Forbidden" copy constructor. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the copy constructor.
       *
       * @param obj Reference to an object of this class.
       */
      /*obj*/
      LaneDetector(const LaneDetector&);

      /**
       * "Forbidden" assignment operator. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the assignment operator.
       *
       * @param obj Reference to an object of this class.
       * @return Reference to this instance.
       */
      /*obj*/
      LaneDetector&operator=(const LaneDetector&);


    public :
      /**
       * Constructor.
       *
       * @param argc Number of command line arguments.
       * @param argv Command line arguments.
       */
      LaneDetector(const int32_t&argc,char**argv);

      virtual~LaneDetector();

      core :: base :: ModuleState :: MODULE_EXITCODE body();

    protected :
      /**
       * This method is called to process an incoming container.
       *
       * @param c Container to process.
       * @return true if c was successfully processed.
       */
      bool readSharedImage(core :: data :: Container&c);

    private :
      bool m_hasAttachedToSharedImageMemory ;
      core :: SharedPointer<core :: wrapper :: SharedMemory>m_sharedImageMemory ;
      IplImage*m_image ;
      uint32_t m_cameraId ;
      bool m_debug ;
      double prevSteerAngle[180];
      float stopSeconds;
      double lastAngle;

      virtual void setUp();

      virtual void tearDown();

      void processImage();
      float GetSlope(cv :: Point pt1,cv :: Point pt2);
      cv :: Mat GetLines(cv :: Mat src,cv :: Point*pt1,cv :: Point*pt2,int res);
      cv :: Mat translate(cv :: Mat);
  }
  ;

}
// msv

#endif /*LANEDETECTOR_H_*/

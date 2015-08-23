/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef DRIVER_H_2
#define DRIVER_H_2

#include "core/base/module/TimeTriggeredConferenceClientModule.h"
#include "LaneDetectionData.h"
#include "opencv2/opencv.hpp"


namespace msv {

  using namespace std;
  using namespace cv;

  /**
   * This class is a skeleton to send driving commands to Hesperia-light's vehicle driving dynamics simulation.
   */
  class laneDriver : public core::base::module::TimeTriggeredConferenceClientModule {
    private:
      /**
       * "Forbidden" copy constructor. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the copy constructor.
       *
       * @param obj Reference to an object of this class.
       */
      laneDriver(const laneDriver &/*obj*/);

      /**
       * "Forbidden" assignment operator. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the assignment operator.
       *
       * @param obj Reference to an object of this class.
       * @return Reference to this instance.
       */
      laneDriver& operator=(const laneDriver &/*obj*/);

      bool laneFollowing(LaneDetectionData* data);
    

    public:
      /**
       * Constructor.
       *
       * @param argc Number of command line arguments.
       * @param argv Command line arguments.
       */
      laneDriver(const int32_t &argc, char **argv);

      virtual ~laneDriver();

      coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode body();

    private:
      virtual void setUp();
      virtual void tearDown();
    float
    calculateDesiredHeading (float oldLateralError);

    void calculateErr(CustomLine ,CustomLine ,float *, double *);

      bool m_hasReceivedLaneDetectionData;
      bool after_intersection;

      // Define control parameters
      float m_controlGains[3];	//For feedback linearization controller
      float m_angularError;
      float m_speed;
      double m_lateralError;
      double m_intLateralError;
      double m_derLateralError;
      float m_desiredSteeringWheelAngle;
      float m_propGain;
      float m_intGain;
      float m_derGain;
      //core::wrapper::SerialPort *m_serialPortPtr;
      const float SCALE_FACTOR;	//For example, 12000 dpm (dots-per-meter)

      int32_t m_timestamp;

      Vec4i m_leftLine;
      Vec4i m_rightLine;
      Vec4i m_dashedLine;
  };

} // carolocup

#endif /*DRIVER_H_*/

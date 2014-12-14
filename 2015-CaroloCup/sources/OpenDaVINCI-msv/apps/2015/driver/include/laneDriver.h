/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef DRIVER_H_2
#define DRIVER_H_2

#include "core/base/ConferenceClientModule.h"
#include "LaneDetectionData.h"
#include "opencv2/opencv.hpp"


namespace msv {

  using namespace std;
  using namespace cv;

  /**
   * This class is a skeleton to send driving commands to Hesperia-light's vehicle driving dynamics simulation.
   */
  class laneDriver : public core::base::ConferenceClientModule {
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

      core::base::ModuleState::MODULE_EXITCODE body();

    private:
      virtual void setUp();
      virtual void tearDown();

      bool m_hasReceivedLaneDetectionData;

      // Define control parameters
      float m_controlGains[3];	//For feedback linearization controller
      float m_deltaPath;
      float m_angularError;
      float m_steeringWheelAngle;
      float m_curvature;
      float m_curvatureDifferential;
      float m_oldCurvature;
      float m_speed;
      double m_lateralError;
      double m_intLateralError;
      double m_derLateralError;
      float m_desiredSteeringWheelAngle;
      float m_scaledLength;
      float m_propGain;
      float m_intGain;
      float m_derGain;
      float m_length;
      float m_wheelRadius;
      //core::wrapper::SerialPort *m_serialPortPtr;
      const float ANGLE_TO_CURVATURE;
      const float SCALE_FACTOR;	//For example, 12000 dpm (dots-per-meter)

      int32_t m_timestamp;

      Vec4i m_leftLine;
      Vec4i m_rightLine;
      Vec4i m_dashedLine;
  };

} // carolocup

#endif /*DRIVER_H_*/

/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef DRIVER_H_2
#define DRIVER_H_2

#include "core/base/ConferenceClientModule.h"
#include "core/wrapper/SerialPort.h"
#include "core/wrapper/SerialPortFactory.h"
#include "LaneDetectionData.h"
#include "opencv2/opencv.hpp"
#include "ArduinoMegaProtocol.h"

namespace carolocup {

  using namespace std;
  using namespace cv;

  /**
   * This class is a skeleton to send driving commands to Hesperia-light's vehicle driving dynamics simulation.
   */
  class Driver : public core::base::ConferenceClientModule {
    private:
      /**
       * "Forbidden" copy constructor. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the copy constructor.
       *
       * @param obj Reference to an object of this class.
       */
      Driver(const Driver &/*obj*/);

      /**
       * "Forbidden" assignment operator. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the assignment operator.
       *
       * @param obj Reference to an object of this class.
       * @return Reference to this instance.
       */
      Driver& operator=(const Driver &/*obj*/);

      /**
       * The control algorithm
       * @param lateralError The current lateral error
       * @param angularError The current angular error
       * @param curvature The current curvature
       * @param curvatureDifferential Approximate derivative of the curvature w.r.t. path coordinate s
       * @param steeringWheelAngle
       * @param The current steering wheel angle
       * @param speed The current set speed
       * @param controlGains Pointer to vector of control gains
       * @return The new desired steeringWheelAngle
       */
      float feedbackLinearizationController();
			float feedbackLinearizationController2();

    public:
      /**
       * Constructor.
       *
       * @param argc Number of command line arguments.
       * @param argv Command line arguments.
       */
      Driver(const int32_t &argc, char **argv);

      virtual ~Driver();

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
      ArduinoMegaProtocol m_protocol;
      const float ANGLE_TO_CURVATURE;
      const float SCALE_FACTOR;	//For example, 12000 dpm (dots-per-meter)

      int32_t m_timestamp;

      Vec4i m_leftLine;
      Vec4i m_rightLine;
      Vec4i m_dashedLine;
  };

} // carolocup

#endif /*DRIVER_H_*/

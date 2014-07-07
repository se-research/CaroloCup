/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MAPPINGNEW_H_2
#define MAPPINGNEW_H_2

#include "core/base/ConferenceClientModule.h"
#include "core/wrapper/SerialPort.h"
#include "core/wrapper/SerialPortFactory.h"
#include "opencv2/opencv.hpp"


namespace carolocup {

  using namespace std;
  using namespace cv;



  class MappingNew : public core::base::ConferenceClientModule {
    private:
      /**
       * "Forbidden" copy constructor. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the copy constructor.
       *
       * @param obj Reference to an object of this class.
       */
      MappingNew(const MappingNew &/*obj*/);

      /**
       * "Forbidden" assignment operator. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the assignment operator.
       *
       * @param obj Reference to an object of this class.
       * @return Reference to this instance.
       */
      MappingNew& operator=(const MappingNew &/*obj*/);

      float feedbackLinearizationController();
			float feedbackLinearizationController2();

    public:
      /**
       * Constructor.
       *
       * @param argc Number of command line arguments.
       * @param argv Command line arguments.
       */
      MappingNew(const int32_t &argc, char **argv);

      virtual ~MappingNew();

      core::base::ModuleState::MODULE_EXITCODE body();

    private:
      virtual void setUp();
      virtual void tearDown();

      bool m_hasReceivedLaneDetectionData;

     
  };

} // carolocup

#endif /*DRIVER_H_*/

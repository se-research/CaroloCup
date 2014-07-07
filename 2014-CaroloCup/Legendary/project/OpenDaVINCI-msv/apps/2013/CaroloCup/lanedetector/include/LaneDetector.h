/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef LANEDETECTOR_H_
#define LANEDETECTOR_H_

#include "opencv2/opencv.hpp"
#include "core/SharedPointer.h"
#include "core/base/ConferenceClientModule.h"
#include "core/wrapper/SharedMemory.h"
#include <LineDetector.h>

namespace carolocup {

using namespace std;

/**
 * This class is an exemplary skeleton for processing video data.
 */
class LaneDetector: public core::base::ConferenceClientModule {
private:
	/**
	 * "Forbidden" copy constructor. Goal: The compiler should warn
	 * already at compile time for unwanted bugs caused by any misuse
	 * of the copy constructor.
	 *
	 * @param obj Reference to an object of this class.
	 */
	LaneDetector(const LaneDetector &/*obj*/);

	/**
	 * "Forbidden" assignment operator. Goal: The compiler should warn
	 * already at compile time for unwanted bugs caused by any misuse
	 * of the assignment operator.
	 *
	 * @param obj Reference to an object of this class.
	 * @return Reference to this instance.
	 */
	LaneDetector& operator=(const LaneDetector &/*obj*/);

public:
	/**
	 * Constructor.
	 *
	 * @param argc Number of command line arguments.
	 * @param argv Command line arguments.
	 */
	LaneDetector(const int32_t &argc, char **argv);

	virtual ~LaneDetector();

	core::base::ModuleState::MODULE_EXITCODE body();

protected:
	/**
	 * This method is called to process an incoming container.
	 *
	 * @param c Container to process.
	 * @return true if c was successfully processed.
	 */
	bool readSharedImage(core::data::Container &c);

private:
  bool m_hasAttachedToSharedImageMemory;
  core::SharedPointer<core::wrapper::SharedMemory> m_sharedImageMemory;
  IplImage *m_image;
  IplImage *img;
  uint32_t m_cameraId;
  int init;
  bool m_debug;
  Config m_config;
  Mat m_frame;


///////////////////////////////////////7
//void *PrintHello(void *threadid);
////////////////////////////////////////

  virtual void setUp();

  virtual void tearDown();

  void processImage();
  bool init_camera();
  bool deinit_camera();
  bool get_image(char*& img);
};

} // carolocup
#endif /*LANEDETECTOR_H_*/

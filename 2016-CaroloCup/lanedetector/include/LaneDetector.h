/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef LANEDETECTOR_H_
#define LANEDETECTOR_H_

#include <memory>
#include <opencv/cv.h>
#include "opendavinci/odcore/base/module/TimeTriggeredConferenceClientModule.h"
#include "opendavinci/odcore/wrapper/SharedMemory.h"

#include <LineDetector.h>

namespace msv
{

using namespace std;

/**
 * This class is an exemplary skeleton for processing video data.
 */
class LaneDetector: public odcore::base::module::TimeTriggeredConferenceClientModule
{
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
    LaneDetector &operator=(const LaneDetector &/*obj*/);

public:
    /**
     * Constructor.
     *
     * @param argc Number of command line arguments.
     * @param argv Command line arguments.
     */
    LaneDetector(const int32_t &argc, char **argv);

    virtual ~LaneDetector();

    odcore::data::dmcp::ModuleExitCodeMessage::ModuleExitCode body();

protected:
    /**
     * This method is called to process an incoming container.
     *
     * @param c Container to process.
     * @return true if c was successfully processed.
     */
    bool readSharedImage(odcore::data::Container &c);

private:
    bool m_hasAttachedToSharedImageMemory;
    shared_ptr<odcore::wrapper::SharedMemory> m_sharedImageMemory;
    uint32_t m_cameraId;
    bool m_debug;
    Config m_config;
    Mat m_frame;
    uint32_t m_frame_count;

	void showResult(LineDetector &road, Mat &f);
	void showResult_createTrajectory(FinalOutput *res, LineDetector &road, Mat &f);
	void showResult_finalFilter(IntermediateResult *res, LineDetector &road, Mat &f);
	void print_lines(IntermediateResult *res, Mat &f);
	int getDynamicThresh(int lux);

    virtual void setUp();

    virtual void tearDown();

    void processImage();

    void showResult_getRectangles(vector<RotatedRect> res, Mat &f);
};

} // msv

#endif /*LANEDETECTOR_H_*/

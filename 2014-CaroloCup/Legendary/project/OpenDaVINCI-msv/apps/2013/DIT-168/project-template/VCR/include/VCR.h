/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef VCR_H_
#define VCR_H_

#include <opencv/cv.h>
#include <opencv/highgui.h>

#include "core/SharedPointer.h"
#include "core/base/ConferenceClientModule.h"
#include "core/wrapper/SharedMemory.h"

namespace msv {

    using namespace std;

    /**
     * This class records the live stream.
     */
    class VCR: public core::base::ConferenceClientModule {
        private:
	        /**
	         * "Forbidden" copy constructor. Goal: The compiler should warn
	         * already at compile time for unwanted bugs caused by any misuse
	         * of the copy constructor.
	         *
	         * @param obj Reference to an object of this class.
	         */
	        VCR(const VCR &/*obj*/);

	        /**
	         * "Forbidden" assignment operator. Goal: The compiler should warn
	         * already at compile time for unwanted bugs caused by any misuse
	         * of the assignment operator.
	         *
	         * @param obj Reference to an object of this class.
	         * @return Reference to this instance.
	         */
	        VCR& operator=(const VCR &/*obj*/);

        public:
	        /**
	         * Constructor.
	         *
	         * @param argc Number of command line arguments.
	         * @param argv Command line arguments.
	         */
	        VCR(const int32_t &argc, char **argv);

	        virtual ~VCR();

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
            uint32_t m_cameraId;
            bool m_debug;
            CvVideoWriter *m_writer;

	        virtual void setUp();

	        virtual void tearDown();

            void processImage();
    };

} // msv

#endif /*VCR_H_*/

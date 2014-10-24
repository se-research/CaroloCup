/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */
#ifndef CSVEXPORTER_H_
#define CSVEXPORTER_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/FIFOQueue.h"

namespace msv{

    class CsvExporter:public core::base::ConferenceClientModule{
    private:
	        /**
	         * "Forbidden" copy constructor. Goal: The compiler should warn
	         * already at compile time for unwanted bugs caused by any misuse
	         * of the copy constructor.
	         *
	         * @param obj Reference to an object of this class.
	         */
    	CsvExporter(const CsvExporter &/*obj*/);

	        /**
	         * "Forbidden" assignment operator. Goal: The compiler should warn
	         * already at compile time for unwanted bugs caused by any misuse
	         * of the assignment operator.
	         *
	         * @param obj Reference to an object of this class.
	         * @return Reference to this instance.
	         */
    	CsvExporter& operator=(const CsvExporter &/*obj*/);

        public:
	        /**
	         * Constructor.
	         *
	         * @param argc Number of command line arguments.
	         * @param argv Command line arguments.
	         */
    		CsvExporter(const int32_t &argc, char **argv);

	        virtual ~CsvExporter();

	        core::base::ModuleState::MODULE_EXITCODE body();


        private:
	        uint32_t frame_count;
            bool m_debug;
            core::base::FIFOQueue m_fifo;
            std::ostream *m_out;

	        virtual void setUp();

	        virtual void tearDown();



    };

}
#endif

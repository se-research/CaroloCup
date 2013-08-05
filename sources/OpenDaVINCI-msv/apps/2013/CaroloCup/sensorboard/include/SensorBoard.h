/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SENSORBOARD_H_
#define SENSORBOARD_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/FIFOQueue.h"
#include "core/data/Container.h"

namespace carolocup {

    using namespace std;

    /**
     * This class is an exemplary skeleton for processing sensor board data
     */
    class SensorBoard : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            SensorBoard(const SensorBoard &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            SensorBoard& operator=(const SensorBoard &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            SensorBoard(const int32_t &argc, char **argv);

            virtual ~SensorBoard();

            core::base::ModuleState::MODULE_EXITCODE body();

        protected:
            /**
             * This method is called to process an incoming container.
             *
             * @param c Container to process.
             * @return true if c was successfully processed.
             */
            bool processContainerExample(core::data::Container &c);

        private:
            virtual void setUp();

            virtual void tearDown();

            core::base::FIFOQueue m_fifo;
    };

} // carolocup

#endif /*SENSORBOARD_H_*/

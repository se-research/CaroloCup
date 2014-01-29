/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SENSORS_H_
#define SENSORS_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/FIFOQueue.h"
#include "core/data/Container.h"


namespace carolocup {

    using namespace std;

    class Sensors : public core::base::ConferenceClientModule {

        private:
           
            Sensors(const Sensors &/*obj*/);

            Sensors& operator=(const Sensors &/*obj*/);

        public:
       
            Sensors(const int32_t &argc, char **argv);

            virtual ~Sensors();

            core::base::ModuleState::MODULE_EXITCODE body();
//void  * function1(void * argument);
//void  * function2(void * argument);

     int converter(char* arrayInput);


        protected:
       

        private:
            virtual void setUp();

            virtual void tearDown();

            core::base::FIFOQueue m_fifo;

      int firstInfraredDistance;
      int secondInfraredDistance;
      int thirdInfraredDistance;
      int fourthInfraredDistance;

      int firstUltrasonicDistance;
      int secondUltrasonicDistance;
    };

} // carolocup

#endif /*SENSORS_H_*/

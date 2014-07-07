/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef LIDAR_H_
#define LIDAR_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/FIFOQueue.h"
#include "core/data/Container.h"

typedef struct{

  unsigned int readingIndex;
  unsigned int degree;
  unsigned int distance;
}reading;

namespace carolocup {


    using namespace std;

    class Lidar : public core::base::ConferenceClientModule {

        private:
           
            Lidar(const Lidar &/*obj*/);

            Lidar& operator=(const Lidar &/*obj*/);

        public:
       
            Lidar(const int32_t &argc, char **argv);
void getDistances(unsigned int id, unsigned int ra1, unsigned int ra2, unsigned int rb1, unsigned int rb2, unsigned int rc1, unsigned int rc2, unsigned int rd1, unsigned int rd2);

bool validate_buffer(void);


            virtual ~Lidar();

            core::base::ModuleState::MODULE_EXITCODE body();

        protected:
       

        private:
            virtual void setUp();

            virtual void tearDown();

            core::base::FIFOQueue m_fifo;
    };

} // carolocup

#endif /*LIDAR_H_*/

/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#ifndef SLL_H_
#define SLL_H_

#include <map>
#include <vector>

#include "core/base/ConferenceClientModule.h"
#include "hesperia/data/environment/Polygon.h"

#include "TSDPointSensor.h"

namespace sll {

    using namespace std;

    /**
     * This class can be used to produce some objects detected by
     * a 360 degree point distance sensor.
     */
    class SLL : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            SLL(const SLL &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            SLL& operator=(const SLL &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            SLL(const int32_t &argc, char **argv);

            virtual ~SLL();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();

            virtual void tearDown();

        private:
            uint32_t m_numberOfPolygons;
            map<uint32_t, hesperia::data::environment::Polygon> m_mapOfPolygons;
            TSDPointSensor* m_sensor;
            map<uint32_t, double> m_distancesPerBeam;
    };

} // sll

#endif /*SLL_H_*/

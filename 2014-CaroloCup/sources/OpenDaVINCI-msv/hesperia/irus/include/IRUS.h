/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#ifndef IRUS_H_
#define IRUS_H_

#include <map>
#include <vector>

#include "core/base/ConferenceClientModule.h"
#include "hesperia/data/environment/Polygon.h"

#include "PointSensor.h"

namespace irus {

    using namespace std;

    /**
     * This class can be used to produce some objects detected by
     * point providing sensors.
     */
    class IRUS : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            IRUS(const IRUS &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            IRUS& operator=(const IRUS &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            IRUS(const int32_t &argc, char **argv);

            virtual ~IRUS();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();

            virtual void tearDown();

        private:
            uint32_t m_numberOfPolygons;
            map<uint32_t, hesperia::data::environment::Polygon> m_mapOfPolygons;
            vector<uint32_t> m_listOfPolygonsInsideFOV;
            map<string, PointSensor*> m_mapOfPointSensors;
            map<string, double> m_distances;
            map<string, hesperia::data::environment::Polygon> m_FOVs;
    };

} // irus

#endif /*IRUS_H_*/

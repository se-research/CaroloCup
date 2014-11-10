/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef VEHICLECONTEXT_MODEL_IRUS_H_
#define VEHICLECONTEXT_MODEL_IRUS_H_

#include <string>

#include "core/base/KeyValueConfiguration.h"
#include "core/data/TimeStamp.h"

#include "context/base/SystemFeedbackComponent.h"

#include "hesperia/data/environment/EgoState.h"
#include "hesperia/data/environment/Polygon.h"

#include "vehiclecontext/model/PointSensor.h"

namespace vehiclecontext {
    namespace model {

        using namespace std;

        /**
         * This class realizes the model for IRUS.
         */
        class OPENDAVINCI_API IRUS : public context::base::SystemFeedbackComponent {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                IRUS(const IRUS&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                IRUS& operator=(const IRUS&);

            public:
                /**
                 * Constructor to create a IRUS.
                 *
                 * @param configuration Configuration data.
                 */
                IRUS(const string &configuration);

                /**
                 * Constructor to create a IRUS. This constructor overrides
                 * any specified frequency in the configuration.
                 *
                 * @param freq Desired runtime frequency.
                 * @param configuration Configuration data.
                 */
                IRUS(const float &freq, const string &configuration);

                virtual ~IRUS();

                virtual float getFrequency() const;

                virtual void setup();

                virtual void tearDown();

                virtual void step(const core::wrapper::Time &t, context::base::SendContainerToSystemsUnderTest &sender);

            private:
                core::base::KeyValueConfiguration m_kvc;
                float m_freq;

                hesperia::data::environment::EgoState m_egoState;

                uint32_t m_numberOfPolygons;
                map<uint32_t, hesperia::data::environment::Polygon> m_mapOfPolygons;
                vector<uint32_t> m_listOfPolygonsInsideFOV;
                map<string, PointSensor*> m_mapOfPointSensors;
                map<string, double> m_distances;
                map<string, hesperia::data::environment::Polygon> m_FOVs;
        };

    }
} // vehiclecontext::model

#endif /*VEHICLECONTEXT_MODEL_SIMPLIFIEDBICYCLEMODEL_H_*/

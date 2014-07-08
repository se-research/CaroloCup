/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef VEHICLECONTEXT_REPORT_DISTANCETOOBJECTS_H_
#define VEHICLECONTEXT_REPORT_DISTANCETOOBJECTS_H_

#include <list>
#include <string>

#include "core/base/KeyValueConfiguration.h"
#include "core/data/environment/Point3.h"
#include "hesperia/data/scenario/PointID.h"

#include "context/base/SystemReportingComponent.h"

namespace vehiclecontext {
    namespace report {

        using namespace std;

        /**
         * This class reports whether the distance to any object is less than a given threshold.
         */
        class OPENDAVINCI_API DistanceToObjectsReport : public context::base::SystemReportingComponent {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
				DistanceToObjectsReport(const DistanceToObjectsReport&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
				DistanceToObjectsReport& operator=(const DistanceToObjectsReport&);

            public:
                /**
                 * Constructor.
                 *
                 * @param configuration Configuration data.
                 * @param threshold Threshold for the distance.
                 */
				DistanceToObjectsReport(const string &configuration, const float &threshold);

                virtual ~DistanceToObjectsReport();

                virtual void setup();

                virtual void tearDown();

                virtual void report(const core::wrapper::Time &t);

                /**
                 * This method returns true, if the distance to the objects was greater than the given threshold.
                 *
                 * @return True, when the distance to all encountered objects was always greater than the given threshold.
                 */
                bool hasCorrectDistance() const;

            private:
                core::base::KeyValueConfiguration m_configuration;
                const float m_threshold;
                bool m_correctDistance;
        };

    }
} // vehiclecontext::report

#endif /*VEHICLECONTEXT_REPORT_DISTANCETOOBJECTS_H_*/

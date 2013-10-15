/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_TRAFFICLIGHT_H_
#define HESPERIA_CORE_DATA_SCENARIO_TRAFFICLIGHT_H_

#include <string>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "hesperia/data/scenario/TrafficControl.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents a traffic light.
             */
            class OPENDAVINCI_API TrafficLight : public TrafficControl {
                public:
                    TrafficLight();

                    virtual ~TrafficLight();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    TrafficLight(const TrafficLight &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    TrafficLight& operator=(const TrafficLight &obj);

                    virtual void accept(scenario::ScenarioVisitor &visitor);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_TRAFFICLIGHT_H_*/

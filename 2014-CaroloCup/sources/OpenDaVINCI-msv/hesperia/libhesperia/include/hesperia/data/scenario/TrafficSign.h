/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_TRAFFICSIGN_H_
#define HESPERIA_CORE_DATA_SCENARIO_TRAFFICSIGN_H_

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
             * This class represents a traffic sign.
             */
            class OPENDAVINCI_API TrafficSign : public TrafficControl {
                public:
                    TrafficSign();

                    virtual ~TrafficSign();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    TrafficSign(const TrafficSign &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    TrafficSign& operator=(const TrafficSign &obj);

                    virtual void accept(scenario::ScenarioVisitor &visitor);

                    /**
                     * This method returns the value of this traffic sign.
                     *
                     * @return Value of this traffic sign.
                     */
                    const string& getValue() const;

                    /**
                     * This method sets the value of this traffic sign.
                     *
                     * @param v Value of this traffic sign.
                     */
                    void setValue(const string &v);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    string m_value;
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_TRAFFICSIGN_H_*/

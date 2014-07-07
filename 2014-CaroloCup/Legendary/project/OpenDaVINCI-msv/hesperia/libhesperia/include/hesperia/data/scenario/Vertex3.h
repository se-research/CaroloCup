/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_VERTEX3_H_
#define HESPERIA_CORE_DATA_SCENARIO_VERTEX3_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"
#include "hesperia/data/scenario/ScenarioNode.h"
#include "hesperia/data/scenario/ScenarioVisitor.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents visitable three dimensional coordinates.
             */
            class OPENDAVINCI_API Vertex3 : public core::data::environment::Point3, public ScenarioNode {
                public:
                    Vertex3();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Vertex3(const Vertex3 &obj);

                    virtual ~Vertex3();

                    virtual void accept(scenario::ScenarioVisitor &visitor);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Vertex3& operator=(const Vertex3 &obj);
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_VERTEX3_H_*/

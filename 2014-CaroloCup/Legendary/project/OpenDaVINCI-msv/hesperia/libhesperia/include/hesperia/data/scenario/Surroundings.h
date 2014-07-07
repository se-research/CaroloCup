/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_SURROUNDINGS_H_
#define HESPERIA_CORE_DATA_SCENARIO_SURROUNDINGS_H_

#include <string>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "hesperia/data/scenario/ScenarioNode.h"
#include "hesperia/data/scenario/ScenarioVisitor.h"
#include "hesperia/data/scenario/Shape.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents the surroundings.
             */
            class OPENDAVINCI_API Surroundings : public core::data::SerializableData, public ScenarioNode {
                public:
                    Surroundings();

                    virtual ~Surroundings();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Surroundings(const Surroundings &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Surroundings& operator=(const Surroundings &obj);

                    virtual void accept(scenario::ScenarioVisitor &visitor);

                    /**
                     * This method returns the list of shapes.
                     *
                     * @return List of shapes.
                     */
                    const vector<Shape*>& getListOfShapes() const;

                    /**
                     * This method adds a shape.
                     *
                     * @param s Shape to be added.
                     */
                    void addShape(Shape *s);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    vector<Shape*> m_listOfShapes;

                    /**
                     * This method creates a deep copy.
                     *
                     * @param obj Object of another instance of this class.
                     */
                    void deepCopy(const Surroundings &obj);

                    /**
                     * This method clean's up the data.
                     */
                    void cleanUp();
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_SURROUNDINGS_H_*/

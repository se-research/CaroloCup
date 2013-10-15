/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_STRAIGHTLINE_H_
#define HESPERIA_CORE_DATA_SCENARIO_STRAIGHTLINE_H_

#include <string>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "hesperia/data/scenario/IDVertex3.h"
#include "hesperia/data/scenario/LaneModel.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents a straight line.
             */
            class OPENDAVINCI_API StraightLine : public LaneModel {
                public:
                    StraightLine();

                    virtual ~StraightLine();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    StraightLine(const StraightLine &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    StraightLine& operator=(const StraightLine &obj);

                    virtual void accept(scenario::ScenarioVisitor &visitor);

                    /**
                     * This method returns the start point.
                     *
                     * @return Start point.
                     */
                    const IDVertex3& getStart() const;

                    /**
                     * This method sets the start point.
                     *
                     * @param s Start point to set.
                     */
                    void setStart(const IDVertex3 &s);

                    /**
                     * This method returns the end point.
                     *
                     * @return End point.
                     */
                    const IDVertex3& getEnd() const;

                    /**
                     * This method sets the end point.
                     *
                     * @param e End point to set.
                     */
                    void setEnd(const IDVertex3 &e);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    IDVertex3 m_start;
                    IDVertex3 m_end;
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_STRAIGHTLINE_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_LANE_H_
#define HESPERIA_CORE_DATA_SCENARIO_LANE_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "hesperia/data/scenario/LaneModel.h"
#include "hesperia/data/scenario/ScenarioNode.h"
#include "hesperia/data/scenario/ScenarioVisitor.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /** Prevent circular dependencies. */
            class Road;

            /**
             * This class represents a lane.
             */
            class OPENDAVINCI_API Lane : public core::data::SerializableData, public ScenarioNode {
                public:
                    Lane();

                    virtual ~Lane();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Lane(const Lane &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Lane& operator=(const Lane &obj);

                    virtual void accept(scenario::ScenarioVisitor &visitor);

                    /**
                     * This method returns the ID.
                     *
                     * @return ID.
                     */
                    uint32_t getID() const;

                    /**
                     * This method sets the ID.
                     *
                     * @param id ID to be set.
                     */
                    void setID(const uint32_t &id);

                    /**
                     * This method returns the lane model.
                     *
                     * @return Lane model.
                     */
                    LaneModel* getLaneModel() const;

                    /**
                     * This method sets the lane model.
                     *
                     * @param lm Lane model to be set.
                     */
                    void setLaneModel(LaneModel *lm);

                    /**
                     * This method sets the road to which this lane belongs to.
                     *
                     * @param r Road.
                     */
                    void setRoad(const Road *r);

                    /**
                     * This method returns the road to which this lane belongs to.
                     *
                     * @return Road.
                     */
                    const Road* getRoad() const;

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    uint32_t m_id;
                    const Road *m_road;
                    LaneModel *m_laneModel;

                    /**
                     * This method creates a deep copy.
                     *
                     * @param obj Object to be copy.
                     */
                    void deepCopy(const Lane &obj);
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_LANE_H_*/

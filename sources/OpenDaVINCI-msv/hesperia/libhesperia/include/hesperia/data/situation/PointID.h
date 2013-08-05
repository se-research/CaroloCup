/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_POINTID_H_
#define HESPERIA_CORE_DATA_SITUATION_POINTID_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "hesperia/data/situation/SituationNode.h"
#include "hesperia/data/situation/SituationVisitor.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;

            /**
             * This class represents a connector between lanes or lanes and zones.
             */
            class OPENDAVINCI_API PointID : public core::data::SerializableData, public SituationNode {
                public:
                    PointID();

                    /**
                     * Constructor to set values from pointID of format
                     * Layer "." Road "." Lane "." Point
                     *
                     * @param pointID PointID to parse from.
                     */
                    PointID(const string &pointID);

                    virtual ~PointID();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    PointID(const PointID &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    PointID& operator=(const PointID &obj);

                    virtual void accept(situation::SituationVisitor &visitor);

                    /**
                     * This method returns the layerID.
                     *
                     * @return Layer identifier.
                     */
                    uint32_t getLayerID() const;

                    /**
                     * This method sets the layer identifier.
                     *
                     * @param layerID Layer identifier.
                     */
                    void setLayerID(const uint32_t &layerID);

                    /**
                     * This method returns the roadID.
                     *
                     * @return Road identifier.
                     */
                    uint32_t getRoadID() const;

                    /**
                     * This method sets the road identifier.
                     *
                     * @param roadID Road identifier.
                     */
                    void setRoadID(const uint32_t &roadID);

                    /**
                     * This method returns the laneID.
                     *
                     * @return Lane identifier.
                     */
                    uint32_t getLaneID() const;

                    /**
                     * This method sets the lane identifier.
                     *
                     * @param laneID Lane identifier.
                     */
                    void setLaneID(const uint32_t &laneID);

                    /**
                     * This method returns the pointID.
                     *
                     * @return Point identifier.
                     */
                    uint32_t getPointID() const;

                    /**
                     * This method sets the point identifier.
                     *
                     * @param pointID Point identifier.
                     */
                    void setPointID(const uint32_t &pointID);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    uint32_t m_layerID;
                    uint32_t m_roadID;
                    uint32_t m_laneID;
                    uint32_t m_pointID;
            };

        }
    }
} // hesperia::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_POINTID_H_*/

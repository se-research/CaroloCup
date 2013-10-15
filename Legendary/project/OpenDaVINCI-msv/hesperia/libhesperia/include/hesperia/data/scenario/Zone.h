/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_ZONE_H_
#define HESPERIA_CORE_DATA_SCENARIO_ZONE_H_

#include <string>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "hesperia/data/scenario/Connector.h"
#include "hesperia/data/scenario/Lane.h"
#include "hesperia/data/scenario/Perimeter.h"
#include "hesperia/data/scenario/ScenarioNode.h"
#include "hesperia/data/scenario/ScenarioVisitor.h"
#include "hesperia/data/scenario/Spot.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /** Prevent circular dependencies. */
            class Layer;

            /**
             * This class represents a zone.
             */
            class OPENDAVINCI_API Zone : public core::data::SerializableData, public ScenarioNode {
                public:
                    Zone();

                    virtual ~Zone();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Zone(const Zone &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Zone& operator=(const Zone &obj);

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
                     * This method returns the road's name.
                     *
                     * @return Zone's name.
                     */
                    const string getName() const;

                    /**
                     * This method set the road's name.
                     *
                     * @param name The road's name.
                     */
                    void setName(const string &name);

                    /**
                     * This method returns the list of connectors.
                     *
                     * @return List of connectors.
                     */
                    const vector<Connector>& getListOfConnectors() const;

                    /**
                     * This method adds a connector.
                     *
                     * @param c Connector to be added.
                     */
                    void addConnector(const Connector &c);

                    /**
                     * This method returns the perimeter.
                     *
                     * @return Perimeter.
                     */
                    const Perimeter& getPerimeter() const;

                    /**
                     * This method sets the perimeter.
                     *
                     * @param p Perimeter to be set.
                     */
                    void setPerimeter(const Perimeter &p);

                    /**
                     * This method returns the list of spots.
                     *
                     * @return List of spots.
                     */
                    const vector<Spot>& getListOfSpots() const;

                    /**
                     * This method adds a spot.
                     *
                     * @param s Spot to be added.
                     */
                    void addSpot(const Spot &s);

                    /**
                     * This method sets the layer to which this zone belongs to.
                     *
                     * @param l Layer.
                     */
                    void setLayer(const Layer *l);

                    /**
                     * This method returns the layer to which this zone belongs to.
                     *
                     * @return Layer.
                     */
                    const Layer* getLayer() const;

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    uint32_t m_id;
                    string m_name;
                    const Layer *m_layer;
                    vector<Connector> m_listOfConnectors;
                    Perimeter m_perimeter;
                    vector<Spot> m_listOfSpots;

                    /**
                     * This method creates a deep copy.
                     *
                     * @param obj Object of another instance of this class.
                     */
                    void deepCopy(const Zone &obj);
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_ZONE_H_*/

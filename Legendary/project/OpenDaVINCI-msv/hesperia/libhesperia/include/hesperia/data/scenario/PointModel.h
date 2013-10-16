/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_POINTMODEL_H_
#define HESPERIA_CORE_DATA_SCENARIO_POINTMODEL_H_

#include <string>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "core/exceptions/Exceptions.h"
#include "hesperia/data/scenario/IDVertex3.h"
#include "hesperia/data/scenario/LaneModel.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents a point model.
             */
            class OPENDAVINCI_API PointModel : public LaneModel {
                public:
                    PointModel();

                    virtual ~PointModel();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    PointModel(const PointModel &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    PointModel& operator=(const PointModel &obj);

                    virtual void accept(scenario::ScenarioVisitor &visitor);

                    /**
                     * This method returns the list of identifiable vertices.
                     *
                     * @return List of identifiable vertices.
                     */
                    const vector<IDVertex3>& getListOfIdentifiableVertices() const;

                    /**
                     * This method returns IDVertex3 for the given ID.
                     *
                     * @param id ID to look up.
                     * @return IDVertex3 for the given ID.
                     * @throws InvalidArgumentException if the ID could not be found.
                     */
                    const IDVertex3 getIDVertex3(const uint32_t &id) const throw (core::exceptions::InvalidArgumentException);

                    /**
                     * This method adds an identifiable vertex.
                     *
                     * @param idV Identifiable vertex to be added.
                     */
                    void addIdentifiableVertex(const IDVertex3 &idV);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    vector<IDVertex3> m_listOfIdentifiableVertices;

                    /**
                     * This method creates a deep copy.
                     *
                     * @param obj Object of another instance of this class.
                     */
                    void deepCopy(const PointModel &obj);
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_POINTMODEL_H_*/

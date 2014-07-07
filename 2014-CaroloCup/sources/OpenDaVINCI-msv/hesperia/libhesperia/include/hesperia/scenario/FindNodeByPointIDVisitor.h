/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_SCENARIO_FINDNODEBYPOINTIDVISITOR_H_
#define HESPERIA_SCENARIO_FINDNODEBYPOINTIDVISITOR_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"

#include "hesperia/data/scenario/ScenarioVisitor.h"
#include "hesperia/data/situation/PointID.h"
#include "hesperia/data/scenario/PointID.h"
#include "hesperia/data/scenario/IDVertex3.h"
#include "hesperia/data/scenario/LaneModel.h"

namespace hesperia {
    namespace scenario {

        using namespace std;

        /**
         * This class finds a node by a given PointID.
         */
        class OPENDAVINCI_API FindNodeByPointIDVisitor : public data::scenario::ScenarioVisitor {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                FindNodeByPointIDVisitor(const FindNodeByPointIDVisitor &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                FindNodeByPointIDVisitor& operator=(const FindNodeByPointIDVisitor &);

            public:
                /**
                 * Constructor.
                 *
                 * @param pID PointID to look for.
                 */
                FindNodeByPointIDVisitor(const data::scenario::PointID &pID);

                /**
                 * Constructor.
                 *
                 * @param pID PointID to look for.
                 */
                FindNodeByPointIDVisitor(const data::situation::PointID &pID);

                virtual ~FindNodeByPointIDVisitor();

                virtual void visit(data::scenario::ScenarioNode &node);

                /**
                 * This method returns the LaneModel, if one could be found.
                 *
                 * @return LaneModel or NULL.
                 */
                const data::scenario::LaneModel* getLaneModel() const;

                /**
                 * This method returns the IDVertex3.
                 *
                 * @return IDVertex3.
                 */
                const data::scenario::IDVertex3 getIDVertex3() const;

            private:
                const data::scenario::PointID m_pointID;
                data::scenario::LaneModel *m_laneModel;
                data::scenario::IDVertex3 m_vertex;
        };

    }
} // hesperia::scenario

#endif /*HESPERIA_SCENARIO_FINDNODEBYPOINTIDVISITOR_H_*/

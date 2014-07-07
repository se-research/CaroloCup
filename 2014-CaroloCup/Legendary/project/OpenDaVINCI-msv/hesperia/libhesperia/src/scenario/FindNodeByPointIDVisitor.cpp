/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <vector>

#include "hesperia/data/scenario/IDVertex3.h"
#include "hesperia/data/scenario/Lane.h"
#include "hesperia/data/scenario/Layer.h"
#include "hesperia/data/scenario/Road.h"

#include "hesperia/data/scenario/Arc.h"
#include "hesperia/data/scenario/Clothoid.h"
#include "hesperia/data/scenario/PointModel.h"
#include "hesperia/data/scenario/StraightLine.h"

#include "hesperia/scenario/FindNodeByPointIDVisitor.h"

namespace hesperia {
    namespace scenario {

        using namespace std;
        using namespace hesperia::data::scenario;
        using namespace hesperia::scenario;

        FindNodeByPointIDVisitor::FindNodeByPointIDVisitor(const PointID &pID) :
            m_pointID(pID),
            m_laneModel(NULL),
			m_vertex() {}

        FindNodeByPointIDVisitor::FindNodeByPointIDVisitor(const hesperia::data::situation::PointID &pID) :
            m_pointID(pID.toString()),
            m_laneModel(NULL),
			m_vertex() {}

        FindNodeByPointIDVisitor::~FindNodeByPointIDVisitor() {}

        void FindNodeByPointIDVisitor::visit(ScenarioNode &node) {
            try {
                Lane &lane = dynamic_cast<Lane&>(node);

                if ( (lane.getLaneModel() != NULL) && (lane.getRoad() != NULL) && (lane.getRoad()->getLayer() != NULL) ) {
                    if (    (lane.getID() == m_pointID.getLaneID())
                         && (lane.getRoad()->getID() == m_pointID.getRoadID())
                         && (lane.getRoad()->getLayer()->getID() == m_pointID.getLayerID()) ) {

                        // Checking for point in LaneModel.
                        try {
                            Arc *arc = dynamic_cast<Arc*>(lane.getLaneModel());
                            if (arc != NULL) {
                            	if (arc->getStart().getID() == m_pointID.getPointID()) {
                                    m_laneModel = arc;
                            		m_vertex = arc->getStart();
                            		return;
                            	}
                                if (arc->getEnd().getID() == m_pointID.getPointID()) {
                                    m_laneModel = arc;
                            		m_vertex = arc->getEnd();
                                    return;
                                }
                            }
                        }
                        catch(...) {}

                        try {
                            Clothoid *clothoid = dynamic_cast<Clothoid*>(lane.getLaneModel());
                            if (clothoid != NULL) {
                            	if (clothoid->getStart().getID() == m_pointID.getPointID()) {
                            		m_laneModel = clothoid;
                            		m_vertex = clothoid->getStart();
                            		return;
                            	}
                                if (clothoid->getEnd().getID() == m_pointID.getPointID()) {
                                    m_laneModel = clothoid;
                                    m_vertex = clothoid->getEnd();
                                    return;
                                }
                            }
                        }
                        catch(...) {}

                        try {
                            StraightLine *sl = dynamic_cast<StraightLine*>(lane.getLaneModel());
                            if (sl != NULL) {
                            	if (sl->getStart().getID() == m_pointID.getPointID()) {
                            		m_laneModel = sl;
                            		m_vertex = sl->getStart();
                            		return;
                            	}
                                if (sl->getEnd().getID() == m_pointID.getPointID()) {
                                    m_laneModel = sl;
                                    m_vertex = sl->getEnd();
                                    return;
                                }
                            }
                        }
                        catch(...) {}

                        try {
                            PointModel *pm = dynamic_cast<PointModel*>(lane.getLaneModel());
                            if (pm != NULL) {
                                const vector<IDVertex3> &listOfVertices = pm->getListOfIdentifiableVertices();
                                vector<IDVertex3>::const_iterator it = listOfVertices.begin();
                                while (it != listOfVertices.end()) {
                                    if ((*it).getID() == m_pointID.getPointID()) {
                                    	m_vertex = (*it);
                                        m_laneModel = pm;
                                        break;
                                    }
                                    it++;
                                }
                                return;
                            }
                        }
                        catch(...) {}
                    }
                }
            }
            catch(...)
            {}
        }

        const IDVertex3 FindNodeByPointIDVisitor::getIDVertex3() const {
        	return m_vertex;
        }

        const LaneModel* FindNodeByPointIDVisitor::getLaneModel() const {
            return m_laneModel;
        }

    }
} // hesperia::scenario

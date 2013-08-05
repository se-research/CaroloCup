/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <sstream>

#include "hesperia/data/graph/WaypointsEdge.h"

namespace hesperia {
    namespace data {
        namespace graph {

            using namespace std;

            WaypointsEdge::WaypointsEdge() :
                m_costs(0) {}

            WaypointsEdge::~WaypointsEdge() {}

            void WaypointsEdge::setCosts(const double &c) {
                m_costs = c;
            }

            const string WaypointsEdge::toString() const {
                stringstream sstr;
                sstr << m_costs;
                return sstr.str();
            }

            double WaypointsEdge::getCosts() const {
                return m_costs;
            }

        }
    }
} // hesperia::data::graph

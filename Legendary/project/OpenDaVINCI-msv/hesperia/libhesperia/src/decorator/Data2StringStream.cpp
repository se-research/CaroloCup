/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/data/Container.h"

#include "hesperia/decorator/Data2StringStream.h"
#include "hesperia/data/environment/EgoState.h"
#include "hesperia/data/planning/Route.h"

namespace hesperia {
    namespace decorator {

        using namespace std;
        using namespace core::data;
        using namespace core::base;
        using namespace hesperia::data::environment;
        using namespace hesperia::data::planning;

        Data2StringStream::Data2StringStream(stringstream &sstr) :
            m_sstr(sstr) {}

        Data2StringStream::~Data2StringStream() {}

        void Data2StringStream::toStringStream(Container &c) {
            switch (c.getDataType()) {
                case Container::EGOSTATE:
                {
                    toStringStream(c.getData<EgoState>());
                    break;
                }

                case Container::ROUTE:
                {
                    toStringStream(c.getData<Route>());
                    break;
                }

                default:
                    break;
            }
        }

        void Data2StringStream::toStringStream(const EgoState &es) {
            m_sstr << es.toString();
        }

        void Data2StringStream::toStringStream(const Route &r) {
            m_sstr << r.toString();
        }

    }
} // hesperia::decorator

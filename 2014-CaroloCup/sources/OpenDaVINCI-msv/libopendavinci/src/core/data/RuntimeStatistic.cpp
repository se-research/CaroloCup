/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "core/data/RuntimeStatistic.h"

namespace core {
    namespace data {

        using namespace std;
        using namespace base;

        RuntimeStatistic::RuntimeStatistic() :
                m_sliceConsumption(0) {}

        RuntimeStatistic::RuntimeStatistic(const RuntimeStatistic &obj) :
                SerializableData(),
                m_sliceConsumption(obj.getSliceConsumption()) {}

        RuntimeStatistic::~RuntimeStatistic() {}

        RuntimeStatistic& RuntimeStatistic::operator=(const RuntimeStatistic &obj) {
            setSliceConsumption(obj.getSliceConsumption());
            return (*this);
        }

        float RuntimeStatistic::getSliceConsumption() const {
            return m_sliceConsumption;
        }

        void RuntimeStatistic::setSliceConsumption(const float &sc) {
            m_sliceConsumption = sc;
        }

        const string RuntimeStatistic::toString() const {
            stringstream s;
            s << getSliceConsumption() << "%";
            return s.str();
        }

        ostream& RuntimeStatistic::operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);

            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('s', 'c') >::RESULT,
                    getSliceConsumption());

            return out;
        }

        istream& RuntimeStatistic::operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);

            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('s', 'c') >::RESULT,
                   m_sliceConsumption);

            return in;
        }

    }
} // core::data

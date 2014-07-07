/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <sstream>

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "core/data/environment/Point3.h"

#include "hesperia/data/camera/ExtrinsicParameters.h"

namespace hesperia {
    namespace data {
        namespace camera {

            using namespace std;
            using namespace core::base;
            using namespace core::data;
            using namespace core::data::environment;

            ExtrinsicParameters::ExtrinsicParameters() :
                    m_translation(),
                    m_rotation() {}

            ExtrinsicParameters::ExtrinsicParameters(const Point3 &translation, const Matrix3x3 &rotation) :
                    m_translation(translation),
                    m_rotation(rotation) {}

            ExtrinsicParameters::ExtrinsicParameters(const ExtrinsicParameters &obj) :
                    m_translation(obj.getTranslation()),
                    m_rotation(obj.getRotation()) {}

            ExtrinsicParameters::~ExtrinsicParameters() {}

            ExtrinsicParameters& ExtrinsicParameters::operator=(const ExtrinsicParameters &obj) {
                m_translation = obj.getTranslation();
                m_rotation = obj.getRotation();

                return (*this);
            }

            const Point3 ExtrinsicParameters::getTranslation() const {
                return m_translation;
            }

            void ExtrinsicParameters::setTranslation(const Point3 &translation) {
                m_translation = translation;
            }

            const Matrix3x3 ExtrinsicParameters::getRotation() const {
                return m_rotation;
            }

            void ExtrinsicParameters::setRotation(const environment::Matrix3x3 &rotation) {
                m_rotation = rotation;
            }

            ostream& ExtrinsicParameters::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('t', 'r', 'a', 'n', 's') >::RESULT,
                        m_translation);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('r', 'o', 't') >::RESULT,
                        m_rotation);

                return out;
            }

            istream& ExtrinsicParameters::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('t', 'r', 'a', 'n', 's') >::RESULT,
                       m_translation);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('r', 'o', 't') >::RESULT,
                       m_rotation);

                return in;
            }

            const string ExtrinsicParameters::toString() const {
                stringstream sstr;
                sstr << "T=" << m_translation.toString() << "/R=" << m_rotation.toString();
                return sstr.str();
            }

        }
    }
} // core::data::camera

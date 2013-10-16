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

#include "hesperia/data/camera/IntrinsicParameters.h"

namespace hesperia {
    namespace data {
        namespace camera {

            using namespace std;

            using namespace core::base;
            using namespace core::data::environment;

            IntrinsicParameters::IntrinsicParameters() :
                    m_principlePoint(),
                    m_focalLength(),
                    m_focalDistance(0),
                    m_alpha(0) {}

            IntrinsicParameters::IntrinsicParameters(const IntrinsicParameters &obj) :
                    m_principlePoint(obj.getPrinciplePoint()),
                    m_focalLength(obj.getFocalLength()),
                    m_focalDistance(obj.getFocalDistance()),
                    m_alpha(obj.getAlpha()) {}

            IntrinsicParameters::~IntrinsicParameters() {}

            IntrinsicParameters& IntrinsicParameters::operator=(const IntrinsicParameters &obj) {
                setPrinciplePoint(obj.getPrinciplePoint());
                setFocalLength(obj.getFocalLength());
                setFocalDistance(obj.getFocalDistance());
                setAlpha(obj.getAlpha());

                return (*this);
            }

            const Point3 IntrinsicParameters::getPrinciplePoint() const {
                return m_principlePoint;
            }

            void IntrinsicParameters::setPrinciplePoint(const Point3 &principlePoint) {
                m_principlePoint = principlePoint;

                // Enforce two-dimensional point by setting z-component to 0.
                m_principlePoint.setZ(0);
            }

            const Point3 IntrinsicParameters::getFocalLength() const {
                return m_focalLength;
            }

            void IntrinsicParameters::setFocalLength(const Point3 &focalLength) {
                m_focalLength = focalLength;

                // Enforce two-dimensional point by setting z-component to 0.
                m_focalLength.setZ(0);
            }

            double IntrinsicParameters::getFocalDistance() const {
                return m_focalDistance;
            }

            void IntrinsicParameters::setFocalDistance(const double &focalDistance) {
                m_focalDistance = focalDistance;
            }

            double IntrinsicParameters::getAlpha() const {
                return m_alpha;
            }

            void IntrinsicParameters::setAlpha(const double &alpha) {
                m_alpha = alpha;
            }

            ostream& IntrinsicParameters::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('p', 'r', 'i', 'n', 'c', 'i', 'p', 'l') >::RESULT,
                        m_principlePoint);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('f', 'o', 'c', 'a', 'l', 'l', 'e', 'n') >::RESULT,
                        m_focalLength);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('f', 'o', 'c', 'd', 'i', 's', 't') >::RESULT,
                        m_focalDistance);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('a', 'l', 'p', 'h', 'a') >::RESULT,
                        m_alpha);

                return out;
            }

            istream& IntrinsicParameters::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('p', 'r', 'i', 'n', 'c', 'i', 'p', 'l') >::RESULT,
                       m_principlePoint);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('f', 'o', 'c', 'a', 'l', 'l', 'e', 'n') >::RESULT,
                       m_focalLength);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('f', 'o', 'c', 'd', 'i', 's', 't') >::RESULT,
                       m_focalDistance);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('a', 'l', 'p', 'h', 'a') >::RESULT,
                       m_alpha);

                return in;
            }

            const string IntrinsicParameters::toString() const {
                stringstream sstr;
                sstr << "principle point = " << m_principlePoint.toString() << ", focal length = " << m_focalLength.toString() << ", focal distance = " << m_focalDistance << ", alpha = " << m_alpha;
                return sstr.str();
            }

        }
    }
} // core::data::camera

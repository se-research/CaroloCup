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

#include "hesperia/data/camera/ImageGrabberCalibration.h"

namespace hesperia {
    namespace data {
        namespace camera {

            using namespace std;
            using namespace core::data;
            using namespace core::base;

            ImageGrabberCalibration::ImageGrabberCalibration() :
                    m_extrinsicParameters(),
                    m_intrinsicParameters() {}

            ImageGrabberCalibration::ImageGrabberCalibration(const ImageGrabberCalibration &obj) :
                    m_extrinsicParameters(obj.getExtrinsicParameters()),
                    m_intrinsicParameters(obj.getIntrinsicParameters()) {}

            ImageGrabberCalibration::~ImageGrabberCalibration() {}

            ImageGrabberCalibration& ImageGrabberCalibration::operator=(const ImageGrabberCalibration &obj) {
                setExtrinsicParameters(obj.getExtrinsicParameters());
                setIntrinsicParameters(obj.getIntrinsicParameters());

                return (*this);
            }

            const ExtrinsicParameters ImageGrabberCalibration::getExtrinsicParameters() const {
                return m_extrinsicParameters;
            }

            void ImageGrabberCalibration::setExtrinsicParameters(const ExtrinsicParameters &extrinsicParameters) {
                m_extrinsicParameters = extrinsicParameters;
            }

            const IntrinsicParameters ImageGrabberCalibration::getIntrinsicParameters() const {
                return m_intrinsicParameters;
            }

            void ImageGrabberCalibration::setIntrinsicParameters(const IntrinsicParameters &intrinsicParameters) {
                m_intrinsicParameters = intrinsicParameters;
            }

            ostream& ImageGrabberCalibration::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('e', 'x', 't') >::RESULT,
                        m_extrinsicParameters);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('i', 'n', 't') >::RESULT,
                        m_intrinsicParameters);

                return out;
            }

            istream& ImageGrabberCalibration::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('e', 'x', 't') >::RESULT,
                       m_extrinsicParameters);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('i', 'n', 't') >::RESULT,
                       m_intrinsicParameters);

                return in;
            }

            const string ImageGrabberCalibration::toString() const {
                stringstream sstr;
                sstr << "Extrinsic: " << m_extrinsicParameters.toString() << "/Intrinsic: " << m_intrinsicParameters.toString();
                return sstr.str();
            }
        }
    }
} // hesperia::data::camera

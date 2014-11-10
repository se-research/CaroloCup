/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "hesperia/data/camera/ImageGrabberID.h"

namespace hesperia {
    namespace data {
        namespace camera {

            using namespace std;
            using namespace core::base;


            ImageGrabberID::ImageGrabberID(const string &name) :
                    m_name(name) {}

            ImageGrabberID::ImageGrabberID(const ImageGrabberID &obj) :
                    m_name(obj.m_name) {}

            ImageGrabberID::~ImageGrabberID() {}

            ImageGrabberID& ImageGrabberID::operator=(const ImageGrabberID &obj) {
                m_name = obj.getName();

                return (*this);
            }

            const string ImageGrabberID::getName() const {
                return m_name;
            }

            ostream& ImageGrabberID::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('n', 'a', 'm', 'e') >::RESULT,
                        m_name);

                return out;
            }

            istream& ImageGrabberID::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('n', 'a', 'm', 'e') >::RESULT,
                       m_name);

                return in;
            }

            const string ImageGrabberID::toString() const {
                return m_name;
            }
        }
    }
} // hesperia::data::camera

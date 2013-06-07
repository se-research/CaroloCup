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

#include "hesperia/data/dmcp/ModuleDescriptor.h"

namespace hesperia {
    namespace data {
        namespace dmcp {

            using namespace core::base;

            ModuleDescriptor::ModuleDescriptor() :
                    m_name(""),
                    m_identifier(""),
                    m_version("")
            {}

            ModuleDescriptor::ModuleDescriptor(const string& name,
                                               const string& identifier,
                                               const string& version ) :
                    m_name(name),
                    m_identifier(identifier),
                    m_version(version)
            {}

            ModuleDescriptor::ModuleDescriptor(const ModuleDescriptor &obj) :
                m_name(obj.getName()),
                m_identifier(obj.getIdentifier()),
                m_version(obj.getVersion())
            {}

            ModuleDescriptor::~ModuleDescriptor() {}

            ModuleDescriptor& ModuleDescriptor::operator=(const ModuleDescriptor &obj) {
                m_name = obj.getName();
                m_identifier = obj.getIdentifier();
                m_version = obj.getVersion();

                return (*this);
            }

            const string ModuleDescriptor::getName() const {
                return m_name;
            }

            const string ModuleDescriptor::getIdentifier() const {
                return m_identifier;
            }

            const string ModuleDescriptor::getVersion() const {
                return m_version;
            }

            const string ModuleDescriptor::toString() const {
                stringstream ss;
                ss << m_name << "[" << m_identifier << "]" << m_version;

                return ss.str();
            }

            ostream& ModuleDescriptor::operator<<(ostream &out) const {
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL4('n', 'a', 'm', 'e') >::RESULT,
                        m_name);

                s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL2('i', 'd') >::RESULT,
                        m_identifier);

                s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL7('v', 'e', 'r', 's', 'i', 'o', 'n') >::RESULT,
                        m_version);

                return out;
            }

            istream& ModuleDescriptor::operator>>(istream &in) {
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL4('n', 'a', 'm', 'e') >::RESULT,
                       m_name);

                d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL2('i', 'd') >::RESULT,
                       m_identifier);

                d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL7('v', 'e', 'r', 's', 'i', 'o', 'n') >::RESULT,
                       m_version);

                return in;
            }

        }
    }
} // hesperia::data::dmcp

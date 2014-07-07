/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */
#ifndef OPENDAVINCI_DATA_DMCP_MODULEDESCRIPTOR_H_
#define OPENDAVINCI_DATA_DMCP_MODULEDESCRIPTOR_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

namespace core {
    namespace data{
        namespace dmcp {

            using namespace std;

            class OPENDAVINCI_API ModuleDescriptor : public core::data::SerializableData {
                public:
                    ModuleDescriptor();

                    ModuleDescriptor(const string& name,
                                     const string& identifier,
                                     const string& version);
                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ModuleDescriptor(const ModuleDescriptor &obj);

                    virtual ~ModuleDescriptor();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ModuleDescriptor& operator=(const ModuleDescriptor &obj);

                    const string getName() const;
                    const string getIdentifier() const;
                    const string getVersion() const;

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                protected:
                    string m_name;
                    string m_identifier;
                    string m_version;
            };
        }
    }
} // core::data::dmcp

#endif /* OPENDAVINCI_DATA_DMCP_MODULEDESCRIPTOR_H_ */

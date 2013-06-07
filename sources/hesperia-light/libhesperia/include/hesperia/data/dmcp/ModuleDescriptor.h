/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */
#ifndef HESPERIA_DATA_DMCP_MODULEDESCRIPTOR_H_
#define HESPERIA_DATA_DMCP_MODULEDESCRIPTOR_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"

namespace hesperia {
    namespace data{
        namespace dmcp {

            using namespace std;

            class HESPERIA_API ModuleDescriptor : public core::data::SerializableData {
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
} // hesperia::data::dmcp

#endif /* HESPERIA_DATA_DMCP_MODULEDESCRIPTOR_H_ */

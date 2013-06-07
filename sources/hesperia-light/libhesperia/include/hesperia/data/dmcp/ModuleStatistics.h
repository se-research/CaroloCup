/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_DMCP_MODULESTATISTICS_H_
#define HESPERIA_DATA_DMCP_MODULESTATISTICS_H_

#include <map>
#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "core/data/RuntimeStatistic.h"
#include "hesperia/data/dmcp/ModuleDescriptorComparator.h"

namespace hesperia {
    namespace data {
        namespace dmcp {

            using namespace std;

            class HESPERIA_API ModuleStatistics : public core::data::SerializableData {
                public:
                    ModuleStatistics();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ModuleStatistics(const ModuleStatistics &obj);

                    virtual ~ModuleStatistics();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ModuleStatistics& operator=(const ModuleStatistics &obj);

                    /**
                     * This method returns module statistics.
                     *
                     * @return Module statistics.
                     */
                    map<ModuleDescriptor, core::data::RuntimeStatistic, ModuleDescriptorComparator> getRuntimeStatistic() const;

                    /**
                     * This method sets a module's statistic.
                     *
                     * @param md ModuleDescriptor.
                     * @param rts RuntimeStatistic.
                     */
                    void setRuntimeStatistic(const ModuleDescriptor &md, const core::data::RuntimeStatistic &rts);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    map<ModuleDescriptor, core::data::RuntimeStatistic, ModuleDescriptorComparator> m_moduleStatistics;
            };
        }
    }
} // hesperia::data::dmcp

#endif /*HESPERIA_DATA_DMCP_MODULESTATISTICS_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <sstream>

#include "core/io/ContainerConferenceFactory.h"

#include "vehiclecontext/VehicleRuntimeControl.h"

#include "vehiclecontext/model/SimplifiedBicycleModel.h"

namespace vehiclecontext {

    using namespace std;
    using namespace core::base;
    using namespace vehiclecontext::model;

    VehicleRuntimeControl::VehicleRuntimeControl(const context::base::RuntimeControlInterface &sci) :
        context::base::StandaloneRuntimeControl(sci),
        m_globalConfiguration() {
        m_globalConfiguration = sci.getConfiguration();
    }

    VehicleRuntimeControl::~VehicleRuntimeControl() {}

    const KeyValueConfiguration VehicleRuntimeControl::getConfigurationFor(const enum VEHICLECONTEXTMODULES &module) {
        KeyValueConfiguration config;

        switch (module) {
            case VehicleRuntimeControl::SIMPLIFIEDBICYCLEMODEL:
            {
                KeyValueConfiguration simplifiedBicycleModelSubset = m_globalConfiguration.getSubsetForSection("vehiclecontext.simplifiedbicyclemodel");
                if (simplifiedBicycleModelSubset.getListOfKeys().size() > 0) {
                    // Remove leading "vehiclecontext.".
                    config = simplifiedBicycleModelSubset.getSubsetForSectionRemoveLeadingSectionName("vehiclecontext.");
                }
            }
            break;
        }

        // Add global.*.
        KeyValueConfiguration globalConfig = m_globalConfiguration.getSubsetForSection("global");
        stringstream fusedConfig;
        fusedConfig << globalConfig << endl;
        fusedConfig << config;

        // Re-read previously written configuration.
        fusedConfig >> config;

        return config;
    }

    void VehicleRuntimeControl::configureRuntimeEnvironment() {
        // Try to configure the environment.
        clog << "(vehiclecontext) Configuring module 'SimplifiedBicycleModel'..." << endl;
        const KeyValueConfiguration kvcSimplifiedBicycleModel = getConfigurationFor(VehicleRuntimeControl::SIMPLIFIEDBICYCLEMODEL);
        if (kvcSimplifiedBicycleModel.getListOfKeys().size() > 0) {
            stringstream config;
            config << kvcSimplifiedBicycleModel;

            try {
                clog << config.str() << endl;

                SimplifiedBicycleModel *sbm = new SimplifiedBicycleModel(config.str());
                if (sbm != NULL) {
                    // Add SystemFeedbackComponent to StandaloneRuntimeControl for getting scheduled.
                    add(sbm);
                    clog << "(vehiclecontext) 'SimpleBicycleModel' configured." << endl;
                }
            }
            catch(...) {
                clog << "(vehiclecontext) Failed to configure 'SimpleBicycleModel'." << endl;
            }
        }
    }

} // vehiclecontext


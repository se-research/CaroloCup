/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef VEHICLECONTEXT_VEHICLERUNTIMECONTROL_H_
#define VEHICLECONTEXT_VEHICLERUNTIMECONTROL_H_

//#include "core/base/KeyValueConfiguration.h"

#include "context/base/StandaloneRuntimeControl.h"

namespace vehiclecontext {

    using namespace std;

    /**
     * This class is used to realize a standalone system simulations for vehicle-related simulations.
     */
    class OPENDAVINCI_API VehicleRuntimeControl : public context::base::StandaloneRuntimeControl {
		public:
			enum VEHICLECONTEXTMODULES {
				SIMPLIFIEDBICYCLEMODEL,
			};

        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            VehicleRuntimeControl(const VehicleRuntimeControl&);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            VehicleRuntimeControl& operator=(const VehicleRuntimeControl&);

        public:
            /**
             * Constructor.
             *
             * @param sci RuntimeControlInterface to be used.
             */
            VehicleRuntimeControl(const context::base::RuntimeControlInterface &sci);

            virtual ~VehicleRuntimeControl();

        private:
            core::base::KeyValueConfiguration m_globalConfiguration;

            /**
             * This method returns the appropriate configuration data.
             *
             * @param module Module to get configuration for.
             */
            const core::base::KeyValueConfiguration getConfigurationFor(const enum VEHICLECONTEXTMODULES &module);

            virtual void configureRuntimeEnvironment();
    };

} // vehiclecontext

#endif /*VEHICLECONTEXT_VEHICLERUNTIMECONTROL_H_*/

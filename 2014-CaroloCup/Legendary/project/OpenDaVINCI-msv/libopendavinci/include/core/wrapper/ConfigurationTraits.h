/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_CONFIGURATIONTRAITS_H_
#define OPENDAVINCI_CORE_WRAPPER_CONFIGURATIONTRAITS_H_

namespace core {
    namespace wrapper {

        /**
         * This template classes describe the configuration of a certain
         * product type.
         */
        template <typename product, product config_value> struct ConfigValue
        {
            typedef product product_type;
            static const product_type value = config_value;
        };

        template < typename product_type > struct ConfigurationTraits
        {
            typedef void configuration;
        };

        #define OPENDAVINCI_STATIC_CONFIGURATION(product, value) template <> struct ConfigurationTraits<product> { typedef ConfigValue<product, value> configuration; };
    }
}

#endif /* OPENDAVINCI_CORE_WRAPPER_CONFIGURATIONTRAITS_H_ */

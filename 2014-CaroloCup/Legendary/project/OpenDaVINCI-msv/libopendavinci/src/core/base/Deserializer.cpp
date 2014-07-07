/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Deserializer.h"
#include "core/wrapper/Libraries.h"

namespace core {
    namespace base {

        Deserializer::Deserializer() {}

        Deserializer::~Deserializer() {}

        float Deserializer::ntohf(float f) {
            if (core::wrapper::USESYSTEMENDINANESS == core::wrapper::IS_BIG_ENDIAN) {
                return f;
            }
            else {
                uint8_t *buf = (uint8_t*)&f;
                uint8_t tmp = 0;
                for(uint32_t i = 0, j = sizeof(float) - 1; i < sizeof(float) / 2; i++, j--) {
                    tmp = buf[i];
                    buf[i] = buf[j];
                    buf[j] = tmp;
                }
                return f;
            }
        }

        double Deserializer::ntohd(double d) {
            if (core::wrapper::USESYSTEMENDINANESS == core::wrapper::IS_BIG_ENDIAN) {
                return d;
            }
            else {
                uint8_t *buf = (uint8_t*)&d;
                uint8_t tmp = 0;
                for(uint32_t i = 0, j = sizeof(double) - 1; i < sizeof(double) / 2; i++, j--) {
                    tmp = buf[i];
                    buf[i] = buf[j];
                    buf[j] = tmp;
                }
                return d;
            }
        }

    }
} // core::base

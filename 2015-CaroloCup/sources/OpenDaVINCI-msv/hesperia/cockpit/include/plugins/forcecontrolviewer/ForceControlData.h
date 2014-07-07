/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_FORCECONTROLDATA_H_
#define COCKPIT_PLUGINS_FORCECONTROLDATA_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#ifndef PANDABOARD

#include <deque>

#include "QtIncludes.h"

#include "core/data/control/ForceControl.h"

namespace cockpit {

    namespace plugins {

      namespace forcecontrolviewer {

            using namespace std;

            /**
             * This class is the container for the controller widget.
             */
            class ForceControlData : public QwtData {
                public:
                    enum FORCECONTROLDATA_INTERFACE {
                        ACCELERATION_FORCE = 0,
                        BRAKE_FORCE        = 1,
                        STEERING_FORCE     = 2
                    };

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ForceControlData(const ForceControlData &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ForceControlData& operator=(const ForceControlData &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param data Reference to the deque of received ForceControl data.
                     * @param dataSelection Select the type of data that is returned.
                     */
                    ForceControlData(deque<core::data::control::ForceControl> &data, const FORCECONTROLDATA_INTERFACE &dataSelection);

                    virtual ~ForceControlData();

                    virtual QwtData *copy() const;

                    virtual size_t size() const;

                    virtual double x(size_t i) const;

                    virtual double y(size_t i) const;

                private:
                    deque<core::data::control::ForceControl> &m_data;
                    FORCECONTROLDATA_INTERFACE m_selectedData;                    
            };
        }
    }
}

#endif

#endif /*COCKPIT_PLUGINS_FORCECONTROLDATA_H_*/


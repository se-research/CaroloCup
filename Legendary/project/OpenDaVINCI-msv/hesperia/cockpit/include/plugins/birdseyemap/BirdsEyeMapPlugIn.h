/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PLUGINS_BIRDSEYEMAP_BIRDSEYEMAPPLUGIN_H_
#define PLUGINS_BIRDSEYEMAP_BIRDSEYEMAPPLUGIN_H_

#include "plugins/PlugIn.h"

#include "plugins/birdseyemap/BirdsEyeMapWidget.h"

namespace cockpit {
    namespace plugins {
        namespace birdseyemap {

            /**
             * This class is birdseye map plugin.
             */
            class BirdsEyeMapPlugIn : public PlugIn {

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BirdsEyeMapPlugIn(const BirdsEyeMapPlugIn &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BirdsEyeMapPlugIn& operator=(const BirdsEyeMapPlugIn &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param name Name of this plugin.
                     * @param kvc KeyValueConfiguration for this GL-based widget.
                     * @param prnt Pointer to the container super window.
                     */
                    BirdsEyeMapPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget* prnt);

                    virtual ~BirdsEyeMapPlugIn();

                    virtual QWidget* getQWidget() const;

                    virtual void setupPlugin();

                    virtual void stopPlugin();

                private:
                    BirdsEyeMapWidget *m_widget;
            };
        }
    }
} // plugins::birdseyemap

#endif /*PLUGINS_BIRDSEYEMAP_BIRDSEYEMAPPLUGIN_H_*/

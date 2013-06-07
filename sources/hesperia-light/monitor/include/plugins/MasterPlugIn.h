/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */


#ifndef MASTERPLUGIN_H_
#define MASTERPLUGIN_H_

#include "hesperia/base/DataStoreManager.h"

#include "PlugIn.h"

#include "FIFOMultiplexer.h"

namespace plugins {
class MasterPlugIn : public PlugIn {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            MasterPlugIn(const MasterPlugIn &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            MasterPlugIn& operator=(const MasterPlugIn &/*obj*/);

        protected:
            /**
             * Constructor.
             *
             * @param name Name of this plugin.
             * @param kvc KeyValueConfiguration for this plugin.
             * @param prnt Pointer to the containing super window.
             */
            MasterPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, hesperia::base::DataStoreManager &dsm, QWidget* prnt);

        public:
            virtual ~MasterPlugIn();

            /**
             * This method must be overridden in subclasses to setup
             * this plugin, i.e. construct all necessary GUI elements.
             */
            virtual void setupPlugin() = 0;

            /**
             * This method must be overridden in subclasses to stop
             * this plugin right before destruction.
             */
            virtual void stopPlugin() = 0;

            /**
             * This method returns the widget on which all content is drawn.
             *
             * @return Widget on which all content is drawn.
             */
            virtual QWidget* getQWidget() const = 0;

            /**
             * This method returns the used Multiplexer.
             * The standard is a the FIFOMultiplexer which can be replaced
             * in subclasses.
             *
             * @return FIFOMultiplexer.
             */
            virtual monitor::FIFOMultiplexer* getMultiplexer();

        protected:
        	void setMultiplexer(monitor::FIFOMultiplexer* multiplexer);

        private :
        	static core::base::Mutex m_singletonMutex;
        	monitor::FIFOMultiplexer* m_fifoMultiplexer;
 };

}

#endif /* MASTERPLUGIN_H_ */

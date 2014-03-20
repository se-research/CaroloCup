/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MDIPLUGIN_H_
#define MDIPLUGIN_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "QtIncludes.h"

#include "plugins/PlugIn.h"

namespace cockpit {

    using namespace std;

    /**
     * This class is a special QMdiSubWindow that invokes the stopPlugIn()
     * method before closing. Therefore, it is STRONGLY RECOMMENDED to
     * use this specialization.
     */
    class MdiPlugIn : public QMdiSubWindow {

            Q_OBJECT

        public:
            /**
             * Constructor.
             *
             * @param plugIn The plugin to which this plugin belongs to.
             * @param prnt Any parental window.
             * @param flags Any flags.
             */
            MdiPlugIn(plugins::PlugIn &plugIn, QWidget *prnt = 0, Qt::WindowFlags flags = 0);

            virtual ~MdiPlugIn();

        protected:
            virtual void closeEvent(QCloseEvent *evnt);

        private:
            plugins::PlugIn &m_plugIn;
    };

} // cockpit

#endif /*MDIPLUGIN_H_*/

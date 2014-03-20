/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PACKETLOGVIEWER_H_
#define PACKETLOGVIEWER_H_

#include "plugins/PlugIn.h"
#include "plugins/packetlogviewer/PacketLogViewerWidget.h"

namespace plugins
{
  namespace packetlogviewer
  {
    class PacketLogViewerPlugIn : public PlugIn
    {

    private:
      /**
       * "Forbidden" copy constructor. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the copy constructor.
       */
      PacketLogViewerPlugIn(const PacketLogViewerPlugIn &/*obj*/);

      /**
       * "Forbidden" assignment operator. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the assignment operator.
       */
      PacketLogViewerPlugIn&
      operator=(const PacketLogViewerPlugIn &/*obj*/);

    public:
      /**
       * Constructor.
       *
       * @param name Name of this plugin.
       * @param kvc KeyValueConfiguration for this GL-based widget.
       * @param prnt Pointer to the container super window.
       */
      PacketLogViewerPlugIn(const string &name,
          const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

      virtual
      ~PacketLogViewerPlugIn();

      virtual QWidget*
      getQWidget() const;

      virtual void
      setupPlugin();

      virtual void
      stopPlugin();

    private:
      PacketLogViewerWidget *m_viewerWidget;
    };

  }
}

#endif /* PACKETLOGVIEWER_H_ */

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PACKETLOGVIEWERWIDGET_H_
#define PACKETLOGVIEWERWIDGET_H_

#include <map>
#include <cstring>

#include "core/data/Container.h"
#include "core/io/ContainerListener.h"

#include "plugins/PlugIn.h"
#include "QtIncludes.h"

namespace plugins
{
  namespace packetlogviewer
  {
    using namespace std;
    using namespace core::data;

    /**
     * This class is the container for the OBJX viewer widget.
     */
    class PacketLogViewerWidget : public QWidget,
        public core::io::ContainerListener
    {

    Q_OBJECT

    private:
      /**
       * "Forbidden" copy constructor. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the copy constructor.
       */
      PacketLogViewerWidget(const PacketLogViewerWidget &/*obj*/);

      /**
       * "Forbidden" assignment operator. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the assignment operator.
       */
      PacketLogViewerWidget&
      operator=(const PacketLogViewerWidget &/*obj*/);

    public:
      /**
       * Constructor.
       *
       * @param plugIn Reference to the plugin to which this widget belongs.
       * @param prnt Pointer to the parental widget.
       */
      PacketLogViewerWidget(const PlugIn &plugIn, QWidget *prnt);

      virtual
      ~PacketLogViewerWidget();

      virtual void
      nextContainer(Container &c);

    private:
      QTreeWidget* m_dataView;
      map<Container::DATATYPE, QTreeWidgetItem* > m_dataToType;

      string DataToString(Container &container);

    };
  }
}

#endif /* PACKETLOGVIEWERWIDGET_H_ */

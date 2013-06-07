/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <sstream>
#include <cstring>

#include "hesperia/data/Configuration.h"
#include "hesperia/data/sensor/ContouredObject.h"
#include "hesperia/data/sensor/ContouredObjects.h"
#include "hesperia/data/dmcp/DiscoverMessage.h"
#include "hesperia/data/dmcp/ModuleStateMessage.h"
#include "hesperia/data/dmcp/ModuleExitCodeMessage.h"
#include "hesperia/data/control/VehicleControl.h"
#include "hesperia/data/environment/EgoState.h"
#include "hesperia/data/environment/Obstacle.h"
#include "hesperia/data/environment/OtherVehicleState.h"
#include "hesperia/data/planning/Route.h"
#include "hesperia/data/sensor/LMS291Message.h"
#include "hesperia/data/dmcp/ModuleStatistics.h"
#include "hesperia/data/player/PlayerCommand.h"
#include "hesperia/data/environment/Obstacle.h"
#include "hesperia/data/environment/PointShapedObject.h"
#include "hesperia/data/environment/Position.h"
#include "hesperia/data/recorder/RecorderCommand.h"
#include "core/data/RuntimeStatistic.h"
#include "core/data/SharedData.h"
#include "hesperia/data/image/SharedImage.h"
#include "core/data/TimeStamp.h"

#include "plugins/packetlogviewer/PacketLogViewerWidget.h"

namespace plugins
{
  namespace packetlogviewer
  {

    using namespace std;
    using namespace core::data;

    PacketLogViewerWidget::PacketLogViewerWidget(const PlugIn &/*plugIn*/,
        QWidget *prnt) :
      QWidget(prnt), m_dataView(NULL), m_dataToType()
    {

      // Set size.
      setMinimumSize(640, 480);

      // Layout manager.
      QGridLayout* mainBox = new QGridLayout(this);

      //ListView and header construction
      m_dataView = new QTreeWidget(this);
      m_dataView->setColumnCount(3);
      QStringList headerLabel;
      headerLabel << tr("Datatype") << tr("Received@") << tr("Sent@");
      m_dataView->setColumnWidth(0, 200);
      m_dataView->setColumnWidth(1, 200);
      m_dataView->setHeaderLabels(headerLabel);

      //add to Layout
      mainBox->addWidget(m_dataView, 0, 0);

      // Set layout manager.
      setLayout(mainBox);
    }

    PacketLogViewerWidget::~PacketLogViewerWidget()
    {
      HESPERIA_CORE_DELETE_POINTER(m_dataView);
    }

    void
    PacketLogViewerWidget::nextContainer(Container &container)
    {

      //create new Header if needed
      if (m_dataToType.find(container.getDataType()) == m_dataToType.end())
        {
          QTreeWidgetItem * newHeader = new QTreeWidgetItem(m_dataView);
          newHeader->setText(0, container.toString().c_str());
          m_dataToType[container.getDataType()] = newHeader;
        }

      //append Data to respective Header
      QTreeWidgetItem * dataItem = new QTreeWidgetItem();
      dataItem->setText(0, DataToString(container).c_str());
      dataItem->setText(1, container.getReceivedTimeStamp().getYYYYMMDD_HHMMSSms().c_str());
      dataItem->setText(2, container.getSentTimeStamp().getYYYYMMDD_HHMMSSms().c_str());
      m_dataToType[container.getDataType()]->insertChild(0, dataItem);

      if (m_dataToType[container.getDataType()]->childCount() > 100000) {
        m_dataToType[container.getDataType()]->removeChild(m_dataToType[container.getDataType()]->takeChild(100000));
      }
    }

    string
    PacketLogViewerWidget::DataToString(Container &container)
    {
      stringstream cs;

      switch (container.getDataType())
        {
      case Container::CONFIGURATION:
        return container.getData<hesperia::data::Configuration> ().toString();
      case Container::CONTOUREDOBJECT:
        return container.getData<hesperia::data::sensor::ContouredObject> ().toString();
      case Container::CONTOUREDOBJECTS:
        return container.getData<hesperia::data::sensor::ContouredObjects> ().toString();
      case Container::DMCP_DISCOVER:
        return container.getData<hesperia::data::dmcp::DiscoverMessage> ().toString();
      case Container::DMCP_CONFIGURATION_REQUEST:
        return container.getData<hesperia::data::dmcp::ModuleDescriptor> ().toString();
      case Container::DMCP_MODULESTATEMESSAGE:
        return container.getData<hesperia::data::dmcp::ModuleStateMessage> ().toString();
      case Container::DMCP_MODULEEXITCODEMESSAGE:
        return container.getData<hesperia::data::dmcp::ModuleExitCodeMessage> ().toString();
      case Container::EGOSTATE:
        return container.getData<hesperia::data::environment::EgoState> ().toString();
      case Container::LMS291MESSAGE:
        return container.getData<hesperia::data::sensor::LMS291Message> ().toString();
      case Container::MODULESTATISTICS:
        return container.getData<hesperia::data::dmcp::ModuleStatistics> ().toString();
      case Container::OBSTACLE:
        return container.getData<hesperia::data::environment::Obstacle> ().toString();
      case Container::OTHERVEHICLESTATE:
        return container.getData<hesperia::data::environment::OtherVehicleState> ().toString();
      case Container::PLAYER_COMMAND:
        return container.getData<hesperia::data::player::PlayerCommand> ().toString();
      case Container::POINTSHAPEDOBJECT:
        return container.getData<hesperia::data::environment::PointShapedObject> ().toString();
      case Container::POSITION:
        return container.getData<hesperia::data::environment::Position> ().toString();
      case Container::RECORDER_COMMAND:
        return container.getData<hesperia::data::recorder::RecorderCommand> ().toString();
      case Container::ROUTE:
        return container.getData<hesperia::data::planning::Route> ().toString();
      case Container::RUNTIMESTATISTIC:
        return container.getData<core::data::RuntimeStatistic> ().toString();
      case Container::SHARED_DATA:
        return container.getData<core::data::SharedData> ().toString();
      case Container::SHARED_IMAGE:
        return container.getData<hesperia::data::image::SharedImage> ().toString();
      case Container::TIMESTAMP:
        return container.getData<core::data::TimeStamp> ().toString();
      case Container::VEHICLECONTROL:
        return container.getData<hesperia::data::control::VehicleControl> ().toString();
      default :
        cs << container;
        return cs.str();
        }
    }
  }
}

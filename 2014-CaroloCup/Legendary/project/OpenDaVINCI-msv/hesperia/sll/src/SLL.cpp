/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include <cmath>
#include <algorithm>

#include "core/macros.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/base/KeyValueDataStore.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/io/URL.h"
#include "core/wrapper/StringComparator.h"

#include "core/data/Constants.h"
#include "hesperia/data/sensor/TSDDistances.h"
#include "hesperia/data/environment/EgoState.h"
#include "hesperia/data/environment/Obstacle.h"
#include "hesperia/data/scenario/Scenario.h"
#include "hesperia/data/scenario/Shape.h"
#include "hesperia/data/scenario/Polygon.h"
#include "hesperia/data/scenario/Surroundings.h"
#include "hesperia/data/scenario/Vertex3.h"
#include "hesperia/scenario/SCNXArchive.h"
#include "hesperia/scenario/SCNXArchiveFactory.h"
#include "hesperia/scenario/ScenarioFactory.h"
#include "hesperia/scenario/ScenarioPrettyPrinter.h"

#include "hesperia/data/environment/Line.h"

#include "TSDPointSensor.h"

#include "SLL.h"

namespace sll {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::io;
    using namespace hesperia::data;
    using namespace hesperia::data::environment;
    using namespace hesperia::data::sensor;
    using namespace hesperia::scenario;

    SLL::SLL(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "sll"),
        m_numberOfPolygons(0),
        m_mapOfPolygons(),
        m_sensor(NULL),
        m_distancesPerBeam()
    {}

    SLL::~SLL() {}

    void SLL::setUp() {}

    void SLL::tearDown() {}

    ModuleState::MODULE_EXITCODE SLL::body() {
        // Load scenario.
        const URL urlOfSCNXFile(getKeyValueConfiguration().getValue<string>("global.scenario"));
        if (urlOfSCNXFile.isValid()) {
            SCNXArchive &scnxArchive = SCNXArchiveFactory::getInstance().getSCNXArchive(urlOfSCNXFile);

            hesperia::data::scenario::Scenario &scenario = scnxArchive.getScenario();

            const hesperia::data::scenario::Surroundings &surroundings = scenario.getGround().getSurroundings();
            const vector<hesperia::data::scenario::Shape*> &listOfShapes = surroundings.getListOfShapes();
            vector<hesperia::data::scenario::Shape*>::const_iterator it = listOfShapes.begin();
            while (it != listOfShapes.end()) {
                hesperia::data::scenario::Shape *shape = (*it++);
                if (shape != NULL) {
                    if (shape->getType() == hesperia::data::scenario::Shape::POLYGON) {
                        hesperia::data::scenario::Polygon *polygon = dynamic_cast<hesperia::data::scenario::Polygon*>(shape);
                        if (polygon != NULL) {
                            Polygon p;
                            m_numberOfPolygons++;

                            const vector<hesperia::data::scenario::Vertex3> &listOfVertices = polygon->getListOfVertices();
                            vector<hesperia::data::scenario::Vertex3>::const_iterator jt = listOfVertices.begin();
                            while (jt != listOfVertices.end()) {
                                p.add(*jt++);
                            }
                            m_mapOfPolygons[m_numberOfPolygons] = p;
                        }
                    }
                }
            }
        }

        // Show found polygons on console and in monitor.
        const bool showPolygons = getKeyValueConfiguration().getValue<bool>("sll.showPolygons");
        if (showPolygons) {
            map<uint32_t, Polygon>::iterator it = m_mapOfPolygons.begin();
            while (it != m_mapOfPolygons.end()) {
                const uint32_t polygonID = it->first;
                Polygon p = it->second;

                Obstacle polygonObstacle(polygonID, Obstacle::UPDATE);
                polygonObstacle.setPolygon(p);

                // Send obstacle.
                Container c = Container(Container::OBSTACLE, polygonObstacle);
                getConference().send(c);

                cerr << "Found polygon: " << it->second.toString() << endl;
                it++;
            }
        }

        // Setup TSD sensor.
        stringstream sensorID;
        sensorID << "sll.id";
        uint16_t id(getKeyValueConfiguration().getValue<uint16_t>(sensorID.str()));

        stringstream sensorName;
        sensorName << "sll.name";
        string name(getKeyValueConfiguration().getValue<string>(sensorName.str()));
        
        stringstream sensorTranslation;
        sensorTranslation << "sll.translation";
        Point3 translation(getKeyValueConfiguration().getValue<string>(sensorTranslation.str()));

        stringstream sensorRotZ;
        sensorRotZ << "sll.rotZ";
        const double rotZ = getKeyValueConfiguration().getValue<double>(sensorRotZ.str());
        
        stringstream sensorMinDistance;
        sensorMinDistance << "sll.minDistance";
        const double minDistance = getKeyValueConfiguration().getValue<double>(sensorMinDistance.str());
        
        stringstream sensorMaxDistance;
        sensorMaxDistance << "sll.maxDistance";
        const double maxDistance = getKeyValueConfiguration().getValue<double>(sensorMaxDistance.str());
        
        stringstream sensorClampDistance;
        sensorClampDistance << "sll.clampDistance";
        const double clampDistance = getKeyValueConfiguration().getValue<double>(sensorClampDistance.str());
        
        m_sensor = new TSDPointSensor(id, name, translation, rotZ, minDistance, maxDistance, clampDistance);

        // Initialize distance map entry.
        for(uint32_t i = 0; i < 360; i++) {
            m_distancesPerBeam[i] = -1;
        }

        // Use the most recent EgoState available.
        KeyValueDataStore &kvs = getKeyValueDataStore();

        // Loop through the map of polygons with the current EgoState and intersect all with the point sensor's FOV.
        while (getModuleState() == ModuleState::RUNNING) {
            // Get current EgoState.
            Container c = kvs.get(Container::EGOSTATE);
            EgoState es = c.getData<EgoState>();

            // Update sensor position.
            m_sensor->update(es.getPosition(), es.getRotation());

            // Calculate distances.
            m_sensor->getDistances(m_mapOfPolygons, m_distancesPerBeam);

            TSDDistances tsd;
            tsd.setDistanceMap(m_distancesPerBeam);

    		c = Container(Container::TSDDISTANCES, tsd);

            getConference().send(c);
        }

        // Delete all point sensors.
        OPENDAVINCI_CORE_DELETE_POINTER(m_sensor);           

        return ModuleState::OKAY;
    }

} // sll

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "hesperia/scenegraph/primitives/Polygon.h"

namespace hesperia {
    namespace scenegraph {
        namespace primitives {

            using namespace std;
            using namespace core::data::environment;

            Polygon::Polygon(const SceneNodeDescriptor &sceneNodeDescriptor, const vector<Point3> &listOfGroundVertices, const Point3 &color, const float &height) :
                SceneNode(sceneNodeDescriptor),
                m_listOfGroundVertices(listOfGroundVertices),
                m_color(color),
                m_height(height) {
                // Add the first point to the end to close the polyon.
                if (m_listOfGroundVertices.size() > 0) {
                    Point3 p = *(m_listOfGroundVertices.begin());
                    m_listOfGroundVertices.push_back(p);
                }
            }

            Polygon::Polygon(const Polygon &obj) :
                SceneNode(obj.getSceneNodeDescriptor()),
                m_listOfGroundVertices(obj.m_listOfGroundVertices),
                m_color(obj.m_color),
                m_height(obj.m_height) {}

            Polygon::~Polygon() {}

            Polygon& Polygon::operator=(const Polygon &obj) {
                setSceneNodeDescriptor(obj.getSceneNodeDescriptor());
                m_listOfGroundVertices = obj.m_listOfGroundVertices;
                m_color = obj.m_color;
                m_height = obj.m_height;
                return (*this);
            }

            const vector<Point3>& Polygon::getListOfGroundVertices() const {
                return m_listOfGroundVertices;
            }

            const Point3& Polygon::getColor() const {
                return m_color;
            }

            float Polygon::getHeight() const {
                return m_height;
            }

        }
    }
} // hesperia::scenegraph::primitives

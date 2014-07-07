/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

// The following include is necessary on Win32 platforms to set up necessary macro definitions.
#ifdef WIN32
#include <windows.h>
#endif

#include <GL/gl.h>

#include "hesperia/threeD/models/Polygon.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            using namespace std;
            using namespace core::data::environment;

            Polygon::Polygon(const NodeDescriptor &nodeDescriptor, const vector<Point3> &listOfGroundVertices, const Point3 &color, const float &height) :
                    Node(nodeDescriptor),
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
                    Node(obj.getNodeDescriptor()),
                    m_listOfGroundVertices(obj.m_listOfGroundVertices),
                    m_color(obj.m_color),
                    m_height(obj.m_height) {}

            Polygon::~Polygon() {}

            Polygon& Polygon::operator=(const Polygon &obj) {
                setNodeDescriptor(obj.getNodeDescriptor());
                m_listOfGroundVertices = obj.m_listOfGroundVertices;
                m_color = obj.m_color;
                m_height = obj.m_height;
                return (*this);
            }

            void Polygon::render(RenderingConfiguration &renderingConfiguration) {
                if ((getNodeDescriptor().getName().size() == 0) || (renderingConfiguration.getNodeRenderingConfiguration(getNodeDescriptor()).hasParameter(NodeRenderingConfiguration::ENABLED))) {
                    glPushMatrix();
                    {
                        glColor3d(m_color.getX(), m_color.getY(), m_color.getZ());

                        glBegin(GL_QUADS);
                        for (uint32_t i = 0; i < m_listOfGroundVertices.size() - 1; i++) {
                            const Point3 &p1 = m_listOfGroundVertices[i];
                            const Point3 &p2 = m_listOfGroundVertices[i+1];

                            Point3 P12 = p2 - p1;
                            P12.setZ(0);
                            const Point3 P1H = Point3(p1.getX(), p1.getY(), m_height);
                            const Point3 P1HxP12 = P1H.cross(P12);

                            glVertex3d(p1.getX(), p1.getY(), 0);
                            glVertex3d(p1.getX(), p1.getY(), m_height);
                            glVertex3d(p2.getX(), p2.getY(), m_height);
                            glVertex3d(p2.getX(), p2.getY(), 0);
                            glNormal3d(P1HxP12.getX(), P1HxP12.getY(), P1HxP12.getZ());
                        }
                        glEnd();

                        // Bottom of the polygon.
                        glBegin(GL_POLYGON);
                        for (uint32_t i = 0; i < m_listOfGroundVertices.size(); i++) {
                            const Point3 &p1 = m_listOfGroundVertices[i];
                            glVertex3d(p1.getX(), p1.getY(), 0);
                        }
                        glEnd();

                        // Top of the polygon.
                        glBegin(GL_POLYGON);
                        for (uint32_t i = 0; i < m_listOfGroundVertices.size(); i++) {
                            const Point3 &p1 = m_listOfGroundVertices[i];
                            glVertex3d(p1.getX(), p1.getY(), m_height);
                        }
                        glEnd();
                    }
                    glPopMatrix();
                }
            }

        }
    }
} // hesperia::threeD::models

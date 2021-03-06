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

#include "hesperia/threeD/models/Grid.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            Grid::Grid(const NodeDescriptor &nodeDescriptor, const uint32_t &size, const float &lineWidth) :
                    Node(nodeDescriptor),
                    m_size(size),
                    m_lineWidth(lineWidth) {}

            Grid::Grid(const Grid &obj) :
                    Node(obj.getNodeDescriptor()),
                    m_size(obj.m_size),
                    m_lineWidth(obj.m_lineWidth) {}

            Grid::~Grid() {}

            Grid& Grid::operator=(const Grid &obj) {
                setNodeDescriptor(obj.getNodeDescriptor()),
                m_size = obj.m_size;
                m_lineWidth = obj.m_lineWidth;
                return (*this);
            }

            void Grid::render(const RenderingConfiguration &renderingConfiguration) const {
                if ((getNodeDescriptor().getName().size() == 0) || (renderingConfiguration.getNodeRenderingConfiguration(getNodeDescriptor()).hasParameter(NodeRenderingConfiguration::ENABLED))) {
                    glPushMatrix();
                    {
                        glLineWidth(m_lineWidth);
                        glColor3f(1, 1, 1);

                        glBegin(GL_LINES);
                        int32_t size = m_size;
                        for (int32_t y = -size; y <= size; y++) {
                            for (int32_t x = -size; x <= size; x++) {
                                // X-axis.
                                glVertex3f(0, static_cast<float>(y), 0);
                                glVertex3f(static_cast<float>(x), static_cast<float>(y), 0);

                                // Y-axis.
                                glVertex3f(static_cast<float>(x), 0, 0);
                                glVertex3f(static_cast<float>(x), static_cast<float>(y), 0);
                            }
                        }
                        glEnd();

                        glLineWidth(1);
                    }
                    glPopMatrix();
                }
            }

        }
    }
} // threeDengine::models

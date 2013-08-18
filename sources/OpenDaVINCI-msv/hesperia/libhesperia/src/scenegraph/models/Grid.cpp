/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/data/Constants.h"
#include "hesperia/scenegraph/models/Grid.h"

namespace hesperia {
    namespace scenegraph {
        namespace models {

            using namespace std;
            using namespace core::data;
            using namespace core::data::environment;
            using namespace hesperia::scenegraph::primitives;

            Grid::Grid(const SceneNodeDescriptor &sceneNodeDescriptor) :
                SceneNode(sceneNodeDescriptor) {
                createGrid();
            }

            Grid::Grid(const Grid &obj) :
                SceneNode(obj.getSceneNodeDescriptor()) {
                createGrid();
            }

            Grid& Grid::operator=(const Grid &obj) {
                setSceneNodeDescriptor(obj.getSceneNodeDescriptor());

                deleteAllChildren();

                createGrid();

                return *this;
            }

            Grid::~Grid() {}

            void Grid::createGrid() {
                const double MIN_AXES = -150;
                const double MAX_AXES = 150;

                for(int32_t y = MIN_AXES; y <= MAX_AXES; y++) {
                    Point3 yAxisGridLineA(MIN_AXES, y, 0);
                    Point3 yAxisGridLineB(MAX_AXES, y, 0);

                    Line* yAxisGridLine = new Line(getSceneNodeDescriptor(), yAxisGridLineA, yAxisGridLineB, Point3(.7, .7, .7), 1);

                    addChild(yAxisGridLine);
                }
                for(int32_t x = MIN_AXES; x <= MAX_AXES; x++) {
                    Point3 xAxisGridLineA(x, MIN_AXES, 0);
                    Point3 xAxisGridLineB(x, MAX_AXES, 0);

                    Line* xAxisGridLine = new Line(getSceneNodeDescriptor(), xAxisGridLineA, xAxisGridLineB, Point3(.7, .7, .7), 1);

                    addChild(xAxisGridLine);
                }
            }

        }
    }
}  // hesperia::scenegraph::models


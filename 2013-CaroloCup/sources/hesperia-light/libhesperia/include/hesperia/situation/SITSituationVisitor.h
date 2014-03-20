/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_SITUATION_SITSITUATIONVISITOR_H_
#define HESPERIA_CORE_SITUATION_SITSITUATIONVISITOR_H_

#include "hesperia/data/situation/Behavior.h"
#include "hesperia/data/situation/ComplexModel.h"
#include "hesperia/data/situation/ExternalDriver.h"
#include "hesperia/data/situation/Header.h"
#include "hesperia/data/situation/Object.h"
#include "hesperia/data/situation/OnEnteringPolygon.h"
#include "hesperia/data/situation/OnMoving.h"
#include "hesperia/data/situation/PointIDDriver.h"
#include "hesperia/data/situation/Polygon.h"
#include "hesperia/data/situation/Rectangle.h"
#include "hesperia/data/situation/Shape.h"
#include "hesperia/data/situation/Situation.h"
#include "hesperia/data/situation/Vertex3.h"
#include "core/exceptions/Exceptions.h"
#include "core/wrapper/parser/ASTVisitor.h"

namespace hesperia {
    namespace situation {

        using namespace std;

        /**
         * This class prints the parsed AST.
         */
        class SITSituationVisitor : public core::wrapper::parser::ASTVisitor {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SITSituationVisitor(const SITSituationVisitor &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SITSituationVisitor& operator=(const SITSituationVisitor &);

            public:
                SITSituationVisitor(data::situation::Situation &s);

                virtual ~SITSituationVisitor();

                virtual void visit(core::wrapper::parser::ASTNode *node) throw (core::exceptions::SITSituationVisitorException);

            private:
                data::situation::Situation &m_situation;

                /**
                 * Read SITUATIONHEADER.
                 *
                 * @param node Node describing the situation header.
                 * @return Parsed situation header.
                 */
                const data::situation::Header visitSituationHeader(core::wrapper::parser::ASTNode *node);

                /**
                 * Read OBJECT.
                 *
                 * @param node Node describing the object.
                 * @return Parsed object.
                 */
                data::situation::Object visitObject(core::wrapper::parser::ASTNode *node);

                /**
                 * Read SHAPE.
                 *
                 * @param node Node describing the shape.
                 * @return Parsed shape.
                 */
                data::situation::Shape* visitShape(core::wrapper::parser::ASTNode *node);

                /**
                 * Read VERTEX3.
                 *
                 * @param node Node describing the vertex3.
                 * @return Parsed vertex3.
                 */
                const data::situation::Vertex3 visitVertex3(core::wrapper::parser::ASTNode *node);

                /**
                 * Read POLYGON.
                 *
                 * @param node Node describing the polygon.
                 * @return Parsed polygon.
                 */
                data::situation::Polygon* visitPolygon(core::wrapper::parser::ASTNode *node);

                /**
                 * Read RECTANGLE.
                 *
                 * @param node Node describing the rectangle.
                 * @return Parsed rectangle.
                 */
                data::situation::Rectangle* visitRectangle(core::wrapper::parser::ASTNode *node);

                /**
                 * Read COMPLEXMODEL.
                 *
                 * @param node Node describing the complexmodel.
                 * @return Parsed complexmodel.
                 */
                data::situation::ComplexModel* visitComplexModel(core::wrapper::parser::ASTNode *node);

                /**
                 * Read BEHAVIOR.
                 *
                 * @param node Node describing the behavior.
                 * @return Parsed behavior.
                 */
                data::situation::Behavior* visitBehavior(core::wrapper::parser::ASTNode *node);

                /**
                 * Read EXTERNALDRIVER.
                 *
                 * @param node Node describing the external driver.
                 * @return Parsed external driver.
                 */
                data::situation::ExternalDriver* visitExternalDriver(core::wrapper::parser::ASTNode *node);

                /**
                 * Read POINTIDDRIVER.
                 *
                 * @param node Node describing the point ID driver.
                 * @return Parsed point ID driver.
                 */
                data::situation::PointIDDriver* visitPointIDDriver(core::wrapper::parser::ASTNode *node);

                /**
                 * Read ONMOVING.
                 *
                 * @param node Node describing the ONMOVING.
                 * @return Parsed ONMOVING.
                 */
                data::situation::OnMoving* visitOnMoving(core::wrapper::parser::ASTNode *node);

                /**
                 * Read ONENTERINGPOLYGON.
                 *
                 * @param node Node describing the ONENTERINGPOLYGON.
                 * @return Parsed ONENTERINGPOLYGON.
                 */
                data::situation::OnEnteringPolygon* visitOnEnteringPolygon(core::wrapper::parser::ASTNode *node);

                /**
                 * This method tries to find a node with the given key.
                 *
                 * @param key Key to be found.
                 * @param node Node from which the search should be started.
                 * @param maxDepth Maximum depth to descend.
                 * @return Node or NULL.
                 */
                core::wrapper::parser::ASTNode* findByKey(const string &key, core::wrapper::parser::ASTNode *node, const int32_t &maxDepth);

                /**
                 * This method tries to find a node with the given value.
                 *
                 * @param value Value to be found.
                 * @param node Node from which the search should be started.
                 * @param maxDepth Maximum depth to descend.
                 * @return Node or NULL.
                 */
                core::wrapper::parser::ASTNode* findByValue(const string &value, core::wrapper::parser::ASTNode *node, const int32_t &maxDepth);
        };

    }
} // hesperia::situation

#endif /*HESPERIA_CORE_SITUATION_SITSITUATIONVISITOR_H_*/

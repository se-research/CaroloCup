/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_WRAPPER_PARSER_ASTVISITOR_H_
#define HESPERIA_WRAPPER_PARSER_ASTVISITOR_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {
        namespace parser {

            // Forward declaration to prevent circular dependencies.
            class ASTNode;

            /**
             * This class is the interface for any AST visitor.
             */
            class OPENDAVINCI_API ASTVisitor {
                public:
                    virtual ~ASTVisitor();

                    /**
                     * This method is called to visit the given node by
                     * this visitor.
                     *
                     * @param node Node to be visited.
                     */
                    virtual void visit(ASTNode *node) = 0;
            };

        }
    }
} // core::wrapper::parser

#endif /*HESPERIA_WRAPPER_PARSER_ASTVISITOR_H_*/

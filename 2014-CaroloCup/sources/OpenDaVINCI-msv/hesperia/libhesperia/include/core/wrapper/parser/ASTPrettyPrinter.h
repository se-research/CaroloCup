/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_WRAPPER_PARSER_ASTPRETTYPRINTER_H_
#define HESPERIA_WRAPPER_PARSER_ASTPRETTYPRINTER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/parser/ASTVisitor.h"

namespace core {
    namespace wrapper {
        namespace parser {

            using namespace std;

            /**
             * This class prints the parsed AST.
             */
            class ASTPrettyPrinter : public ASTVisitor {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ASTPrettyPrinter(const ASTPrettyPrinter &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ASTPrettyPrinter& operator=(const ASTPrettyPrinter &);

                public:
                    ASTPrettyPrinter();

                    virtual ~ASTPrettyPrinter();

                    virtual void visit(ASTNode *node);

                private:
                    uint32_t m_depth;
            };

        }
    }
} // core::wrapper::parser

#endif /*HESPERIA_WRAPPER_PARSER_ASTPRETTYPRINTER_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <boost/spirit/include/classic_parse_tree.hpp>

#include "hesperia/scenario/SCNGrammar.h"

namespace hesperia {
    namespace scenario {

        using namespace core::wrapper::parser;

        SCNGrammarTokenIdentifier::SCNGrammarTokenIdentifier(const enum IDENTIFIERS &id) :
                m_id(id) {}

        SCNGrammarTokenIdentifier::~SCNGrammarTokenIdentifier() {}

        enum SCNGrammarTokenIdentifier::IDENTIFIERS SCNGrammarTokenIdentifier::getIdentifier() const {
            return m_id;
        }

        SCNGrammar::SCNGrammar(ParserTokenListener &ptl, ParserErrorListener &pel) :
                BoostSpiritGrammar(ptl, pel),
                expect(0),
                m_guard() {}

        SCNGrammar::~SCNGrammar() {}

        ASTNode* SCNGrammar::getAST(const string &s) {
            // Reset lines counter.
            resetLines();

            // Parse the data and build an AST.
            ASTNode *root = NULL;
            tree_parse_info<> info = ast_parse(s.c_str(), *this, comment_p("/*", "*/"));

            // If the instance of the grammar could successfully parsed, build the AST.
            if (info.full == 1) {
                root = new ASTNode();
                buildAST(info.trees.begin(), root);
            }

            return root;
        }

    }
} // hesperia::scenario

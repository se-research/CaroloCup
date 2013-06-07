/*
 * Copyright (c) Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_PARSERTESTSUITE_H_
#define CORE_PARSERTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>
#include <string>
#include <vector>

#include <boost/spirit/include/classic_error_handling.hpp>
#include <boost/spirit/include/classic_confix.hpp>
#include <boost/spirit/include/classic_core.hpp>

#include "core/wrapper/parser/ASTNode.h"
#include "core/wrapper/parser/BoostSpiritGrammar.h"
#include "core/wrapper/parser/ParserToken.h"
#include "core/wrapper/parser/ParserTokenListener.h"
#include "core/wrapper/parser/ParserTokenExtendedData.h"
#include "core/wrapper/parser/ParserError.h"
#include "core/wrapper/parser/ParserErrorListener.h"
#include "core/wrapper/parser/ParserErrorExtendedData.h"

using namespace std;
using namespace core::wrapper::parser;
using namespace boost::spirit::classic;

class TokenIdentifierParserTest : public ParserTokenExtendedData {
    public:
        /** IDs for terminals and non-terminals. */
        enum IDS { S_ID_ = 1, HEADER_ID_, FILENAME_ID_, TESTENTRY_ID_, NEWLINE_ID_, TAB_ID_, ALPHANUM_ID_ };

        TokenIdentifierParserTest(const enum IDS &id) :
                m_id(id) {}

        IDS m_id;
};

class ErrorIdentifierParserTest : public ParserErrorExtendedData {
    public:
        enum Errors {
            BLA = 5,
            BLUBB = 2
        };

        ErrorIdentifierParserTest(const enum Errors &e) :
                m_error(e) {}

        Errors m_error;
};

class MyParserTokenListenerParserTest : public ParserTokenListener {
    public:
        MyParserTokenListenerParserTest() : output() {};

        void nextToken(const ParserToken &token) {
            const TokenIdentifierParserTest &tid = dynamic_cast<const TokenIdentifierParserTest&>(token.getExtendedData());
            stringstream s;
            s << "Processing: " << tid.m_id << ", value: " << token.getValue() << endl;
            output.push_back(s.str());
            clog << s.str();
        }
        vector<string> output;
};

class MyParserErrorListenerParserTest : public ParserErrorListener {
    public:
        MyParserErrorListenerParserTest() : output() {};

        void errorToken(ParserError &error) {
            const ErrorIdentifierParserTest &eid = dynamic_cast<const ErrorIdentifierParserTest&>(error.getExtendedData());
            stringstream s;
            s << "ErrorListener: " << eid.m_error << " at line: " << eid.getLine() << " in context '" << error.getContext() << "'" << endl;
            output.push_back(s.str());
            clog << s.str();
        }
        vector<string> output;
};

class GrammarParserTest : public BoostSpiritGrammar, public grammar<GrammarParserTest> {
    public:
        assertion<ErrorIdentifierParserTest::Errors> expect_BLA;
        assertion<ErrorIdentifierParserTest::Errors> expect_BLUBB;
        guard<ErrorIdentifierParserTest::Errors> my_guard;

        GrammarParserTest(ParserTokenListener &gtl, ParserErrorListener &gel) :
                BoostSpiritGrammar(gtl, gel),
                expect_BLA(ErrorIdentifierParserTest::BLA),
                expect_BLUBB(ErrorIdentifierParserTest::BLUBB),
                my_guard() {};

    public:

        virtual ASTNode* getAST(const string &s) {
            resetLines();
            parse_info<> info = boost::spirit::classic::parse(s.c_str(), *this, comment_p("/*", "*/"));
            ASTNode *n = NULL;
            if (info.full == 1) {
                n = new ASTNode();
            }
            return n;
        }

        template <typename ScannerT>
        struct definition {
            definition(GrammarParserTest const &self) :
                    S(),
                    HEADER(),
                    FILENAME(),
                    TESTENTRY(),
                    NEWLINE(),
                    TAB(),
                    ALPHANUM() {
                S = *NEWLINE >> HEADER >> !NEWLINE;

                HEADER =  str_p("NAME") >> TAB
                          >> self.my_guard(self.expect_BLA(FILENAME[BoostSpiritGrammar::ParserTokenHandler(self, new TokenIdentifierParserTest(TokenIdentifierParserTest::FILENAME_ID_))]))[BoostSpiritGrammar::ParserErrorHandler(self, new ErrorIdentifierParserTest(ErrorIdentifierParserTest::BLA))]
                          >> NEWLINE
                          >> self.my_guard(self.expect_BLUBB(TESTENTRY[BoostSpiritGrammar::ParserTokenHandler(self, new TokenIdentifierParserTest(TokenIdentifierParserTest::TESTENTRY_ID_))]))[BoostSpiritGrammar::ParserErrorHandler(self, new ErrorIdentifierParserTest(ErrorIdentifierParserTest::BLUBB))]
                          >> NEWLINE;

                FILENAME = + ALPHANUM;

                TESTENTRY = + ALPHANUM;

                ALPHANUM = ch_p('A');

                TAB = + blank_p;

                NEWLINE = *TAB >> + eol_p[BoostSpiritGrammar::ParserNewlineHandler(self)]; // Count new lines.
            }

            // Identifier für die Nicht-Terminale der Grammatik.
            rule<ScannerT, parser_context<>, parser_tag<TokenIdentifierParserTest::S_ID_> > S;
            rule<ScannerT, parser_context<>, parser_tag<TokenIdentifierParserTest::HEADER_ID_> > HEADER;
            rule<ScannerT, parser_context<>, parser_tag<TokenIdentifierParserTest::FILENAME_ID_> > FILENAME;
            rule<ScannerT, parser_context<>, parser_tag<TokenIdentifierParserTest::TESTENTRY_ID_> > TESTENTRY;

            // Identifier für die Terminale der Grammatik.
            rule<ScannerT, parser_context<>, parser_tag<TokenIdentifierParserTest::NEWLINE_ID_> > NEWLINE;
            rule<ScannerT, parser_context<>, parser_tag<TokenIdentifierParserTest::TAB_ID_> > TAB;
            rule<ScannerT, parser_context<>, parser_tag<TokenIdentifierParserTest::ALPHANUM_ID_> > ALPHANUM;

            // Startregel für den Parser.
            rule<ScannerT, parser_context<>, parser_tag<TokenIdentifierParserTest::S_ID_> > const& start() const {
                return S;
            }
        };
};


class ParserTest : public CxxTest::TestSuite {
    public:
        void testParser() {
            MyParserTokenListenerParserTest mptl;
            MyParserErrorListenerParserTest mpel;
            stringstream s;
            s << "NAME" << " " << "A" << " " << endl << endl << "A" << endl;
            GrammarParserTest grammar(mptl, mpel);
            TS_ASSERT(grammar.getAST(s.str().c_str()) != NULL);
            TS_ASSERT(mptl.output.size() == 2);
            TS_ASSERT(mptl.output.at(0) == "Processing: 3, value: A\n");
            TS_ASSERT(mptl.output.at(1) == "Processing: 4, value: A\n");
            TS_ASSERT(mpel.output.size() == 0);

            mpel.output.clear();
            mptl.output.clear();

            s.str("");
            s << "NAME" << " " << "A" << " " << endl << endl << "B" << endl;
            TS_ASSERT(grammar.getAST(s.str().c_str()) == NULL);
            TS_ASSERT(mptl.output.size() == 1);
            TS_ASSERT(mptl.output.at(0) == "Processing: 3, value: A\n");
            TS_ASSERT(mpel.output.size() == 1);
            TS_ASSERT(mpel.output.at(0) == "ErrorListener: 2 at line: 3 in context \'B\n\'\n");

            mpel.output.clear();
            mptl.output.clear();

            s.str("");
            s << "NAME" << " " << "B" << " " << endl << endl << "A" << endl;
            TS_ASSERT(grammar.getAST(s.str().c_str()) == NULL);
            TS_ASSERT(mptl.output.size() == 0);
            TS_ASSERT(mpel.output.size() == 1);
            TS_ASSERT(mpel.output.at(0) == "ErrorListener: 5 at line: 1 in context \'===Erroneous line===>>> B \n\nA\n\'\n");
        }

};

#endif /*CORE_PARSERTESTSUITE_H_*/

import os
from SCons.Script import *
            
def generate(env, **kwargs):
    env.SetDefault(CXXTEST_GENERATOR = '_buildtools/cxxtest/cxxtestgen.py',
                   CXXTEST_PRINTER = 'ErrorPrinter',
                   CXXTEST_GENERATORCMDSTR = 'Generating CxxTest $SOURCES')
                    
    apply(env.Replace, (), kwargs)

    # Builder zum Erstellen eines CxxTestrunners aus TestSuites    
    GenerateCxxTestAction = Action('python $CXXTEST_GENERATOR --runner=$CXXTEST_PRINTER --have-eh -o $TARGET $SOURCES', 
                                   "$CXXTEST_GENERATORCMDSTR")
    GenerateCxxTestBuilder = SCons.Builder.Builder(action = GenerateCxxTestAction, 
                                                   suffix = '.cpp', src_suffix = '.h')
    env.Append(BUILDERS = {'GenerateCxxTest' : GenerateCxxTestBuilder})
    
def exists(env):
    return true
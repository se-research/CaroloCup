from SCons.Script import *
from subprocess import *
import os.path

def Test(ENV, MODULE, SOURCEFILES, TESTSUITES, COVERAGEREPORT, VALGRINDREPORT):
    """
    Pseudo-Builder that take source files, testsuite files and calls the
    appropriate builders to execute the testsuites and to generate a
    coverage and valgrind report.  
    """
      
    # Create, compile and link selected testsuites. 
    generatedTestPrograms = [] # list of generated testprograms for execution
    executedTestPrograms = []
    generatedValgrindReports = []
    
    if ENV['TESTING_BUILDSERVER']:
        # If the build process is running on the daily build server,
        # we generate just one testrunner for all testsuites.
        testrunner = ENV.GenerateCxxTest(os.path.join('testsuites', MODULE+'TestRunner'), TESTSUITES)
        testprogram = ENV.Program(os.path.join('testsuites', MODULE+'TestRunner'), 
                                  testrunner)
        generatedTestPrograms.append(testprogram)
        
        # Execute testcases
        valgrindFile = str(testprogram[0]) + '.valgrind'
        execution = ENV.Valgrind(valgrindFile, testprogram)
        executedTestPrograms.append(execution) 
        generatedValgrindReports.append(valgrindFile)
        
        # Rename XML Report that is only generated on build server. 
        ENV.AddPostAction(execution, 'mv TestSuiteReport.xml TestSuiteReport-' + MODULE + '.xml')
    else:                    
        for suite in TESTSUITES:
            filename, extension = os.path.splitext(suite.abspath)       
            testsuiteName = os.path.join('testsuites', filename)

            # Run the CxxTest generator
            testrunner = ENV.GenerateCxxTest(testsuiteName + 'Runner', suite) 

            # Compile testsuite. It's statical linked against the module's lib
            testprogram = ENV.Program(testsuiteName, 
                                      testrunner)
            generatedTestPrograms.append(testprogram)
            
            # Execute testsuites using valgrind
            valgrindFile = str(testprogram[0]) + '.valgrind'
            execution = ENV.Valgrind(valgrindFile, testprogram)
            executedTestPrograms.append(execution)
            generatedValgrindReports.append(valgrindFile)
        
    # Generate coverage report.
    coverage = ENV.Coverage(COVERAGEREPORT, SOURCEFILES)
    ENV.Depends(COVERAGEREPORT, executedTestPrograms)
    
    # Generate valgrind report
    memcheck = ENV.ValgrindReport(VALGRINDREPORT, generatedValgrindReports)
    ENV.Depends(VALGRINDREPORT, executedTestPrograms)
    
    generatedReports = [coverage, memcheck]

    return (generatedTestPrograms, executedTestPrograms, generatedReports)     

#******************************************************************************
def Test_DEPRECATED(ENV, MODULE, SOURCEFILES, TESTSUITES, COVERAGEREPORT, VALGRINDREPORT):
    """
    Pseudo-Builder that take source files, testsuite files and calls the
    appropriate builders to execute the testsuites and to generate a
    coverage and valgrind report.  
    """
    aliasTestsuites = MODULE + "-TESTSUITES"
    aliasTestsuitesExecution = MODULE + "-TESTSUITESEXECUTION"
     
    # First create a static library from the source files
    staticLib = ENV.StaticLibrary( MODULE, SOURCEFILES, LIBPATH='.')
    
    # Create, compile and link selected testsuites. 
    generatedTestPrograms = [] # list of generated testprograms for execution
    valgrindFiles = []
    
    if ENV['TESTING_BUILDSERVER']:
        # If the build process is running on the daily build server,
        # we generate just one testrunner for all testsuites.
        testrunner = ENV.GenerateCxxTest(os.path.join('testsuites', MODULE+'TestRunner'), TESTSUITES)
        testprogram = ENV.Program(os.path.join('testsuites', MODULE+'TestRunner'), 
                                  testrunner, 
                                  LIBS = ENV["LIBS"] + [staticLib])
        generatedTestPrograms.append(testprogram)
        ENV.Alias(aliasTestsuites, testprogram)
        
        # Execute testcases
        testexecution = ENV.Valgrind(str(testprogram[0]) + '.valgrind', testprogram) 
        valgrindFiles.append(str(testprogram[0]) + '.valgrind')
        
        # Rename XML Report that is only generated on build server. 
        ENV.AddPostAction(testexecution, 'mv TestSuiteReport.xml TestSuiteReport-' + MODULE + '.xml')
        # Alias used as a dependency for the reports
        ENV.Alias(aliasTestsuitesExecution, testexecution)
    else:
        # Let the developer choose the testsuites and generate a own 
        # testrunner
        if ENV["noGUI"]:
            selectedTestSuites = TESTSUITES
        else:
            selectedTestSuites = selectTestSuites(ENV, TESTSUITES)
            
        if (selectedTestSuites == []):
            print "Keine Testfaelle ausgewaehlt: " + MODULE
            Return()
            
        for suite in selectedTestSuites:
            filename, extension = os.path.splitext(suite.abspath)       
            testsuiteName = os.path.join('testsuites', filename)

            testrunner = ENV.GenerateCxxTest(testsuiteName + 'Runner', suite) 
                                             
            testprogram = ENV.Program(testsuiteName, 
                                      testrunner, 
                                      LIBS = ENV["LIBS"] + [staticLib] )
            generatedTestPrograms.append(testprogram)
            ENV.Alias(aliasTestsuites, testprogram)
            
            # Now execute them.
            testexecution = ENV.Valgrind(str(testprogram[0]) + '.valgrind', testprogram) 
            valgrindFiles.append(str(testprogram[0]) + '.valgrind')
            # Add executed testprograms to the alias. Coverage and valgring report
            # depend on this alias.
            ENV.Alias(aliasTestsuitesExecution, testexecution)
        
    # Generate coverage report.
    coverage = ENV.Coverage(COVERAGEREPORT, SOURCEFILES)
    ENV.Depends(COVERAGEREPORT, aliasTestsuitesExecution)
    
    # Generate valgrind report
    memcheck = ENV.ValgrindReport(VALGRINDREPORT, valgrindFiles)
    ENV.Depends(VALGRINDREPORT, aliasTestsuitesExecution)
    
    # Return an alias that causes the correct build order.
    return ENV.Alias(MODULE+'-Testing', [staticLib, ENV.Alias(aliasTestsuites), coverage, memcheck])    
    
def selectTestSuites(env, availableTestSuites):
    result = []

    # Check, if any test suites are found.
    if (len(availableTestSuites) > 0):
        # Create a string of all available test suites.
        testSuites = ""
        for testSuite in availableTestSuites:
            testSuites = testSuites + " " + testSuite.str_for_display()
            
        # Construct argument for python call.
        command = "/usr/bin/python"
        arg = GetLaunchDir() + "/_buildtools/TestSuiteComposer/TestSuiteComposer.py buildEnvironment SCons" + " " + testSuites

        # Call python with argument.
        output = Popen(command + " " + arg, shell=True, stdout=PIPE).communicate()[0]

        # Check for python's return value.
        selectedTestSuites = []
        if (len(output) > 0):
            value = output.split(os.linesep)
            for i in value:
                i.strip()
                if (len(i) > 0):
                    for testSuite in availableTestSuites:
                        if (testSuite.str_for_display().strip().strip("\"").find(i.strip()) > -1):
                            found = False
                            for j in selectedTestSuites:
                                if (j == i.strip()):
                                    found = True
                            if (found == False):
                                print "  Using testsuite '" + i.strip() + "'"
                                result.append(testSuite)
                                selectedTestSuites.append(i.strip())

    return result
    
def generate(env):
    env.SetDefault(TESTING_BUILDSERVER=False)
    env.AddMethod(Test_DEPRECATED, "Test__DEPRECATED")
    env.AddMethod(Test, "Test")
    env.AddMethod(selectTestSuites, "selectTestSuites")
   
def exists(env):
    return true


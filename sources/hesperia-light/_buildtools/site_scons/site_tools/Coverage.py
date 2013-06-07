from SCons.Script import *
import os.path


def doCoverage(env, target, source):
    reportFile = open(str(target[0]), 'w')
    
    for file in source:
        if existsGCNO(file.abspath):
            # Datei auswerten
            output =""
            gcovoutput = executeGCOV(env, file.abspath)
            if gcovoutput == []:
                output = "Coverage for " + file.name + ": 0%"
            else:
                stringForReport = analyzeGCOV(gcovoutput)
                                    
                if stringForReport == "":
                    output += "Coverage for " + file.name + ": 100%"
                else:
                    output += "Coverage for " + file.name + ":\n"
                    output += stringForReport
            
            # Auswertung ausgeben und abspeichern
            print output
            reportFile.write(output+"\n")
        
    reportFile.close()
    
def executeGCOV(env, file):
    """
    Runs the gcov command for file. The result is return as list of
    strings. If this list is empty ist most likley that the file
    is completly uncovered...
    Temporary files (generated *.gcov by call th gcov and *.gcda/*.gcno
    generate by execution of testsuites) are removed afterwards.
    """
    path, filename = os.path.split(file)
    
    # The generate gcda and gcno file contains paths relative to the
    # launch directory. Therefore gcov needs to be called in this 
    # directory. (This behaviour is caused mainly by the use of a 
    # seperate build directory) Otherwise gcov is unabled to find 
    # referenced files.
    if not existsGCDA(file):
        return []
        
    env.Execute('-@cd ' + GetLaunchDir() + ' && gcov $GCOV_OPTION -o ' + path + ' ' + file + ' > /dev/null')
        
    
    # Open the gcov output file, read lines to output and close file.   
    gcovFile = open(os.path.join(GetLaunchDir(), filename + '.gcov'), 'r')    
    output = gcovFile.readlines()
    gcovFile.close()
    
    # gcov pollutes our root directory with *.gcov files. Delete them.
    env.Execute('-@rm ' + os.path.join(GetLaunchDir(), '*.gcov'))
    # Remove the .gcda file the get a clean coverage analysis the next time.
    env.Execute('-@rm ' + os.path.splitext(file)[0] + '.gcda')  
    
    # Returns gcov's output
    return output

def existsGCDA(file):
    filename, extension = os.path.splitext(file)
    return os.path.isfile(filename +'.gcda')

def existsGCNO(file):
    filename, extension = os.path.splitext(file)
    return os.path.isfile(filename +'.gcno')

def analyzeGCOV(lines):
    """
    Analyze the output lines of gcov. Returns a string containing
    the information that is printed to the console and written to
    the report file.
    """
    
    output = ""
    for line in lines:
        if "#####" in line:
            if not "NO_COVERAGE" in line:
                output += line
    return output
    
def generate(env):
    """
    Creates a builder that runs gcov on source files. It is important
    that testsuites our similiar programs are executed before this builder.
    Otherwise there won't be any files that can by analyzed. To ensure that
    testsuites are executed before, do something like:
    
    coverage = env.Coverage("coverage.txt", SOURCE_FILES)   
    env.Depends("coverage.txt", env.Alias("ALIAS_FOR_ALL_TESTSUITES") )
    
    The GCOV_OPTION environment variable can be used to specify what kind
    of coverage report is generated by gcov. See the 'man gcov' for more
    information.     
    """
    env.SetDefault(GCOV_OPTION = '-a')
     
    action = Action(doCoverage, 'Generating coverage report')
    env.Append(BUILDERS = {'Coverage' : Builder(action = action,
                                                #emitter = CoverageEmitter,
                                                multi = 1)})
    
def exists(env):
    return env.WhereIs('gcov')

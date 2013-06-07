from SCons.Script import *
import os.path

def Valgrind(env, target, source):
    if env['valgrind']:
        error = env.Execute('@valgrind --log-file=' + str(target[0]) + ' --leak-check=full ' + str(source[0]))
    else:
        report = open(str(target[0]), 'w')
        print "Valgrind was disabled!"
        report.write("Valgrind was disabled!")
        report.close()
        error = env.Execute('DISPLAY=:0.0 ' + str(source[0])) 
    
    if error:
        print "TestSuite failed with errorcode: " + str(error)
        Exit(10)

def ValgrindReport(env, target, source):
    report = open(str(target[0]), 'w')
    
    if env['valgrind']:
        for file in source:
            filename, extension = os.path.splitext(file.abspath)
            
            output = "Valgrind-Report for " + filename +'\n'
            output += os.popen('grep -A5 "LEAK SUMMARY" ' + filename + '.valgrind' ).read()        
            output += "See " + filename + '.valgrind' + " for more information\n"
            print output
            report.write(output+"\n")
    else:
        output = "Valgrind was disabled!"
        print output
        report.write(output)
            
    report.close()
    
def generate(env):
    env.Append(BUILDERS = {'Valgrind' : Builder(action=Action(Valgrind, "Executing Testsuite $SOURCE"))})
    env.Append(BUILDERS = {'ValgrindReport' : Builder(action=Action(ValgrindReport, "Generating Valgrind report"))})
    
def exists(env):
    return env.WhereIs('valgrind')

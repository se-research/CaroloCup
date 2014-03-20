from SCons.Script import *

def configureEnvironment(env):    
    cfg = env.Configure()
   
    if not cfg.CheckLibWithHeader('GL', 'GL/gl.h', 'CXX'):
        print 'GL header and lib not found. At least GLUT 3.8.0 is necessary (libglut3-dev).'
        exit(1)
        
    if not cfg.CheckLibWithHeader('qwt-qt4', 'qwt-qt4/qwt.h', 'CXX'):
        print 'qwt5-qt4 header and lib not found. (libqwt5-qt4-dev, libqwt5-qt4).'
        exit(1)
                
    if not cfg.CheckLibWithHeader('GLU', 'GL/glu.h', 'CXX'):
        print 'GLU header and lib not found. At least GLUT 3.8.0 is necessary (libglut3-dev).'
        exit(1)
        
    if not cfg.CheckLibWithHeader('glut', 'GL/glut.h', 'CXX'):
        print 'GLUT header and lib not found. At least GLUT 3.8.0 is necessary (libglut3-dev).'
        exit(1)
        
    if not cfg.CheckLibWithHeader('libdb', 'db.h', 'CXX'):
        print 'Berkeley DB header and lib not found. At least Berkeley DB 4.6 is necessary (libdb4.6-dev).'
        exit(1)
        
    if not cfg.CheckLibWithHeader('libpthread', ['pthread.h'],  'CXX'):
        print 'pthread header and lib not found.'
        exit(1)
        
    if not cfg.CheckLibWithHeader('libzip', ['zip.h'],  'CXX'):
        print 'zip header and lib not found (libzip-dev).'
        exit(1)
        
    if not cfg.CheckLibWithHeader('libcxcore', ['opencv/cxcore.h'],  'CXX'):
        print 'OpenCV header and lib not found (libcv-dev).'
        exit(1)
        
    if not cfg.CheckLibWithHeader('libcv', ['opencv/cv.h'],  'CXX'):
        print 'OpenCV header and lib not found (libcv-dev).'
        exit(1)
        
    if not cfg.CheckLibWithHeader('libhighgui', ['opencv/highgui.h'],  'CXX'):
        print 'OpenCV highgui header and lib not found (libcv-dev).'
        exit(1)
        
    if not cfg.CheckHeader('arpa/inet.h'):
        print 'No arpa/inet.h header.'
        exit(1)
        
    if not cfg.CheckHeader('sys/socket.h'):
        print 'No sys/socket.h header.'
        exit(1)
        
    if not cfg.CheckHeader('sys/time.h'):
        print 'No sys/time.h header.'
        exit(1)

    return cfg.Finish()        
    

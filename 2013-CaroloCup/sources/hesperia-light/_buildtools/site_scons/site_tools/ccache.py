from SCons.Script import *
import sys

def generate(env):
    if sys.platform == 'win32':
        print "[ccache]: Not avaiable under win32"
    else:
        p = env.WhereIs("gcc")
        if os.path.islink(p) and os.readlink(p).endswith('ccache'): 
            print "[ccache]: using ccache via symlink %s -> %s" % (p, os.readlink(p)) 
            ccache = None 
        else: 
            ccache = env.WhereIs("ccache") 
            if ccache is None: 
                print "[ccache]: ccache not found. please install it." 
            else: 
                print "[ccache]: using ccache from %s" % (ccache,) 
    
        if ccache: 
            env['CC'] = 'ccache %s' % env['CC'] 
            env['CXX'] = 'ccache %s' % env['CXX']
        
def exists(env):
    return env.WhereIs("ccache")         
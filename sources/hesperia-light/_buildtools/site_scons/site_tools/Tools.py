import os.path
import os
import SCons.Script
import SCons.Builder
import SCons
import fnmatch

#InstallDirectory
def InstallDirectory(env, target_dir, src_dir, pattern):
    installer = InstallDirHelper(src_dir, env.Dir(src_dir).srcnode().abspath, target_dir, pattern, env)
    return env.Alias( target_dir, installer)
    
def InstallDirHelper(src_dir, src_path, target_dir, pattern, env):
    files = env.Glob(src_dir+'/'+pattern)
    installer = env.Install( target_dir, files )

    for file in os.listdir(src_path):
        srcpath = env.Dir(src_dir).srcnode().abspath + '/' + file

        if (os.path.isdir(srcpath)):  
            installer.extend( InstallDirHelper(src_dir+'/'+file, srcpath, target_dir+'/'+file, pattern, env) )
    
    return installer
    

#RecursiveGlob
#RecursiveGlob
def RecursiveGlob(env, src_dir, pattern):
    files = RecursiveGlobHelper(src_dir, env.Dir(src_dir).srcnode().abspath, pattern, env)

    return files

def RecursiveGlobHelper(src_dir, abspath, pattern, env):
    files = env.Glob(src_dir+'/'+ pattern)

    for file in os.listdir(abspath):
        srcpath = env.Dir(src_dir).srcnode().abspath + '/' + file

        if (os.path.isdir(srcpath)):  
            files.extend( RecursiveGlobHelper(src_dir+"/"+file, srcpath, pattern, env) )

    return files

#def RecursiveGlob(env, src_dir, pattern):
#    files = []
#    for root, dirs, files in os.walk(src_dir):
#        
#        files.extend( env.Glob(root + '/*.cpp') )
#    return files

#SymLink
def CreateSymLink(target, source, env):
    path = os.path.split(str(target[0]))[0]
    targetFile = os.path.split(str(target[0]))[1]
    sourceFile = os.path.split(str(source[0]))[1]
    command = 'cd %s && ln -sf %s %s' % (path, sourceFile, targetFile)
    os.system(command)
        
#SCons Tool
def generate(env):        
    symlinkAction = SCons.Action.Action(CreateSymLink, "Creating symbolic link from $SOURCE to $TARGET")
    symlinkBuilder = SCons.Builder.Builder(action = symlinkAction)
    env.Append(BUILDERS = {'SymLink' : symlinkBuilder})
    
    env.AddMethod(InstallDirectory)
    env.AddMethod(RecursiveGlob)
    
def exists(env):
    return true
        

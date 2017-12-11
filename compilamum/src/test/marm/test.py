import subprocess
import os

COMPILERPATH = '../../../target/scala-2.12/compilamum_2.12-1.0.jar'
for root, _, files in os.walk('.'):
    for f in files:
        if f.endswith('.marm'):
            name = f[:f.rfind('.')]
            mpath = root + f
            cpath = root + name + '-client.js'
            spath = root + name + '-server.js'

            compiler = subprocess.Popen(COMPILERPATH + ' ' + mpath, shell=True,
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            stdout, stderr = compiler.communicate()
            errcode = compiler.returncode

            server = subprocess.Popen('node ' + spath, shell=True,
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            serverout, servererr = server.communicate()
            errcode = server.returncode

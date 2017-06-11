import os.path

rpath=[
        Literal('\\$$ORIGIN'),
        Literal('\\$$ORIGIN/../deps'),
        Literal('\\$$ORIGIN/../../deps')
    ]

env_release = Environment(
    CXX = 'g++',
    parse_flags = "-std=c++11 -D_REENTRANT -fstack-protector -O2 -Wl,--no-undefined",
    RPATH=rpath,
    )

env_debug = Environment(
    CXX = 'g++',
    parse_flags = "-std=c++11 -D_REENTRANT -fstack-protector -O0 -g3",
    RPATH=rpath,
    )

def build(env, env_name):
    env.Append() 
    install_prefix=os.path.join('/usr/local/lib/shimmercat-build', env_name, 'artifacts')
    includes_prefix=os.path.join('/usr/local/lib/shimmercat-build', 'include')

    env.SConscript(
        'cbits/SConscript', 
        variant_dir=os.path.join('cbits_build', env_name), 
        exports=['env', 'install_prefix', 'includes_prefix'])
    env.Alias('install', install_prefix)

build(env_release, 'release')
build(env_debug, 'debug')

env.Install(includes_prefix, 'cbits/botan_all.h')
env.Install(includes_prefix, 'cbits/botan_all_internal.h')

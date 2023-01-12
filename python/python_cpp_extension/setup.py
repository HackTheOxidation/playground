from distutils.core import setup, Extension


my_module = Extension('demo',
                      sources=['demo.cpp'],
                      extra_compile_args=['-std=c++20', '-O3'],
                      language='c++')

setup(name='cpp-extension-demo',
      version='1.0',
      description='This is a demo for writing python extensions in C++',
      ext_modules=[my_module])

import os

from opengeode import ogParser

from sdl2nim import NimGenerator

if __name__ == "__main__":
    cwd = os.getcwd()

    files = ['/home/taste/tool-src/opengeode/tests/testsuite/test-forloop/og.pr']

    files = [os.path.abspath(each) for each in files]
    path = os.path.dirname(files[0])
    os.chdir(path or '.')
    ast, warnings, errors = ogParser.parse_pr(files=files)
    os.chdir(os.path.dirname(__file__)+'/out/')

    errors = NimGenerator.generate(ast.processes[0], shared=False, taste=False)

#!/usr/bin/env python
"""
Find all function calls in bindings-hdf5 and work out whether they are used in hs-hdf5.

The aim is to tabulate how much of the bindings-hdf5 API is targetted by hs-hdf5.

This is made more complicated by bindings-dsl defining a custom C/Haskell mangling scheme.
"""

import sys
import subprocess as sp
import re
from collections import defaultdict

def find_deflines(bindings_dir):
    """
    :bindings_dir: path to bindings-hdf5 repo

    """

    defs = {}

    pipe = sp.Popen('grep ccall -R {}/src'.format(bindings_dir), shell=True, stdout=sp.PIPE)
    for line in pipe.stdout:
        mo = re.match(r'(.*?):#ccall ([a-zA-Z0-9_]+)', line.decode('ascii'))
        if not mo:
            print('ERROR: {}'.format(line.strip()))
            continue
        filename, func_name = mo.groups()
        mangled = mangle(func_name)
        defs[mangled] = filename

    return defs

def find_hsdef(name):
    found = set()

    pipe = sp.Popen('grep {} -R ./src'.format(name), shell=True, stdout=sp.PIPE)
    for line in pipe.stdout:
        filename, line = line.decode('ascii').split(':', 1)
        found.add(filename)

    return found

def mangle(name):
    mo = re.match(r'(H5[A-Z]*)([a-z0-9_]+)', name)
    prefix, suffix = mo.groups()

    if suffix[0] != '_':
        suffix = '_'+suffix
    return prefix.lower() + suffix


def print_prefix_results(prefix, results):
    c = 0 ; tot = 0
    print('========\n{}'.format(prefix))
    for name, is_defined in results[prefix]:
        if is_defined:
            print('  {:30}\t[  OK  ]'.format(name))
            c += 1
        else:
            print('  {:30}\t[ FAIL ]'.format(name))
        tot += 1
    print('--------\n{} / {}'.format(c, tot))


if __name__ == '__main__':
    names = find_deflines('.')
    if len(sys.argv) > 1:
        prefixes = sys.argv[1:]
    else:
        prefixes = None

    results = defaultdict(list)

    for name in names:
        prefix = name.split('_', 1)[0]
        results[prefix].append((name, len(find_hsdef(name)) > 0))

    for prefix in results:
        if prefixes and prefix not in prefixes:
            continue
        print_prefix_results(prefix, results)

#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
impsort.py
==========
Sort python imports. Based on vim-sort-python-imports_

Links
-----
+ https://www.python.org/dev/peps/pep-0008/#imports
+ https://github.com/reddit/reddit/wiki/PythonImportGuidelines
+ https://google-styleguide.googlecode.com/svn/trunk/pyguide.html#Imports

.. _vim-sort-python-imports: https://github.com/public/vim-sort-python-imports/blob/master/plugin/sort_imports.py
"""
from __future__ import print_function

import argparse
import ast
import sys
if sys.version_info < (3, 4):
    import imp
    import pkgutil
else:
    import importlib.util
from collections import defaultdict
import sysconfig
from glob import glob1
from os import path, scandir

def _clean_ext_suffix(libname):
    # See: https://www.python.org/dev/peps/pep-3149/
    ext_suffix = sysconfig.get_config_var('EXT_SUFFIX') or '.so'
    return libname.replace(ext_suffix, '')


def _get_python_lib(standard_lib=True):
    if standard_lib:
        return sysconfig.get_path("stdlib")
    else:
        return sysconfig.get_path("purelib")


class ImpSorter(ast.NodeVisitor):
    """
    This class visits all the import nodes at the root of tree
    and generates new import nodes that are sorted according to the Google
    and PEP8 coding guidelines.

    In practice this means that they are sorted according to this tuple.

        (stdlib, site_packages, names)

    We also make sure only 1 name is imported per import statement.
    """

    def __init__(self):
        self.original_nodes = []
        self.imports = set()
        self.from_imports = defaultdict(set)
        self.stdlibs = set(self.iter_stdmodules()) | set(self.get_dynlibs()) | set(sys.builtin_module_names)
        self.python_paths = [p for p in sys.path if p]

    def visit_Import(self, node):
        if node.col_offset != 0:
            return
        self.imports.update((nm.name, nm.asname) for nm in node.names)
        self.original_nodes.append(node)

    def visit_ImportFrom(self, node):
        if node.col_offset != 0:
            return
        # we need to group the names imported from each module
        # into single from X import N,M,P,... groups so we store the names
        # and regenerate the node when we find more
        # we'll then insert this into the full imports chain when we're done
        self.from_imports[(node.level, node.module)].update(
            (nm.name, nm.asname) for nm in node.names
        )
        self.original_nodes.append(node)

    @staticmethod
    def get_dynlibs():
        dirname = path.join(sysconfig.get_path("stdlib"), "lib-dynload")
        dynlibs = glob1(dirname, '*.so')
        return map(_clean_ext_suffix, dynlibs)

    @staticmethod
    def _iter_stdmodules_deprecated():
        stdlib_path = _get_python_lib(standard_lib=True)
        importer = pkgutil.ImpImporter(stdlib_path)
        return (m for m, _ in importer.iter_modules())

    @staticmethod
    def _iter_stdmodules_new():
        # note this misses some edge cases that the original ImpImporter covered, i.e., namespace packages
        # but works for the majority of common packages
        stdlib_path = sysconfig.get_path('stdlib')
        for entry in scandir(stdlib_path):
            if entry.is_file() and entry.name.endswith('.py'):
                module_name = path.splitext(entry.name)[0]
                if module_name != '__init__':
                    yield module_name
            elif entry.is_dir():
                init_file = path.join(entry.path, '__init__.py')
                if path.isfile(init_file):
                    yield entry.name

        # Check for frozen modules
        for module_name in sys.builtin_module_names:
            if importlib.util.find_spec(module_name) is not None:
                yield module_name

    @staticmethod
    def iter_stdmodules():
        if sys.version_info < (3, 4):
            return ImpSorter._iter_stdmodules_deprecated()
        else:
            return ImpSorter._iter_stdmodules_new()

    def _is_thirdparty_deprecated(self, modname):
        try:
            imp.find_module(modname, self.python_paths)
            thirdparty = True
        except ImportError:
            thirdparty = False
        return thirdparty

    def _is_thirdparty_new(self, modname):
        for path in self.python_paths:
            spec = importlib.util.find_spec(modname, path)
            if spec is not None:
                return True
        return False

    def is_thirdparty(self, modname):
        # for older python
        if sys.version_info < (3, 4):
            return self._is_thirdparty_deprecated(modname)
        # newer python (imp package doesn't exist in python 3.12+)
        else:
            return self._is_thirdparty_new(modname)

    # :: Node -> Key
    def _node_sort_key(self, node):
        """
        Given an AST node return a tuple which is used for sorting.
        """
        non_future = non_stdlib = non_thirdparty = True
        relative = False
        if isinstance(node, ast.Import):
            name = [node.names[0].name, node.names[0].asname]
            level = 0
            from_names = None
            fromimport = 0
        elif isinstance(node, ast.ImportFrom):
            name = [node.module]
            level = node.level
            from_names = [nm.name for nm in node.names]
            fromimport = 1
        else:
            raise TypeError(node)
        modname = name[0].split('.')[0] if name[0] else ''
        if level != 0:
            relative = True
        elif modname == '__future__':
            non_future = False
        elif modname in self.stdlibs:
            non_stdlib = False
        elif self.is_thirdparty(modname):
            non_thirdparty = False
        return (non_future, non_stdlib, non_thirdparty, relative, level, fromimport, name, from_names)

    def new_nodes(self):
        """
        Generate a list of tuples with the form `(Key, Node)`.
        """
        nodes = []
        for (level, module), names in self.from_imports.items():
            for nm, asnm in sorted(names):
                node = ast.ImportFrom(
                    module=module,
                    names=[ast.alias(name=nm, asname=asnm)],
                    level=level
                )
                nodes.append((self._node_sort_key(node), node))
        for nm, asnm in self.imports:
            node = ast.Import(names=[ast.alias(name=nm, asname=asnm)])
            nodes.append((self._node_sort_key(node), node))
        return nodes

    def write_sorted(self, file=sys.stdout):
        """
        Write sorted imports to file.

        file: a file-like object (stream).
        """
        pkey = None
        for key, node in sorted(self.new_nodes()):
            # insert new lines between groups
            if pkey and key[:4] != pkey[:4]:
                print(u'', file=file)
            pkey = key

            # names here will actually always only have 1 element in it
            # because we are only allowed 1 per line, but it's easy
            # enough to cope with multiple anyway.
            all_names = ', '.join(
                (' as '.join(nm for nm in (name.name, name.asname) if nm))
                for name in node.names
            )

            if isinstance(node, ast.Import):
                print(u'import {0}'.format(all_names), file=file)
            elif isinstance(node, ast.ImportFrom):
                print(u'from {0}{1} import {2}'.format('.' * node.level, node.module or '', all_names), file=file)


def main():
    parser = argparse.ArgumentParser(description="Python sort imports.")
    parser.add_argument('infile', nargs='?', type=argparse.FileType('r'),
                        default=sys.stdin)
    parser.add_argument('outfile', nargs='?', type=argparse.FileType('w'),
                        default=sys.stdout)

    args = parser.parse_args()
    with args.infile as infile, args.outfile as outfile:
        tree = ast.parse(infile.read())
        i = ImpSorter()
        i.visit(tree)
        i.write_sorted(outfile)


if __name__ == '__main__':
    main()

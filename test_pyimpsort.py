# -*- coding: utf-8 -*-

import ast
import io

import pytest

from pyimpsort import ImpSorter


@pytest.mark.parametrize('infname,outfname', [
    ('samples/1.in', 'samples/1.out'),
    ('samples/2.in', 'samples/2.out'),
    ('samples/3.in', 'samples/3.out'),
    ('samples/4.in', 'samples/4.out'),
    ('samples/5.in', 'samples/5.out'),
    ('samples/6.in', 'samples/6.out'),
    ('samples/7.in', 'samples/7.out'),
    ('samples/8.in', 'samples/8.out'),
    ('samples/9.in', 'samples/9.out'),
    ('samples/10.in', 'samples/10.out'),
])
def test_sort_import(infname, outfname):
    with io.open(infname) as fin, io.open(outfname) as fout, io.StringIO() as res:
        tree = ast.parse(fin.read())
        i = ImpSorter()
        i.visit(tree)
        i.write_sorted(res)
        assert res.getvalue().split('\n') == fout.read().split('\n')

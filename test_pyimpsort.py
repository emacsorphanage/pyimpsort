# -*- coding: utf-8 -*-
import ast
import io
from dataclasses import dataclass

import pytest

from pyimpsort import ImpSorter


@dataclass
class TestCase:
    infname: str
    outfname: str
    group: bool = False


@pytest.mark.parametrize(
    "test_case",
    [
        TestCase("samples/1.in", "samples/1.out"),
        TestCase("samples/2.in", "samples/2.out"),
        TestCase("samples/3.in", "samples/3.out"),
        TestCase("samples/4.in", "samples/4.out"),
        TestCase("samples/5.in", "samples/5.out"),
        TestCase("samples/6.in", "samples/6.out"),
        TestCase("samples/7.in", "samples/7.out"),
        TestCase("samples/8.in", "samples/8.out"),
        TestCase("samples/9.in", "samples/9.out"),
        TestCase("samples/10.in", "samples/10.out"),
        TestCase("samples/11.in", "samples/11.out", group=True),
    ],
)
def test_sort_import(test_case):
    with io.open(test_case.infname) as fin, io.open(test_case.outfname) as fout, io.StringIO() as res:
        tree = ast.parse(fin.read())
        i = ImpSorter()
        i.visit(tree)
        i.write_sorted(res, test_case.group)
        assert res.getvalue().split("\n") == fout.read().split("\n")

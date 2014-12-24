#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup


setup(
    name='impsort',
    version='0.1',
    description='Sort python imports.',
    py_modules=['impsort'],
    license='MIT',
    entry_points="""
        [console_scripts]
        impsort=impsort:main
    """,
    classifiers=(
        "Development Status :: 4 - Beta",
        'Intended Audience :: Developers',
        'Natural Language :: English',
        'License :: OSI Approved :: MIT',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Environment :: Console',
    )
)

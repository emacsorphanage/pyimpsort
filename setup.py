#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup


setup(
    name='pyimpsort',
    version='0.0.1',
    description='Sort python imports.',
    py_modules=['pyimpsort'],
    license='MIT',
    entry_points={
        "console_scripts": [
            "pyimpsort = pyimpsort:main"
        ]
    },
    classifiers=[
        "Development Status :: 4 - Beta",
        'Intended Audience :: Developers',
        'Natural Language :: English',
        'License :: OSI Approved :: MIT',
        'Programming Language :: Python',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
        'Programming Language :: Python :: 3.12',
        'Programming Language :: Python :: 3.13',
        'Environment :: Console',
    ]
)

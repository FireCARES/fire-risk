#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup
import re
import os
from setuptools import find_packages


name = 'fire-risk'
package = 'fire_risk'
description = 'Numerical Models for Developing Community Scale Risk Model'
long_description = open('README.md').read()
url = 'https://github.com/FireCARES/fire-risk'
author = 'Austin Anderson, Tyler Garner, Andrew Kurzawski, Craig Weinschenk'
author_email = 'AustinDAnderson@gmail.com, garnertb@prominentedge.com, AJKurzawski@gmail.com'
license = 'MIT'


def get_version(package):
    """
    Return package version as listed in `__version__` in `init.py`.
    """
    init_py = open(os.path.join(package, '__init__.py')).read()
    return re.search("^__version__ = ['\"]([^'\"]+)['\"]", init_py, re.MULTILINE).group(1)


def get_packages(package):
    """
    Return root package and all sub-packages.
    """
    return [dirpath
            for dirpath, dirnames, filenames in os.walk(package)
            if os.path.exists(os.path.join(dirpath, '__init__.py'))]


def get_package_data(package):
    """
    Return all files under the root package, that are not in a
    package themselves.
    """
    walk = [(dirpath.replace(package + os.sep, '', 1), filenames)
            for dirpath, dirnames, filenames in os.walk(package)
            if not os.path.exists(os.path.join(dirpath, '__init__.py'))]

    filepaths = []
    for base, filenames in walk:
        filepaths.extend([os.path.join(base, filename)
                          for filename in filenames])
    return {package: filepaths}


setup(
    name=name,
    version=get_version(package),
    url=url,
    license=license,
    description=description,
    long_description=long_description,
    author=author,
    author_email=author_email,
    packages=find_packages(exclude=["tests.*", "tests"]),
    package_data=get_package_data(package),
    test_suite='tests',
    install_requires=[
        'matplotlib==1.4.3',
        'numpy==1.10.4',
        'pandas==0.16.0',
        'psycopg2==2.7',
        'click==4.0',
        'pytest==2.7.0',
        'pytest-cov==2.5.1',
        'flake8==2.2.5',
        'scipy==0.17.0',
        'mock==2.0.0'
    ],
    zip_safe=False
)

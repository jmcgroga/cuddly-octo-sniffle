#!/usr/bin/env python

import os
import io
import tarfile
from tarfile import TarInfo
from string import Template
from datetime import datetime

PACKAGE_NAME="servicenow"
AUTHOR="James McGrogan"
AUTHOR_EMAIL="jmcgroga@gmail.com"
REQUIRES="(js2-mode) (yasnippet)"
KEYWORDS="servicenow"
URL="https://www.jamesmcgrogan.com/"
COMMENTARY=""";;; This is a library of ServiceNow utility functions"""
YEAR=datetime.now().strftime('%Y')
VERSION="1.0.0"
DESCRIPTION="Servicenow utilities."
SPLIT_VERSION=list(map(int, VERSION.split('.')))
MTIME=int(datetime.now().replace(month=1,
                                 day=SPLIT_VERSION[0],
                                 hour=SPLIT_VERSION[1],
                                 minute=SPLIT_VERSION[2],
                                 second=0).strftime('%s'))

def get_package_dir():
    return '%s-%s' % (PACKAGE_NAME, VERSION)

def add_dir(t, *args):
    dirname = os.path.join(*args)
    tarinfo = TarInfo(dirname)
    tarinfo.type = tarfile.DIRTYPE
    tarinfo.mode = 0o755
    tarinfo.mtime = MTIME
    t.addfile(tarinfo)
    return dirname

def add_file(t, dirname, filename, infile, encoding='UTF-8'):
    tarinfo = TarInfo(os.path.join(dirname, filename))
    infile.seek(0, os.SEEK_END)
    tarinfo.size = infile.tell()
    tarinfo.mtime = MTIME
    infile.seek(0)
    t.addfile(tarinfo, infile)

def add_package_template(t):
    template_file = 'package-template.el'
    package_filename = '%s-pkg.el' % (PACKAGE_NAME)
    package_dir = get_package_dir()
    with open(template_file, 'r') as infile:
        package_file_content = Template(infile.read()).substitute(PACKAGE_NAME=PACKAGE_NAME,
                                                                  AUTHOR=AUTHOR,
                                                                  AUTHOR_EMAIL=AUTHOR_EMAIL,
                                                                  REQUIRES=REQUIRES,
                                                                  KEYWORDS=KEYWORDS,
                                                                  URL=URL,
                                                                  COMMENTARY=COMMENTARY,
                                                                  YEAR=YEAR,
                                                                  VERSION=VERSION,
                                                                  DESCRIPTION=DESCRIPTION)
        targetdir = add_dir(t, package_dir)
        add_file(t, targetdir, package_filename, io.BytesIO(bytes(package_file_content.encode('UTF-8'))))


def add_subdirectory(t, dirname):
    package_dir = get_package_dir()
    for root, dirs, files in os.walk(dirname):
        targetdir = os.path.join(package_dir, root)
        for fn in files:
            add_file(t, targetdir, fn, open(os.path.join(root, fn), 'rb'))

def add_el_files(t):
    package_dir = get_package_dir()
    for fn in os.listdir():
        if fn.endswith('.el') and fn != 'package-template.el':
            add_file(t, package_dir, fn, open(fn, 'rb'))

def main():
    package_filename = '%s-%s.tar' % (PACKAGE_NAME, VERSION)

    t = tarfile.open(package_filename, 'w')
    add_package_template(t)
    add_el_files(t)
    add_subdirectory(t, 'snippets')

if __name__ == '__main__':
    main()

    

#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import argparse
import logging
import traceback

import opengeode
from . import NimGenerator, utils

LOG = logging.getLogger(__name__)

__all__ = ['sdl2nim']
__version__ = '0.1'



def parse_args():
    """
    Parse cmdline arguments
    """
    parser = argparse.ArgumentParser()
    parser.add_argument('-v', '--version', action='version',
                        version=__version__)
    parser.add_argument('--output-dir', dest='output_dir', action='store', type=str,
                        metavar='path', default='.', help='set output directory (default .)')
    parser.add_argument('--shared', action='store_true', default=False,
                        help='Generate getters/setters to access internal state')
    parser.add_argument('--taste', dest='taste_target', action='store_true',
                        help='Generate code for TASTE targets')
    parser.add_argument('files', metavar='file.pr', type=str, nargs='*',
                        help='SDL file(s)')
    return parser.parse_args()


def sdl2nim() -> int:

    LOG.setLevel(logging.INFO)
    options = parse_args()

    # parsing SDL/PR with opengeode
    try:
        ast, warnings, errors = opengeode.parse(options.files)
    except IOError as err:
        LOG.error('Aborting due to I/O error')
        LOG.error(str(err))
        return 1
    if len(ast.processes) != 1:
        LOG.error(f'Found {len(ast.processes)} process(es), one is expected')
        return 1
    if errors:
        return 1

    # generate IF code
    try:
        NimGenerator.generate(ast.processes[0],
                              simu=options.shared,
                              taste=options.taste_target,
                              options=vars(options))
    except (TypeError, ValueError, NameError) as err:
        err = str(err).replace(u'\u00fc', '.')
        err = err.encode('utf-8')
        LOG.error(str(err))
        LOG.error(str(traceback.format_exc()))
        LOG.error('Nim code generation failed')
        return 1

    return 0


if __name__ == "__main__":

    args = parse_args()

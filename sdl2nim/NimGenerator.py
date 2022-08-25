import inspect
from multiprocessing.spawn import import_main_path
import re

import logging
import opengeode

from functools import singledispatch
from opengeode import ogAST, Helper

from .utils import not_implemented_error
from .Expressions import expression

from typing import List, Tuple

LOG = logging.getLogger(__name__)

__all__ = ['generate']





@singledispatch
def generate(*args, **kwargs) -> Tuple:
    """ Generate the code for an item of the AST
        Base-case; should not occur
    """
    raise TypeError('Incorrect, unsupported or missing data in model AST')
    return [], []

@generate.register(ogAST.Process)
def _process(process, simu=False, instance=False, taste=False, **kwargs):
    not_implemented_error()

@generate.register(ogAST.Output)
@generate.register(ogAST.ProcedureCall)
def _call_external_function(output, **kwargs) -> str:
    not_implemented_error()

@generate.register(ogAST.TaskAssign)
def _task_assign(task, **kwargs):
    not_implemented_error()

@generate.register(ogAST.TaskInformalText)
def _task_informal_text(task, **kwargs):
    not_implemented_error()

@generate.register(ogAST.TaskForLoop)
def _task_forloop(task, **kwargs):
    not_implemented_error()

@generate.register(ogAST.Decision)
def _decision(dec, branch_to=None, sep='if ', last='end if;', exitcalls=[], **kwargs):
    not_implemented_error()

@generate.register(ogAST.Label)
def _label(lab, **kwargs):
    not_implemented_error()

@generate.register(ogAST.Transition)
def _transition(tr, **kwargs):
    not_implemented_error()

@generate.register(ogAST.Floating_label)
def _floating_label(label, **kwargs):
    not_implemented_error()

@generate.register(ogAST.Procedure)
def _inner_procedure(proc, **kwargs):
    not_implemented_error()
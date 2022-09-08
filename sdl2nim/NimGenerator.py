import inspect
from multiprocessing.spawn import import_main_path
import re

import logging
import opengeode

from functools import singledispatch
from opengeode import ogAST, Helper

from .utils import not_implemented_error, var_exists, is_local, is_numeric, type_name, child_spelling, traceability
from .Expressions import (VARIABLES,
                          LOCAL_VAR,
                          TYPES,
                          PROCEDURES,
                          SEPARATOR,
                          LPREFIX)
from .Expressions import expression, array_content, find_basic_type, string_payload

from typing import List, Tuple

LOG = logging.getLogger(__name__)

__all__ = ['generate']


def write_statement(param, newline):
    ''' Generate the code for the special "write" operator '''
    code = []
    string = ''
    local = []
    basic_type = find_basic_type(param.exprType) or {}
    type_kind = basic_type.kind
    if isinstance(param, ogAST.ExprAppend):
        # Append: call Put_Line separately for each side of the expression
        st1, _, lcl1 = write_statement(param.left, newline=False)
        st2, _, lcl2 = write_statement(param.right, newline=False)
        code.extend(st1)
        code.extend(st2)
        local.extend(lcl1)
        local.extend(lcl2)
    elif type_kind == 'IA5StringType':
        # IA5String are null-terminated to match the C representation
        # ASN1SCC API offers the getStringSize function to read the actual size
        code, string, local = expression(param, readonly=1)
        code.append(f'stdout.write ({string})')
    elif type_kind.endswith('StringType'):
        if isinstance(param, ogAST.PrimOctetStringLiteral):
            # Octet string or bit string
            code.append(f'stdout.write ("{param.printable_string}")')
        elif isinstance(param, ogAST.PrimStringLiteral):
            # Raw string
            # First remove the newline statements and handle escaping
            text = param.value[1:-1]
            text = text.replace("\\'", "'").replace('\\"', '"')
            text = text.replace('"', '""').split('\n')
            for idx, val in enumerate(text):
                code.append(f'stdout.write ("{val}")')
                if len(text) > 1 and idx < len(text) - 1:
                    code.append(r'stdout.write "\n"')
        else:
            code, string, local = expression(param, readonly=1)
            if type_kind == 'OctetStringType':
                # Octet string -> convert to Ada string
                last_it = ""
                if isinstance(param, ogAST.PrimSubstring):
                    range_str = f"{string}'Range"
                    iterator = f"i - {string}'First + 1"
                elif basic_type.Min == basic_type.Max:
                    range_str = f"{string}.Data'Range"
                    string += ".Data"
                    iterator = "i"
                else:
                    range_str = f"1 .. {string}.Length"
                    string += ".Data"
                    iterator = "i"
                    last_it = f"({range_str})"
                code.extend([f"for i in {range_str} loop",
                             f"Put (Character'Val({string}(i)));",
                             "end loop;"])
            else:
                code.append(f"stdout.write ({string})")
    elif type_kind in ('IntegerType', 'RealType',
                       'BooleanType', 'Integer32Type', 'IntegerU8Type'):
        code, string, local = expression(param, readonly=1)
        if hasattr(param, "expected_type"):
            cast = type_name(param.expected_type)
        else:
            cast = type_name(param.exprType)
        code.append(f"stdout.write (({string}).{cast})")
    elif type_kind == 'EnumeratedType':
        code, string, local = expression(param, readonly=1)
        # code.append(f"Put ({type_name(param.exprType)}'Image ({string}));")
        # enumerated must be variable, so we can use 'Img
        code.append(f"stdout.write ({string})")
    else:
        error = ('Unsupported parameter in write call ' +
                 param.inputString + '(type kind: ' + type_kind + ')')
        LOG.error(error)
        raise TypeError(error)
    if newline:
        code.append(r'stdout.write "\n"')
    return code, string, local


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
    ''' A list of assignments in a task symbol '''
    code, local_decl = [], []
    if task.comment:
        code.extend(traceability(task.comment))
    for expr in task.elems:
        code.extend(traceability(expr))
        # ExprAssign only returns code statements, no string
        try:
            code_assign, _, decl_assign = expression(expr)
        except TypeError as err:
            raise TypeError(f"{str(err)} - TaskAssign: '{task.inputString}' (please report this bug)")
        code.extend(code_assign)
        local_decl.extend(decl_assign)
    return code, local_decl


@generate.register(ogAST.TaskInformalText)
def _task_informal_text(task, **kwargs):
    ''' Generate Nim comments for informal text '''
    code = []
    if task.comment:
        code.extend(traceability(task.comment))
    code.extend(['### ' + text.replace('\n', '\n-- ') for text in task.elems])
    return code, []


@generate.register(ogAST.TaskForLoop)
def _task_forloop(task, **kwargs):
    '''
            Return the code corresponding to a for loop. Two forms are possible:
            for x in range ([start], stop [, step])
            for x in iterable (a SEQUENCE OF)
        '''
    stmt, local_decl = [], []
    local_scope = dict(LOCAL_VAR)
    if task.comment:
        stmt.extend(traceability(task.comment))
    stmt.extend(traceability(task))
    for loop in task.elems:
        if loop['range']:
            start_str, stop_str = '0.asn1SccUint', ''

            if loop['range']['start']:
                basic = find_basic_type(loop['range']['start'].exprType)
                start_stmt, start_str, start_local = \
                    expression(loop['range']['start'])

                if not is_numeric(start_str):
                    start_str = f"({start_str}).asn1SccUint"

                local_decl.extend(start_local)
                stmt.extend(start_stmt)

            if loop['range']['step'] == 1:
                start_str += ' ..< '

            basic = find_basic_type(loop['range']['stop'].exprType)
            stop_stmt, stop_str, stop_local = expression(loop['range']['stop'])

            if not is_numeric(stop_str):
                stop_str = f"({stop_str}).asn1SccUint"

            local_decl.extend(stop_local)
            stmt.extend(stop_stmt)
            if loop['range']['step'] == 1:
                stmt.append('for {it} in {start}{stop}:'
                            .format(it=loop['var'],
                                    start=start_str,
                                    stop=stop_str))
            else:
                step_str = f"({loop['range']['step']}).asn1SccUint"
                stmt.append('for {it} in countup({start}, {stop}, {step}):'
                            .format(it=loop['var'],
                                    start=start_str,
                                    stop=stop_str,
                                    step=step_str))
            # Add iterator to the list of local variables
            LOCAL_VAR.update({loop['var']: (loop['type'], None)})
        else:
            # case of form: FOR x in SEQUENCE OF
            # Add iterator to the list of local variables
            LOCAL_VAR.update({loop['var']: (loop['type'], None)})

            list_stmt, list_str, list_local = expression(loop['list'])
            basic_type = find_basic_type(loop['list'].exprType)
            list_payload = list_str + string_payload(loop['list'], list_str)

            stmt.extend(list_stmt)
            local_decl.extend(list_local)
            stmt.extend([f'for {loop["var"]}_idx, {loop["var"]}_val in {list_payload}:',])
        try:
            code_trans, local_trans = generate(loop['transition'])
            if local_trans:
                stmt.extend(set(local_trans))
            stmt.extend(code_trans)
        except AttributeError:
            stmt.append('discard')
    # Restore list of local variables
    LOCAL_VAR.clear()
    LOCAL_VAR.update(local_scope)
    return stmt, local_decl


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

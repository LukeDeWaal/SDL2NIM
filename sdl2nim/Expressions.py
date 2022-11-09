import inspect
import re

import logging
import sys

import opengeode

from functools import singledispatch
from opengeode import ogAST, Helper
from opengeode.Helper import find_basic_type as __find_basic_type

from . import settings

from .utils import not_implemented_error, var_exists, is_local, is_numeric, child_spelling, ia5string_raw
from .utils import string_payload as __string_payload
from .utils import array_content as __array_content
from .utils import type_name as __type_name
from .utils import external_ri_list as __external_ri_list
from .utils import procedure_header as __procedure_header

from typing import List, Tuple

LOG = logging.getLogger(__name__)

__all__ = ['expression',
           'string_payload',
           'array_content',
           'find_basic_type',
           'find_basic_type',
           'string_payload',
           'array_content',
           'type_name',
           'append_size',
           'external_ri_list',
           'procedure_header']


def find_basic_type(a_type):
    return __find_basic_type(settings.TYPES, a_type)


def string_payload(prim, nim_string):
    return __string_payload(prim, nim_string, settings.TYPES)


def array_content(prim, values, asnty, pad_zeros=False):
    return __array_content(prim, values, asnty, pad_zeros)


def type_name(a_type, use_prefix=True):
    return __type_name(a_type, use_prefix=use_prefix, prefix=settings.ASN1SCC)


def external_ri_list(process):
    return __external_ri_list(process, settings.SEPARATOR, settings.ASN1SCC)


def procedure_header(proc):
    return __procedure_header(proc, settings.SEPARATOR)


@singledispatch
def expression(expr):
    ''' Generate the code for Expression-classes, returning 3 things:
        - list of statements
        - useable string corresponding to the evaluation of the expression,
        - list of local declarations
    '''
    raise TypeError('Unsupported expression: ' + str(expr))
    return [], '', []


@expression.register(ogAST.PrimVariable)
def _primary_variable(prim, **kwargs):
    exists = var_exists(prim.value[0].lower(), settings.VARIABLES)
    if (not exists) or is_local(prim.value[0], settings.LOCAL_VAR):  # Local Variable Access
        sep = ''
    else:  # Global Variable Access
        sep = settings.LPREFIX + '.'

    # No special action required yet
    nim_string = f'{sep}{prim.value[0].lower()}'

    return [], str(nim_string), []

@expression.register(ogAST.PrimFPAR)
def _prim_fpar(prim, **kwargs):
    return [], f'{prim.value[0]}[]', []

@expression.register(ogAST.PrimCall)
def _prim_call(prim, **kwargs):
    ''' Cover all built-in functions and inner procedures with RETURN stmt '''
    stmts, nim_string, local_decl = [], '', []

    func = prim.value[0].lower()
    params = prim.value[1]['procParams']


    if func in ('abs', 'fix', 'float', 'chr',):
        is_unsigned = (float(find_basic_type(params[0].exprType).Min) >= 0)

        param_stmts, param_str, local_var = expression(params[0], readonly=1)
        stmts.extend(param_stmts)
        local_decl.extend(local_var)

        if func == 'abs':
            if is_unsigned:
                nim_string += f'({param_str})'
            else:
                nim_string += f'abs({param_str})'
        elif func == 'chr':
            nim_string += f'({param_str}).byte'
        elif func == 'float':
            nim_string += f'({param_str}).asn1Real'
        elif func == 'fix':
            if is_unsigned:
                nim_string += f'({param_str}).asn1SccUint'
            else:
                nim_string += f'({param_str}).asn1SccSint'

    elif func in ('sin', 'cos', 'sqrt', 'round', 'ceil', 'floor', 'trunc'):
        exp = params[0]

        basic_type = find_basic_type(exp.exprType)
        needs_cast = False
        if basic_type.kind != 'RealType':
            needs_cast = True

        param_stmts, param_str, local_var = expression(exp, readonly=1)
        stmts.extend(param_stmts)
        local_decl.extend(local_var)

        if needs_cast:
            param_str = f'({param_str}).asn1Real'

        nim_string += f"{func}({param_str})"

    elif func in ('shift_left', 'shift_right'):
        p1, p2 = params
        param_stmts, s1, local_var = expression(p1, readonly=1)
        stmts.extend(param_stmts)
        local_decl.extend(local_var)
        param_stmts, s2, local_var = expression(p2, readonly=1)
        stmts.extend(param_stmts)
        local_decl.extend(local_var)

        fcn = 'shl' if func == "shift_left" else 'shr'
        nim_string += f'(({s1}) {fcn} ({s2}))'

    elif func == 'power':
        operands = [None, None]
        for idx, param in enumerate(params):
            stmt, operands[idx], local = expression(param, readonly=1)
            stmts.extend(stmt)
            local_decl.extend(local)

        base_type = find_basic_type(params[0].exprType)
        expn_type = find_basic_type(params[1].exprType)

        if expn_type.kind.startswith('Integer') and float(expn_type.Min) >= 0:
            # use ^ operator (need positive integer exponent)
            nim_string += f'(({operands[0]}) ^ ({operands[1]}))'
        else:
            # use pow operator
            nim_string += f'pow(({operands[0]}),({operands[1]}))'

    elif func == 'length':
        exp = params[0]
        exp_type = find_basic_type(exp.exprType)
        min_length = getattr(exp_type, 'Min', None)
        max_length = getattr(exp_type, 'Max', None)
        if min_length is None or max_length is None:
            error = f'{exp.inputString} is not a SEQUENCE OF'
            LOG.error(error)
            raise TypeError(error)
        param_stmts, param_str, local_var = expression(exp, readonly=1)

        stmts.extend(param_stmts)
        local_decl.extend(local_var)
        if min_length == max_length and not isinstance(exp, ogAST.PrimSubstring):
            nim_string += min_length  # f"len({param_str}.arr)"
        else:
            nim_string += f"({param_str}).nCount"

    elif func == 'num':
        exp = params[0]

        basic_type = find_basic_type(exp.exprType)
        if not basic_type.kind == 'EnumeratedType':
            error = f'{exp.inputString} is not an ENUM'
            LOG.error(error)
            raise TypeError(error)

        param_stmts, param_str, local_var = expression(exp, readonly=1)
        stmts.extend(param_stmts)
        local_decl.extend(local_var)

        nim_string += f"(num({param_str})).{type_name(prim.exprType)}"

    elif func == 'exist':
        exp = params[0]

        param_stmts, param_str, local_var = expression(exp, readonly=1)
        stmts.extend(param_stmts)
        local_decl.extend(local_var)

        tmp = param_str.split('.')
        tmp.insert(-1, 'exist')
        nim_string += '.'.join(tmp)
        nim_string = f"({nim_string}).{type_name(prim.exprType)}"

    elif func == 'present':
        exp = params[0]

        param_stmts, param_str, local_var = expression(exp, readonly=1)
        stmts.extend(param_stmts)
        local_decl.extend(local_var)

        base_type = type_name(exp.exprType, use_prefix=False) # type_name(prim.exprType, use_prefix=False)
        selection_type = type_name(prim.exprType, use_prefix=False)
        if not selection_type:
            # Hacky way to find proper type when type cannot easily be deduced by parser, eg in decision
            for k, v in settings.TYPES.items():
                if prim.exprType == v.type:
                    selection_type = k.replace("-","_")
                    break

        bty = find_basic_type(prim.exprType)
        local_decl.append(f"var tmp{prim.tmpVar}: {settings.ASN1SCC}{settings.PROCESS_NAME}_{selection_type}")
        stmts.extend([
            f"case {param_str}.kind:",
        ])
        for k, v in bty.EnumValues.items():
            stmts.extend([
                f"of {base_type}_{v.EnumID}:",
                f"tmp{prim.tmpVar} = {settings.PROCESS_NAME.capitalize()}_{selection_type}_{v.EnumID}"
            ])
        stmts.extend([
            "else:", stmts[-1], "# end case"
        ])

        nim_string += f"tmp{prim.tmpVar}"

    elif func == 'val':
        operands = [None, None]
        for idx, param in enumerate(params):
            stmt, operands[idx], local = expression(param, readonly=1)
            stmts.extend(stmt)
            local_decl.extend(local)

        numb_type = find_basic_type(params[0].exprType)
        enum_type = params[1].value[0]

        nim_string = f"{operands[0]}.{settings.ASN1SCC}{enum_type}"

    elif func == 'choice_to_int':
        p1, p2 = params
        sort = find_basic_type(p1.exprType)
        assert (sort.kind == 'ChoiceType')  # normally checked by the parser
        param_stmts, varstr, local_var = expression(p1, readonly=1)
        stmts.extend(param_stmts)
        local_decl.extend(local_var)
        param_stmts, defaultstr, local_var = expression(p2, readonly=1)
        stmts.extend(param_stmts)
        local_decl.extend(local_var)
        local_decl.append(f'var tmp{prim.tmpVar}: {settings.ASN1SCC}Sint')


        choices = [f'case {varstr}.kind:']
        # all choice elements must be either signed or unsigned
        # a mix would result in inconsistencies
        # therefore we have to cast to signed as if there is at least one
        # signed element (with the risk of cutting very big values)
        has_unsigned = False
        has_signed = False
        for each in sort.Children.values():
            child_sort = find_basic_type(each.type)
            if child_sort.kind.startswith('Integer'):
                if float(child_sort.Min) < 0.0:
                    has_signed = True
                else:
                    has_unsigned = True

        base = type_name(p1.exprType, use_prefix=False)
        need_cast = has_signed and has_unsigned
        for child_name, descr in sort.Children.items():
            child_name_nim = child_name.replace('-', '_')
            child_id = descr.EnumID
            child_sort = find_basic_type(descr.type)
            if not child_sort.kind.startswith('Integer'):
                continue
            set_value = f'{varstr}.u.{child_name_nim}'
            if need_cast and float(child_sort.Min) >= 0.0:
                set_value = f'{settings.ASN1SCC}Sint({set_value})'
            choices.extend([f'of {base}_{child_id}:', f'tmp{prim.tmpVar} = {set_value}'])

        choices.extend([f'else:', f'tmp{prim.tmpVar} = {defaultstr}', '# end case'])
        stmts.extend(choices)
        nim_string += f'tmp{prim.tmpVar}'

    elif func in ('to_enum', 'to_selector'):
        variable, target_type = params
        var_typename = type_name (variable.exprType, use_prefix=False)

        var_bty = find_basic_type(variable.exprType)
        var_stmts, var_str, var_decl = expression (variable, readonly=1)
        stmts.extend(var_stmts)
        local_decl.extend(var_decl)
        destSort = target_type.value[0].replace('-','_')
        nim_typename = f"{settings.ASN1SCC}{destSort}"

        if func == 'to_enum':
            assert var_typename.endswith('selection')
            local_decl.append(f'var tmp{prim.tmpVar}: {nim_typename}')
        elif func == 'to_selector':
            local_decl.append(f'var tmp{prim.tmpVar}: {settings.ASN1SCC}{settings.PROCESS_NAME}_{destSort}_selection')

        try:
            nimtype = settings.TYPES[destSort.replace('_', '-')].type
            if func == 'to_enum':
                assert nimtype.EnumValues.keys() == var_bty.EnumValues.keys()
            elif func == 'to_selector':
                assert nimtype.Children.keys() == var_bty.EnumValues.keys()
        except (KeyError, AttributeError, TypeError, AssertionError) as err:
            raise TypeError(f"{str(err)} - PrimCall: '{prim.inputString}' (please report this bug)")

        stmts.extend([f'case {var_str}:'])
        for child_name, descr in var_bty.EnumValues.items():
            child_name_nim = child_name.replace('-', '_')
            child_id = descr.EnumID

            if func == 'to_enum':
                set_value = f'{nim_typename}_{child_name_nim}'
                stmts.extend([f'of {settings.PROCESS_NAME.capitalize()}_{var_typename}_{child_id}:', f'tmp{prim.tmpVar} = {set_value}'])
            elif func == 'to_selector':
                set_value = f'{settings.PROCESS_NAME}_{destSort}_selection_{child_name_nim}_present'.capitalize()
                stmts.extend([f'of {var_typename}_{child_id}:', f'tmp{prim.tmpVar} = {set_value}'])

        stmts.extend([f'else:', stmts[-1], '# end case'])
        nim_string = f'tmp{prim.tmpVar}'

    elif func in ('observer_status',):
        # TODO
        pass

    else:
        # inner procedure call (with a RETURN statement)
        # TODO
        # retrieve the procedure signature
        p, = [p for p in settings.PROCEDURES if p.inputString.lower() == func.lower()]

        # for inner procedures we do not use a temporary variable because
        # we remain in Ada and therefore in parameters do not need to
        # be pointers (in out).
        nim_string += f'p{settings.SEPARATOR}{func}('
        # Take all params and join them with commas
        list_of_params = []
        for idx, param in enumerate(params):
            # Expected basic type of the parameter
            param_type = p.fpar[idx]['type']
            basic_param = find_basic_type(param_type)

            param_stmt, param_str, local_var = expression(param, readonly=1)

            # We need to format strings properly, this depends on the expected
            # type of the procedure parameter
            if isinstance(param,
                          (ogAST.PrimSequenceOf, ogAST.PrimStringLiteral)):
                if basic_param.kind == 'IA5StringType':
                    param_str = ia5string_raw(param)
                elif basic_param.kind.startswith('Integer'):
                    param_str = str(param.numeric_value)
                else:
                    param_str = array_content(param, param_str, basic_param)

            elif isinstance(param, ogAST.PrimVariable):
                param_str = f'addr {param_str}'

            list_of_params.append(param_str)
            stmts.extend(param_stmt)
            local_decl.extend(local_var)
        nim_string += ', '.join(list_of_params)
        nim_string += ')'

    return stmts, str(nim_string), local_decl



@expression.register(ogAST.PrimIndex)
def _prim_index(prim, **kwargs):
    stmts, nim_string, local_decl = [], '', []
    ro = kwargs.get("readonly", 0)
    receiver = prim.value[0]

    receiver_stms, nim_string, receiver_decl = expression(receiver, readonly=ro)
    stmts.extend(receiver_stms)
    local_decl.extend(receiver_decl)

    lbty = find_basic_type(prim.value[0].exprType)
    if lbty.kind in ('SequenceOfType', 'OctetStringType'):
        nim_string += '.arr'

    index = prim.value[1]['index'][0]
    idx_stmts, idx_string, idx_var = expression(index, readonly=ro)
    if str.isnumeric(idx_string):
        idx_string = int(idx_string)
    else:
        idx_string = f'({idx_string}).asn1SccUint'
    nim_string += f'[{idx_string}]'
    stmts.extend(idx_stmts)
    local_decl.extend(idx_var)

    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimSubstring)
def _prim_substring(prim, **kwargs):
    ''' Generate expression for SEQOF/OCT.STRING substrings, e.g. foo(1,2) '''
    stmts, nim_string, local_decl = [], '', []
    ro = kwargs.get("readonly", 0)

    receiver = prim.value[0]
    receiver_stms, receiver_string, receiver_decl = expression(receiver,
                                                               readonly=ro)
    nim_string = receiver_string
    stmts.extend(receiver_stms)
    local_decl.extend(receiver_decl)

    r1_stmts, r1_string, r1_local = expression(prim.value[1]['substring'][0],
                                               readonly=ro)
    r2_stmts, r2_string, r2_local = expression(prim.value[1]['substring'][1],
                                               readonly=ro)

    # SDL starts indexes at 0, ASN1 Nim types at 0
    # SDL ends at final - 1, Nim at final, thus change range from .. to ..<
    if str.isnumeric(r1_string):
        r1_string = str(int(r1_string))
    else:
        r1_string = f"({r1_string}).asn1SccUint"
    if str.isnumeric(r2_string):
        r2_string = str(int(r2_string))
    else:
        r2_string = f"({r2_string}).asn1SccUint"

    stmts.extend(r1_stmts)
    stmts.extend(r2_stmts)
    local_decl.extend(r1_local)
    local_decl.extend(r2_local)
    local_decl.extend([
        f"var tmp{prim.tmpVar}: {type_name(prim.exprType)}"
    ])
    stmts.extend([
        f"tmp{prim.tmpVar}.nCount = ({r2_string} - {r1_string}).cint",
        f"tmp{prim.tmpVar}.arr[0 ..< tmp{prim.tmpVar}.nCount] = {receiver_string}.arr[{r1_string} ..< {r2_string}]"
    ])
    nim_string = f"tmp{prim.tmpVar}"

    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimSelector)
def _prim_selector(prim, **kwargs):
    ''' Selector (field access with '!' or '.' separation) '''
    stmts, nim_string, local_decl = [], '', []
    ro = kwargs.get("readonly", 0)

    receiver = prim.value[0]  # can be a PrimSelector
    field_name = prim.value[1]

    receiver_stms, receiver_string, receiver_decl = expression(receiver,
                                                               readonly=ro)
    nim_string = receiver_string
    stmts.extend(receiver_stms)
    local_decl.extend(receiver_decl)

    receiver_bty = find_basic_type(receiver.exprType)

    if receiver_bty.kind == 'ChoiceType':
        nim_string = f'{nim_string}.u.{field_name}'
    else:
        # SEQUENCE, check for field optionality first
        child = child_spelling(field_name, receiver_bty)
        if receiver_bty.Children[child].Optional == 'True' \
                and not kwargs.get("readonly", 0):
            # Must set Exist only when assigning value, not each time it is
            # accessed: this is what "readonly" ensures.
            stmts.append(f'{nim_string}.exist.{field_name} = 1')
        nim_string += '.' + field_name

    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimStateReference)
def _primary_state_reference(prim, **kwargs):
    ''' Reference to the current state '''
    return [], f'{settings.LPREFIX}.state', []


@expression.register(ogAST.ExprPlus)
@expression.register(ogAST.ExprMul)
@expression.register(ogAST.ExprMinus)
@expression.register(ogAST.ExprGt)
@expression.register(ogAST.ExprGe)
@expression.register(ogAST.ExprLt)
@expression.register(ogAST.ExprLe)
@expression.register(ogAST.ExprDiv)
@expression.register(ogAST.ExprMod)
@expression.register(ogAST.ExprRem)
def _basic_operators(expr, **kwargs):
    code, nim_string, local_decl = [], '', []

    left_stmts, left_str, left_local = expression(expr.left, readonly=1)
    right_stmts, right_str, right_local = expression(expr.right, readonly=1)

    # Check if either side is a literal number
    right_is_numeric = is_numeric(right_str)
    left_is_numeric = is_numeric(left_str)

    lbty = find_basic_type(expr.left.exprType)
    rbty = find_basic_type(expr.right.exprType)

    ltype = type_name(expr.left.exprType)
    rtype = type_name(expr.right.exprType)

    if lbty.kind.startswith('Integer') and rbty.kind.startswith('Integer') and expr.operand == '/':
        operand = 'div'
    elif expr.operand == 'rem':
        operand = 'mod'
    else:
        operand = expr.operand

    if lbty.kind.startswith('Integer') and \
            isinstance(expr.right, (ogAST.PrimOctetStringLiteral,
                                    ogAST.PrimBitStringLiteral)):
        right_str = str(expr.right.numeric_value)

    if rbty.kind.startswith('Integer') and \
            isinstance(expr.left, (ogAST.PrimOctetStringLiteral,
                                   ogAST.PrimBitStringLiteral)):
        left_str = str(expr.left.numeric_value)

    if left_is_numeric and right_is_numeric:
        # Both sides are literals : compute the result on the fly
        nim_string = "{}".format(eval("{left} {op} {right}"
                                      .format(left=left_str,
                                              op=operand,
                                              right=right_str)))

    elif rbty.kind != lbty.kind:
        # Basic types are different (one is an Integer32, eg. loop iterator)
        # => We must cast it to the type of the other side
        if lbty.kind == 'Integer32Type':
            left_str = f'({left_str}).{type_name(expr.right.exprType)}'
        else:
            right_str = f'({right_str}).{type_name(expr.left.exprType)}'
        nim_string = f'({left_str} {operand} {right_str})'

    elif ltype != rtype:
        cast = ltype
        nim_string = '({left} {op} ({right}).{cast})'.format(left=left_str,
                                                             op=operand,
                                                             right=right_str,
                                                             cast=cast)

    else:
        nim_string = '({left} {op} {right})'.format(left=left_str,
                                                    op=operand,
                                                    right=right_str)

    code.extend(left_stmts)
    code.extend(right_stmts)
    local_decl.extend(left_local)
    local_decl.extend(right_local)
    return code, str(nim_string), local_decl


@expression.register(ogAST.ExprEq)
@expression.register(ogAST.ExprNeq)
def _equality(expr, **kwargs):
    code, left_str, local_decl = expression(expr.left, readonly=1)
    right_stmts, right_str, right_local = expression(expr.right, readonly=1)

    code.extend(right_stmts)
    local_decl.extend(right_local)

    asn1_type = getattr(expr.left.exprType, 'ReferencedTypeName', None)
    actual_type = type_name(expr.left.exprType)

    lbty = find_basic_type(expr.left.exprType)
    rbty = find_basic_type(expr.right.exprType)

    eq, neq = False, False
    if expr.operand == '=':
        operand = '=='
        eq = True
    else:
        operand = '!='
        neq = True

    basic = lbty.kind in ('IntegerType',
                          'Integer32Type',
                          'IntegerU8Type',
                          'BooleanType',
                          'EnumeratedType',
                          'ChoiceEnumeratedType')

    nim_string = ''

    if basic:
        if isinstance(expr.right, (ogAST.PrimBitStringLiteral,
                                   ogAST.PrimOctetStringLiteral)):
            right_str = str(expr.right.numeric_value)
        # Cast in case a side is using a 32bits ints (eg when using Length(..))
        if lbty.kind == 'IntegerType' and rbty.kind != lbty.kind:
            right_str = f'({right_str}).{type_name(lbty)}'
        elif rbty.kind == 'IntegerType' and lbty.kind != rbty.kind:
            left_str = f'({left_str}).{type_name(rbty)}'
        nim_string = f'({left_str} {operand} {right_str})'
    else:
        if lbty.kind.startswith('Real') or rbty.kind.startswith('Real'):
            nim_string = f'abs({left_str} - {right_str}) {"<=" if eq else ">"} 1e-5'

        elif lbty.kind == 'SequenceOfType' and rbty.kind == lbty.kind:
            if expr.left.is_raw and expr.right.is_raw:
                nim_string = str(left_str == right_str)
            elif not (expr.left.is_raw or expr.right.is_raw):
                if lbty.Min == lbty.Max and rbty.Min == rbty.Max and lbty.Min == rbty.Min:
                    nim_string = f'{left_str} {operand} {right_str}'
                elif lbty.Min != lbty.Max and rbty.Min != rbty.Max and lbty.Min == rbty.Min and lbty.Max == rbty.Max:
                    nim_string = f'({left_str}.nCount {operand} {right_str}.nCount) {"and" if eq else "or"} ({left_str}.arr[0 ..< {left_str}.nCount] {operand} {right_str}.arr[0 ..< {right_str}.nCount] )'
                else:
                    raise ValueError("Unexpected Values")
            else:
                local_decl.extend([
                    f"var tmp{expr.tmpVar}: %s"
                ])
                if expr.left.is_raw:
                    local_decl[-1] = local_decl[-1] % type_name(expr.right.exprType)
                    code.extend([
                        f"tmp{expr.tmpVar}.nCount = {len(left_str.split(','))}",
                        f"tmp{expr.tmpVar}.arr[0 ..< {len(left_str.split(','))}] = {array_content(expr.left, left_str, rbty)}"
                    ])
                    nim_string = f"((tmp{expr.tmpVar}.nCount == {right_str}.nCount) and (tmp{expr.tmpVar}.arr == {right_str}.arr))"
                elif expr.right.is_raw:
                    local_decl[-1] = local_decl[-1] % type_name(expr.left.exprType)
                    code.extend([
                        f"tmp{expr.tmpVar}.nCount = {len(right_str.split(','))}",
                        f"tmp{expr.tmpVar}.arr[0 ..< {len(right_str.split(','))}] = {array_content(expr.right, right_str, lbty)}"
                    ])
                    nim_string = f"((tmp{expr.tmpVar}.nCount == {left_str}.nCount) and (tmp{expr.tmpVar}.arr == {left_str}.arr))"

        elif isinstance(expr.right, ogAST.PrimStringLiteral) ^ isinstance(expr.left, ogAST.PrimStringLiteral):
            if isinstance(expr.right, ogAST.PrimStringLiteral):
                # Hacky Way of determining cast to char or byte
                if isinstance(expr.left, ogAST.PrimSelector):
                    kind = find_basic_type(find_basic_type(expr.left.value[0].exprType).Children['s'].type).kind
                    if kind.startswith('IA5'):
                        right_str = ', '.join([f"{s}.char" for s in right_str.split(', ')])
                    elif kind.startswith('Octet'):
                        right_str = ', '.join([f"{s}.byte" for s in right_str.split(', ')])

                elif lbty.kind == 'OctetStringType':
                    left_str = f"{left_str}.arr"
                    right_str = ', '.join([f"{s}.byte" for s in right_str.split(', ')])

                nim_string = f"{left_str}[0 ..< {len(right_str.split(','))}] {operand} [{right_str}]"
            else:
                # Hacky Way of determining cast to char or byte
                if isinstance(expr.right, ogAST.PrimSelector):
                    kind = find_basic_type(find_basic_type(expr.right.value[0].exprType).Children['s'].type).kind
                    if kind.startswith('IA5'):
                        left_str = ', '.join([f"{s}.char" for s in left_str.split(', ')])
                    elif kind.startswith('Octet'):
                        left_str = ', '.join([f"{s}.byte" for s in left_str.split(', ')])

                nim_string = f"{right_str}[0 ..< {len(left_str.split(','))}] {operand} [{left_str}]"

        elif isinstance(expr.right, ogAST.PrimStringLiteral) and isinstance(expr.left, ogAST.PrimStringLiteral):
            nim_string = f'{left_str} {operand} {right_str}'

        else:
            print(f"Cannot Compare Types: {lbty.kind} and {rbty.kind}", file=sys.stderr)  # TODO

    return code, str(nim_string), local_decl


@expression.register(ogAST.ExprAssign)
def _assign_expression(expr, **kwargs):
    code, local_decl = [], []
    strings = []
    left_stmts, left_str, left_local = expression(expr.left)
    right_stmts, right_str, right_local = expression(expr.right, readonly=1)

    basic_left = find_basic_type(expr.left.exprType)
    basic_right = find_basic_type(expr.right.exprType)

    # if isinstance(expr.left, ogAST.PrimFPAR):
    #     left_str = f"{left_str}[]"
    #
    # if isinstance(expr.right, ogAST.PrimFPAR):
    #     right_str = f"{right_str}[]"

    if (basic_left.kind == 'IA5StringType') and isinstance(expr.right, ogAST.PrimStringLiteral):
        # TODO
        pass


    elif basic_left.kind in ('SequenceOfType', 'OctetStringType', 'BitStringType'):
        rlen = f"{right_str}.len"
        # left_str += '.arr'
        if isinstance(expr.right, ogAST.PrimSubstring):
            if not isinstance(expr.left, ogAST.PrimSubstring):
                # only if left is not a substring, otherwise syntax
                # would be wrong due to result of _prim_substring
                if basic_left.kind == basic_right.kind == 'OctetStringType':
                    strings.append(f"{left_str} = {right_str}")
                    rlen = ''
                else:
                    strings.append(f"{left_str}.arr[0 ..< {right_str}.len] = {right_str}")
            else:
                # left is substring: no length, direct assignment
                rlen = ""
                strings.append(f"{left_str} = {right_str}")

        elif isinstance(expr.right, ogAST.ExprAppend):
            if (left_str in right_str) or (not right_str.startswith('tmp')):
                rlen = append_size(expr.right)
                strings.append("{lvar}.arr[0 ..< {lstr}] = {rvar}".format(lvar=left_str,
                                                                          rvar=right_str,
                                                                          lstr=rlen))
            else:
                rlen = f"tmp{expr.right.tmpVar}.nCount"
                strings.append("{lvar}.arr[0 ..< {lstr}] = {rvar}".format(lvar=left_str,
                                                                          rvar=right_str,
                                                                          lstr=rlen))

        elif isinstance(expr.right, (ogAST.PrimSequenceOf,
                                     ogAST.PrimStringLiteral)):
            if not isinstance(expr.left, ogAST.PrimSubstring):
                content = array_content(expr.right, right_str, basic_left)
                if basic_left.Min != basic_left.Max:
                    if basic_left.kind == 'OctetStringType':
                        newlen = len(right_str.split(","))
                    else:
                        newlen = len(expr.right.value)
                    strings.extend([
                        f"{left_str}.nCount = ({newlen}).cint",
                        f"{left_str}.arr[0 ..< {left_str}.nCount] = {content}"])
                else:
                    strings.extend([
                        f"{left_str}.arr[0 ..< {len(content.split(','))}] = {content}"])
            else:
                # left is substring: no length, direct assignment
                strings.append(f"{left_str} = ({right_str})")

            rlen = None

        elif isinstance(expr.right, (ogAST.ExprNot, )):
            if not expr.right.expr.is_raw:
                content = array_content(expr.right.expr, right_str, basic_left)
            else:
                content = right_str
            if basic_left.Min != basic_left.Max:
                if expr.right.expr.is_raw: newlen = len(expr.right.expr.value)
                else: newlen = f"{right_str}.nCount"
                strings.extend([
                    f"{left_str}.nCount = ({newlen}).cint",
                    f"{left_str}.arr[0 ..< {left_str}.nCount] = {content}"]
                )
            else:
                strings.extend([
                    f"{left_str}.arr[0 ..< {len(content.split(','))}] = ({content})"])
            rlen = None

        elif isinstance(expr.right, ogAST.PrimEmptyString):
            strings.append(f"{left_str} = {type_name(expr.right.exprType)}()")
            rlen = None
        else:
            # Right part is a variable
            strings.append(f"{left_str} = {right_str}")
            rlen = None
        if rlen and basic_left.Min != basic_left.Max:
            strings.append(f"{left_str}.nCount = {rlen}")

    elif basic_left.kind.startswith('Integer') and isinstance(expr.right,
                                                              (ogAST.PrimOctetStringLiteral,
                                                               ogAST.PrimBitStringLiteral)):
        # If right is an octet string or bit string literal, use the numerical
        # value directly.
        right_str = str(expr.right.numeric_value)
        strings.append(f"{left_str} = {right_str}")

    elif basic_left.kind.startswith('Integer'):
        # Integers should be compatible by default (opengeode parser checks this).
        # Casting rhs to lhs should thus always be safe
        basic_right = find_basic_type(expr.right.exprType)
        cast_left, cast_right = type_name(basic_left), type_name(basic_right)
        if cast_left != cast_right:
            res = f'({right_str}).{cast_left}'
        else:
            if hasattr(expr.right, "expected_type") \
                    and expr.right.expected_type is not None:

                cast_expected = type_name(expr.right.expected_type)
                if cast_expected != cast_left:
                    res = f'({right_str}).{cast_left}'
                else:
                    res = right_str
            else:
                res = right_str

        strings.append(f"{left_str} = {res}")

    elif basic_left.kind in ('SequenceType', 'ChoiceType'):
        # No direct assignment, only assigning members separately
        # First we assign empty object (to zero all memory)
        # Then the member assignments happen
        if right_str:
            code = [f"{left_str} = {right_str}"] + code
        right_stmts = [rstmt % left_str if '%' in rstmt else rstmt for rstmt in right_stmts]

    else:
        strings.append(f"{left_str} = {right_str}")

    code.extend(left_stmts)
    code.extend(right_stmts)
    code.extend(strings)
    local_decl.extend(left_local)
    local_decl.extend(right_local)
    return code, '', local_decl


@expression.register(ogAST.ExprOr)
@expression.register(ogAST.ExprAnd)
@expression.register(ogAST.ExprXor)
@expression.register(ogAST.ExprImplies)
def _bitwise_operators(expr, **kwargs):
    code, local_decl = [], []
    nim_string = ""

    left_stmts, left_str, left_local = expression(expr.left, readonly=1)
    right_stmts, right_str, right_local = expression(expr.right, readonly=1)

    basic_type = find_basic_type(expr.exprType)
    left_bty = find_basic_type(expr.left.exprType)
    right_bty = find_basic_type(expr.right.exprType)

    if hasattr(expr, 'operand'):
        operand = expr.operand
        if left_bty.kind.startswith('Integer') and right_bty.kind.startswith('Integer'):
            operand = f"bit{operand}"
    else:
        operand = '=>'

    if basic_type.kind != 'BooleanType':

        if left_bty.kind.startswith('Integer') and right_bty.kind.startswith('Integer') and not expr.right.is_raw:
            # left and right are numbers
            nim_string = f'{operand}({left_str}, {right_str})'

        elif expr.right.is_raw:
            if left_bty.kind.startswith('Integer'):
                # right is raw (e.g. hex string literal) and left is a number
                if isinstance(expr.right, (ogAST.PrimBitStringLiteral, ogAST.PrimOctetStringLiteral)):
                    right_payload = str(expr.right.numeric_value)
                else:
                    right_payload = right_str

                left_payload = left_str  # + string_payload(expr.left, left_str)
                nim_string = f'{operand}({left_payload}, {right_payload})'

            else:
                # right is a raw value (hex/bit string)
                # right cannot be an integer here (it would need to be converted
                # to an hex string for bitwise operations to work against
                # a sequence of / bit string
                # Declare a temporary variable to store the raw value
                tmp_string = f'tmp{expr.right.tmpVar}'

                if isinstance(expr.right,
                              (ogAST.PrimSequenceOf,
                               ogAST.PrimStringLiteral)):
                    right_str = array_content(expr.right, right_str, basic_type)

                local_decl.append(f'const {tmp_string} : {type_name(expr.right.exprType)} = {right_str}')
                # code.append(f'{tmp_string} := {right_str}')

                right_str = tmp_string
                right_payload = right_str

        elif left_bty.kind.startswith('SequenceOf') and left_bty.kind == right_bty.kind:
            _cast = type_name(
                find_basic_type(expr.exprType).type)  # TODO: Feels hacky, probably there's a cleaner way to do this?
            local_decl.extend([
                f"var tmp_bool_{expr.tmpVar}: {type_name(expr.left.exprType)}",
                f"var tmp_idx_{expr.tmpVar}: {settings.ASN1SCC}Sint"
            ])
            code.extend([
                f"for tmp_idx_{expr.tmpVar} in 0 ..< len({left_str}.arr):",
                f"tmp_bool_{expr.tmpVar}.arr[tmp_idx_{expr.tmpVar}] = ({left_str}.arr[tmp_idx_{expr.tmpVar}] {operand} {right_str}.arr[tmp_idx_{expr.tmpVar}]).{_cast}",
                "# end for"
            ])

            nim_string = f"tmp_bool_{expr.tmpVar}"

        else:
            left_payload = left_str + string_payload(expr.left, left_str)
            right_payload = right_str + string_payload(expr.right, right_str)

            if isinstance(expr, ogAST.ExprImplies):
                nim_string = f'(( not ({left_payload})) or ({right_payload}))'
            else:
                nim_string = f'({left_payload} {operand} {right_payload})'

    elif isinstance(expr, ogAST.ExprImplies):
        nim_string = f'({left_str} {operand} {right_str})'
    else:
        nim_string = f'({left_str} {operand}{expr.shortcircuit} {right_str})'

    code.extend(left_stmts)
    code.extend(right_stmts)
    local_decl.extend(left_local)
    local_decl.extend(right_local)
    return code, str(nim_string), local_decl


@expression.register(ogAST.ExprNot)
def _not_expression(expr, **kwargs):
    ''' Generate the code for a not expression '''
    code, local_decl = [], []
    if isinstance(expr.expr, ogAST.PrimSequenceOf):
        # Raw sequence of boolean (e.g. not "{true, false}") -> flip values
        for each in expr.expr.value:
            each.value[0] = 'true' if each.value[0] == 'false' else 'false'

    expr_stmts, expr_str, expr_local = expression(expr.expr, readonly=1)

    bty_inner = find_basic_type(expr.expr.exprType)
    bty_outer = find_basic_type(expr.exprType)

    if (bty_outer.kind != 'BooleanType') and ("Integer" not in bty_outer.kind):
        if bty_outer.kind.startswith('SequenceOf'):
            if expr.expr.is_raw:
                nim_string = array_content(expr.expr, expr_str, bty_outer)
            else:
                tp = type_name(expr.exprType)
                local_decl.extend([
                    f"var tmp_{tp}_{expr.tmpVar}: {tp}",
                    f"var tmp_idx_{expr.tmpVar}: asn1SccSint"
                ])
                loop_range = f"0 ..< {settings.LPREFIX}.{expr.expr.value[0]}.nCount" if bty_outer.Min != bty_outer.Max \
                        else f"0 ..< len({settings.LPREFIX}.{expr.expr.value[0]}.arr)"

                if bty_inner.Min != bty_inner.Max:
                    lencode = f"tmp_{tp}_{expr.tmpVar}.nCount = {settings.LPREFIX}.{expr.expr.value[0]}.nCount"
                else:
                    lencode = ''
                if lencode: code.append(lencode)
                code.extend([
                    f"for tmp_idx_{expr.tmpVar} in {loop_range}:",
                    f"tmp_{tp}_{expr.tmpVar}.arr[tmp_idx_{expr.tmpVar}] = not {settings.LPREFIX}.{expr.expr.value[0]}.arr[tmp_idx_{expr.tmpVar}]",
                    "# end loop"
                ])

                nim_string = f"tmp_{tp}_{expr.tmpVar}"
        else:
            raise TypeError("Unexpected Type in ExprNot")

        # TODO
    else:
        nim_string = f'(not ({expr_str}))'

    code.extend(expr_stmts)
    local_decl.extend(expr_local)
    return code, str(nim_string), local_decl


@expression.register(ogAST.ExprNeg)
def _neg_expression(expr, **kwargs):
    ''' Generate the code for a negative expression '''
    code, local_decl = [], []
    expr_stmts, expr_str, expr_local = expression(expr.expr, readonly=1)
    cast = type_name(find_basic_type(expr.exprType))
    if not is_numeric(expr_str):
        nim_string = '(-({expr}.{cast}))'.format(cast=cast, expr=expr_str)
    else:
        nim_string = '(-{expr})'.format(expr=expr_str)
    code.extend(expr_stmts)
    local_decl.extend(expr_local)
    return code, str(nim_string), local_decl


@expression.register(ogAST.ExprAppend)
def _append(expr, **kwargs):
    ''' Generate code for the APPEND construct: a // b '''
    stmts, nim_string, local_decl = [], '', []
    left_stmts, left_str, left_local = expression(expr.left, readonly=1)
    right_stmts, right_str, right_local = expression(expr.right, readonly=1)
    stmts.extend(left_stmts)
    stmts.extend(right_stmts)
    local_decl.extend(left_local)
    local_decl.extend(right_local)

    lbtype = find_basic_type(expr.left.exprType)
    rbtype = find_basic_type(expr.right.exprType)

    right_self_standing = False
    left_self_standing = isinstance(expr.left,
                                    (ogAST.PrimVariable, ogAST.PrimConstant, ogAST.PrimSubstring))

    left = '{}{}'.format(left_str,
                         string_payload(expr.left, left_str)
                         if left_self_standing else '')

    if isinstance(expr.right, (ogAST.PrimVariable,
                               ogAST.PrimConditional,
                               ogAST.PrimConstant)):
        payload = string_payload(expr.right, right_str)
        right_self_standing = True
    else:
        payload = ''

    if isinstance(expr.right, ogAST.PrimSubstring):
        right_self_standing = True

    right = '{}{}'.format(right_str, payload)

    try:
        name_of_type = type_name(expr.expected_type)
    except (NotImplementedError, AttributeError):
        name_of_type = type_name(expr.left.exprType)

    left_arr = array_content(expr.left, left_str, lbtype)
    right_arr = array_content(expr.right, right_str, rbtype)

    if not (right_self_standing or left_self_standing):
        local_decl.extend([
            f"var tmp{expr.tmpVar}: {name_of_type}"
        ])

        if isinstance(expr.left, ogAST.PrimSequenceOf) and isinstance(expr.right, ogAST.PrimSequenceOf):
            size = len(expr.left.value) + len(expr.right.value)
            concat = [
                f"tmp{expr.tmpVar}.arr[0 ..< {size}] = [{left_arr[1:-1]}, {right_arr[1:-1]}]"
            ]
        elif isinstance(expr.left, ogAST.PrimSequenceOf) and not isinstance(expr.right, ogAST.PrimSequenceOf):
            size = f"tmp{expr.right.tmpVar}.nCount + {len(expr.left.value)}"
            concat = [
                f"tmp{expr.tmpVar}.arr[0 ..< {len(expr.left.value)}] = {left_arr}",
                f"tmp{expr.tmpVar}.arr[{len(expr.left.value)} ..< {len(expr.left.value)} + tmp{expr.right.tmpVar}.nCount] = tmp{expr.right.tmpVar}.arr[0 ..< tmp{expr.right.tmpVar}.nCount]"
            ]
        elif not isinstance(expr.left, ogAST.PrimSequenceOf) and isinstance(expr.right, ogAST.PrimSequenceOf):
            size = f"tmp{expr.left.tmpVar}.nCount + {len(expr.right.value)}"
            concat = [
                f"tmp{expr.tmpVar}.arr[0 ..< tmp{expr.left.tmpVar}.nCount] = tmp{expr.left.tmpVar}.arr[0 ..< tmp{expr.left.tmpVar}.nCount]",
                f"tmp{expr.tmpVar}.arr[tmp{expr.left.tmpVar}.nCount ..< tmp{expr.left.tmpVar}.nCount + {len(expr.right.value)}] = {right_arr}"
            ]
        else:
            raise ValueError("Expected at least one SeqOf")

        stmts.append(f"tmp{expr.tmpVar}.nCount += {size}")
        stmts.extend(concat)
        nim_string = f"tmp{expr.tmpVar}.arr[0 ..< {size}]"

    elif right_self_standing and left_self_standing:
        nim_string = f"({left_str} // {right_str}).arr[0 ..< {left_str}.nCount + {right_str}.nCount]"

    else:
        local_decl.extend([
            f"var tmp{expr.tmpVar}: {name_of_type}"
        ])
        if right_self_standing and not isinstance(expr.left, ogAST.ExprAppend):
            if isinstance(expr.left, ogAST.PrimStringLiteral):
                val = expr.left.value.strip("'\"")
            else:
                val = expr.left.value
            stmts.extend([
                f"tmp{expr.tmpVar}.nCount = {len(val)}.cint",
                f"tmp{expr.tmpVar}.arr[0 ..< {len(val)}] = {left_arr}",
            ])
            nim_string = f"(tmp{expr.tmpVar} // {right_str}).arr[0 ..< tmp{expr.tmpVar}.nCount + {right_str}.nCount]"

        elif left_self_standing and not isinstance(expr.right, ogAST.ExprAppend):
            if isinstance(expr.right, ogAST.PrimStringLiteral):
                val = expr.right.value.strip("'\"")
            else:
                val = expr.right.value
            stmts.extend([
                f"tmp{expr.tmpVar}.nCount = {len(val)}.cint",
                f"tmp{expr.tmpVar}.arr[0 ..< {len(val)}] = {right_arr}",
            ])
            nim_string = f"({left_str} // tmp{expr.tmpVar}).arr[0 ..< {left_str}.nCount + tmp{expr.tmpVar}.nCount]"

        else:
            if isinstance(expr.left, ogAST.ExprAppend):
                left_str = left_str.split(".arr")[0]
            elif isinstance(expr.right, ogAST.ExprAppend):
                right_str = right_str.split(".arr")[0]
            else:
                raise TypeError("Unexpected Input")
            nim_string = f"({left_str} // {right_str}).arr[0 ..< {append_size(expr)}]"

    return stmts, str(nim_string), local_decl


@expression.register(ogAST.ExprIn)
def _expr_in(expr, **kwargs):
    ''' IN expressions: check if item is in a SEQUENCE OF '''
    stmts, local_decl = [], []
    nim_string = ""

    left_stmts, left_str, left_local = expression(expr.left, readonly=1)
    right_stmts, right_str, right_local = expression(expr.right, readonly=1)

    local_decl.extend(left_local)
    local_decl.extend(right_local)

    lbty = find_basic_type(expr.left.exprType)
    rbty = find_basic_type(expr.right.exprType)

    # it is possible to test against a raw sequence of: x in { 1,2,3 }
    # in that case we create an array on the type of x, and we test
    # presence using the form "x in array[N,type_x]"
    if isinstance(expr.left, ogAST.PrimSequenceOf):
        sort = type_name(expr.right.exprType)
        size = expr.left.exprType.Max

        if isinstance(expr.left.value[0], ogAST.PrimSequence):
            local_decl.append(f'var tmp{expr.tmpVar} : array[ {size} , {sort} ] = [{left_str}]')
            c = 0
            for idx, val in enumerate(expr.left.value):
                sz = len(val.value.keys())
                for i in range(sz):
                    left_stmts[c] = left_stmts[c] % f"tmp{expr.tmpVar}[{idx}]"
                    c += 1
            nim_string = f"({right_str} in tmp{expr.tmpVar})"
        else:

            arr = array_content(expr.left, left_str, lbty)
            nim_string = f'( {right_str} in {arr} )'
            # local_decl.append(f'const tmp{expr.tmpVar} : array[ {size} , {sort} ] = [{left_str}]')

        stmts.extend(left_stmts)
        stmts.extend(right_stmts)

    else:

        stmts.extend(left_stmts)
        stmts.extend(right_stmts)

        if lbty.kind.startswith('SequenceOf'):
            nim_string = f'{right_str} in {left_str}.arr'
        else:
            # Might not be needed anymore;
            local_decl.append(f'var tmp{expr.tmpVar} : asn1bool = false')
            nim_string = f'tmp{expr.tmpVar}'

            # stmts.append(f"in_loop_{nim_string}:")
            left_type = find_basic_type(expr.left.exprType)

            len_str = f"len({left_str}.arr)"

            if left_type.Min == left_type.Max:
                stmts.append(f"for idx in 0 ..< {len_str}:")
            else:
                stmts.append(f"for idx in 0 ..< {left_str}.nCount:")

            stmts.append(f"{nim_string} = ({left_str}[idx] == {right_str})")

            # stmts.append(f"{nim_string} == true")

            stmts.append(f"if {nim_string} == true:")
            stmts.append("break")
            stmts.append("# end if")

            stmts.append("# end loop")

    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimEnumeratedValue)
def _enumerated_value(primary, **kwargs):
    ''' Generate code for an enumerated value '''
    enumerant = primary.value[0].replace('-', '_').lower()
    basic = find_basic_type(primary.exprType)
    for each in basic.EnumValues:
        if each.lower() == enumerant:
            break
    # no "asn1Scc" prefix if the enumerated is a choice selector
    use_prefix = getattr(basic.EnumValues[each], "IsStandardEnum", True)
    prefix = type_name(basic, use_prefix=use_prefix)

    if 'selection' in str(primary.exprType):
        string_hack = str(primary.exprType).split()[-1].strip(" \n<>\'\"").replace("-", "_").split('.')[-1].lower().capitalize().split('_selection')[0]
        nim_string = f"{settings.PROCESS_NAME.capitalize()}_{string_hack}_selection_{enumerant}_present"
    else:
        string_hack = str(primary.exprType.ReferencedTypeName).replace("-", "_")
        nim_string = (prefix + string_hack + "_" + basic.EnumValues[each].EnumID)
    # TODO: Doesn't put right enum name

    return [], str(nim_string), []


@expression.register(ogAST.PrimChoiceDeterminant)
def _choice_determinant(primary, **kwargs):
    ''' Generate code for a choice determinant (enumerated) '''
    enumerant = primary.value[0].replace('_', '-').lower()
    for each in primary.exprType.EnumValues:
        if each.lower() == enumerant:
            break
    nim_string = primary.exprType.EnumValues[each].EnumID  # TODO
    return [], str(nim_string), []


@expression.register(ogAST.PrimInteger)
@expression.register(ogAST.PrimReal)
def _integer(primary, **kwargs):
    ''' Generate code for a raw numerical value  '''
    if float(primary.value[0]) < 0:
        # Put brackets around negative integers for maintaining
        # the precedence in the generated code
        nim_string = f'({primary.value[0]})'
    else:
        nim_string = primary.value[0]
    return [], str(nim_string), []


@expression.register(ogAST.PrimBoolean)
def _boolean(primary, **kwargs):
    ''' Generate code for a raw boolean value  '''
    nim_string = primary.value[0]
    return [], str(nim_string), []


@expression.register(ogAST.PrimNull)
def _null(primary, **kwargs):
    nim_string = 'nil'
    return [], str(nim_string), []


@expression.register(ogAST.PrimEmptyString)
def _empty_string(primary, **kwargs):
    ''' Generate code for an empty SEQUENCE OF: {} '''
    nim_string = f'{type_name(primary.exprType)}()'
    return [], str(nim_string), []


@expression.register(ogAST.PrimStringLiteral)
@expression.register(ogAST.PrimOctetStringLiteral)
def _string_literal(primary, **kwargs):
    ''' Generate code for a string (Octet String) '''
    basic_type = find_basic_type(primary.exprType)
    # If user put a literal string to fill an Octet string,
    # then convert the string to an array of unsigned_8 integers
    # as expected by the Nim type corresponding to Octet String

    if isinstance(primary, ogAST.PrimOctetStringLiteral):
        # Hex string used as input
        unsigned_8 = [str(x) for x in primary.hexstring]
    else:
        unsigned_8 = [str(ord(val)) for val in primary.value[1:-1]]

    nim_string = ', '.join(unsigned_8)
    return [], str(nim_string), []


@expression.register(ogAST.PrimConstant)
def _constant(primary, **kwargs):
    ''' Generate code for a reference to an ASN.1 constant '''
    if primary.constant_c_name == 'pi':
        return [], str(primary.constant_c_name), [f'const {primary.constant_c_name} = {primary.constant_value}']
    else:
        return [], str(primary.constant_c_name), []


@expression.register(ogAST.PrimMantissaBaseExp)
def _mantissa_base_exp(primary, **kwargs):
    # TODO
    return [], '', []


@expression.register(ogAST.PrimConditional)
def _conditional(cond, **kwargs):
    ''' Return string and statements for conditional expressions '''
    stmts = []

    tmp_type = type_name(cond.exprType)

    if tmp_type == 'cstring' or tmp_type.startswith('String'):
        then_str = cond.value['then'].value.replace("'", '"')
        else_str = cond.value['else'].value.replace("'", '"')
        lens = [len(then_str), len(else_str)]
        tmp_type = 'cstring'  # f'String (1 .. {max(lens) - 2})'
        # Ada require fixed-length strings, adjust with spaces
        if lens[0] < lens[1]:
            then_str = then_str[0:-1] + ' ' * (lens[1] - lens[0]) + '"'
        elif lens[1] < lens[0]:
            else_str = else_str[0:-1] + ' ' * (lens[0] - lens[1]) + '"'

    local_decl = [f'var tmp{cond.value["tmpVar"]} : {tmp_type}']
    if_stmts, if_str, if_local = expression(cond.value['if'], readonly=1)
    stmts.extend(if_stmts)
    local_decl.extend(if_local)
    if tmp_type != 'cstring' and not tmp_type.startswith('String'):
        then_stmts, then_str, then_local = expression(cond.value['then'],
                                                      readonly=1)
        else_stmts, else_str, else_local = expression(cond.value['else'],
                                                      readonly=1)
        #       print "\nCONDITIONAL :", cond.inputString, tmp_type,
        #       print "THEN TYPE:", type_name(find_basic_type(cond.value['then'].exprType)),
        #       print "ELSE TYPE:", type_name(find_basic_type(cond.value['else'].exprType))
        stmts.extend(then_stmts)
        stmts.extend(else_stmts)
        local_decl.extend(then_local)
        local_decl.extend(else_local)
    stmts.append('if {if_str}:'.format(if_str=if_str))

    basic_then = find_basic_type(cond.value['then'].exprType)
    basic_else = find_basic_type(cond.value['else'].exprType)

    then_len = None
    if not tmp_type.startswith('String') and tmp_type != 'cstring' and isinstance(cond.value['then'],
                                                                                  (ogAST.PrimSequenceOf,
                                                                                   ogAST.PrimStringLiteral)):
        then_str = array_content(cond.value['then'], then_str, basic_then)
    if isinstance(cond.value['then'], ogAST.ExprAppend):
        then_len = append_size(cond.value['then'])
        stmts.append("tmp{idx} = {then_str}"
                     .format(idx=cond.value['tmpVar'],
                             then_str=then_str))
    elif isinstance(cond.value['then'], ogAST.PrimSubstring):
        stmts.append("tmp{idx} = {then_str}"
                     .format(idx=cond.value['tmpVar'], then_str=then_str))
        if basic_then.Min != basic_then.Max:
            then_len = f"len({then_str})"
    else:
        stmts.append('tmp{idx} = {then_str}'
                     .format(idx=cond.value['tmpVar'], then_str=then_str))
    # if then_len:
    #     stmts.append("tmp{idx} = {then_len}"
    #                  .format(idx=cond.value['tmpVar'], then_len=then_len))

    stmts.append('else:')
    else_len = None
    if not tmp_type.startswith('String') and tmp_type != 'cstring' and isinstance(cond.value['else'],
                                                                                  (ogAST.PrimSequenceOf,
                                                                                   ogAST.PrimStringLiteral)):
        else_str = array_content(cond.value['else'], else_str, basic_else)

    if isinstance(cond.value['else'], ogAST.ExprAppend):
        else_len = append_size(cond.value['else'])
        stmts.append("tmp{idx} = {else_str}"
                     .format(idx=cond.value['tmpVar'],
                             else_str=else_str))
    elif isinstance(cond.value['else'], ogAST.PrimSubstring):
        stmts.append("tmp{idx} = {else_str}"
                     .format(idx=cond.value['tmpVar'], else_str=else_str))
        if basic_else.Min != basic_else.Max:
            else_len = "len({})".format(else_str)
    else:
        stmts.append('tmp{idx} = {else_str}'.format(
            idx=cond.value['tmpVar'],
            else_str=else_str))

    if stmts:
        stmts.append('# end if')
    # if else_len:
    #     stmts.append("tmp{idx}.Length := {else_len}"
    #                  .format(idx=cond.value['tmpVar'], else_len=else_len))
    # stmts.append('end if')
    nim_string = 'tmp{idx}'.format(idx=cond.value['tmpVar'])
    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimSequence)
def _sequence(seq, **kwargs):
    stmts, local_decl = [], []
    try:
        nim_string = f"{type_name(seq.exprType)}("
    except NotImplementedError as err:
        err = f"!!YOU FOUND A BUG!! - The type of this record is undefined: {seq.inputString}"
        raise TypeError(str(err).replace('\n', ''))

    sep = ''
    type_children = find_basic_type(seq.exprType).Children
    type_children = {k.replace("-","_") : v for k,v in type_children.items()}
    optional_fields = {field.lower(): {'present': False,
                                       'ref': (field, val)}
                       for field, val in type_children.items()
                       if val.Optional == 'True'}
    present_fields = []
    absent_fields = []
    counter, ceil = 0, len(seq.value)
    for elem, value in seq.value.items():
        # Set the type of the field - easy thanks to ASN.1 flattened AST
        elem_spec = None
        for each in type_children:
            if each.lower() == elem.lower():
                elem_spec = type_children[each]
                break
        else:
            raise ValueError("Error")

        elem_specty = elem_spec.type

        # Find the basic type of the elem: if it is a number and the value
        # is an octet/bit string literal, then use the raw number
        elem_bty = find_basic_type(elem_specty)
        elem_typename = type_name(elem_specty)

        value_stmts, value_str, local_var = expression(value, readonly=1)

        # if isinstance(value, ogAST.PrimSequence):
        #     value_stmts = [v % f"%s.{elem.lower()}" for v in value_stmts]

        if isinstance(value, (ogAST.PrimSequenceOf, ogAST.PrimStringLiteral)):
            if elem_bty.kind.startswith('Integer'):
                value_str = str(value.numeric_value)
            elif elem_bty.kind == 'IA5StringType':
                value_str = ia5string_raw(value, fill=True)
            else:
                if isinstance(value, ogAST.PrimStringLiteral):
                    S = len(value_str.split(','))
                else:
                    S = len(value.value)
                value_str = f"{elem_typename}(nCount: {S}, arr: {array_content(value, value_str, elem_bty, pad_zeros=True)})"


        if elem.lower() in optional_fields:
            # Set optional field presence
            optional_fields[elem.lower()]['present'] = True
        sep = ', '
        stmts.extend(value_stmts)
        #stmts = [f"%s.{elem.lower()} = {value_str}"] + stmts
        nim_string += f"{elem.lower()}: {value_str}" + (counter < ceil - 1)*", "
        local_decl.extend(local_var)
        counter += 1
    # Process optional fields
    if optional_fields:
        # present_fields = list((fd_name, fd_data['ref'])
        #                       for fd_name, fd_data in optional_fields.items()
        #                       if fd_data['present'])

        nim_string += ", "
        fields = ', '.join([f'{varname}: {int(_dat["present"])}' for varname, _dat in optional_fields.items()])
        nim_string += f"exist: {type_name(seq.exprType)}_exist({fields})"

    # stmts.extend([
    #     f"%s.exist.{varname} = 1" for varname, _dat in present_fields
    # ])

    nim_string += ')'
    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimSequenceOf)
def _sequence_of(seqof, **kwargs):
    stmts, local_decl = [], []
    seqof_ty = seqof.exprType
    try:
        # asn_type = find_basic_type(settings.TYPES[seqof_ty.ReferencedTypeName].type)
        sortref = settings.TYPES[seqof.expected_type.ReferencedTypeName]
        while (hasattr(sortref, "type")):
            sortref = sortref.type
        asn_type = find_basic_type(sortref)
    except AttributeError:
        asn_type = None
        min_size, max_size = seqof_ty.Min, seqof_ty.Max
        if hasattr(seqof, 'expected_type'):
            sortref = settings.TYPES[seqof.expected_type.ReferencedTypeName]
            while (hasattr(sortref, "type")):
                sortref = sortref.type
            asn_type = find_basic_type(sortref)
    tab = []
    for i in range(len(seqof.value)):
        value = seqof.value[i]
        item_stmts, item_str, local_var = expression(value, readonly=1)

        if hasattr(seqof, 'expected_type'):
            bty = find_basic_type(seqof.expected_type)
        else:
            bty = find_basic_type(seqof.exprType)

        if hasattr(bty, 'type'):
            rbty = type_name(bty.type)
        elif hasattr(seqof, 'expected_type'):
            rbty = type_name(seqof.expected_type)
        else:
            rbty = type_name(seqof.exprType)


        if isinstance(value, (ogAST.PrimStringLiteral)):

            item_str = array_content(value, item_str, asn_type or find_basic_type(value.exprType), pad_zeros=True)
            if hasattr(bty, 'Max') and bty.Min != bty.Max:
                S = len(value.inputString.strip("'"))
                item_str = f"{rbty}(nCount: {S}, arr: {item_str})"

        elif isinstance(value, ogAST.PrimSequenceOf):
            item_str = array_content(value, item_str, asn_type or find_basic_type(value.exprType), pad_zeros=True)
            if hasattr(bty, 'Max') and bty.Min != bty.Max:
                S = len(value.value)
                item_str = f"{rbty}(nCount: {S}, arr: {item_str})"

        elif isinstance(value, ogAST.PrimSubstring):
            # Put substring elements in a local variable, otherwise they may
            # not work well with some operators (e.g. Append)
            tmpVarName = f'tmp{seqof.value[i].tmpVar}'
            tmpVarSort = seqof.value[i].exprType
            local_decl.append(f'var {tmpVarName} : {type_name(tmpVarSort)}')
            # To get a proper assignment we need to create an ExprAssign
            expr = ogAST.ExprAssign()
            expr.left = ogAST.PrimVariable()
            expr.left.value = [tmpVarName]
            expr.left.exprType = tmpVarSort
            expr.right = seqof.value[i]
            expr.right.exprType = tmpVarSort
            expr.exprType = tmpVarSort
            assign_stmt, _, assign_loc = expression(expr, readonly=1)
            item_stmts.extend(assign_stmt)
            local_decl.extend(assign_loc)
            item_str = tmpVarName

        stmts.extend(item_stmts)
        local_decl.extend(local_var)
        tab.append(f'{item_str}')
    nim_string = ', '.join(tab)
    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimChoiceItem)
def _choiceitem(choice, **kwargs):
    stmts, choice_str, local_decl = expression(choice.value['value'],
                                               readonly=1)

    chbty = find_basic_type(choice.exprType)
    bty = find_basic_type(choice.value['value'].exprType)
    value = choice.value['value']
    if isinstance(choice.value['value'], (ogAST.PrimSequenceOf,
                                          ogAST.PrimStringLiteral)):
        if bty.kind.startswith('Integer'):
            choice_str = choice.value['value'].numeric_value
        else:
            elem_typename = type_name(choice.value['value'].expected_type)
            elem_bty = find_basic_type(choice.value['value'].expected_type)
            if isinstance(value, ogAST.PrimStringLiteral):
                S = len(choice_str.split(','))
            else:
                S = len(value.value)
            choice_str = f"{elem_typename}(nCount: {S}, arr: {array_content(value, choice_str, elem_bty, pad_zeros=True)})"

    elif isinstance(choice.value['value'], ogAST.PrimEmptyString):
        choice_str = f"{type_name(choice.value['value'].exprType)}()"

    elif isinstance(choice.value['value'], ogAST.PrimSequence):
        choice_name = choice.value['choice']
        T = type_name(choice.exprType)
        nim_string = f"{T}(u: {T}_unchecked_union({choice_name}: {choice_str}), " \
                     f"kind: {type_name(choice.exprType, use_prefix=False)}_{chbty.Children[choice_name].EnumID})"
        return stmts, nim_string, local_decl

    # look for the right spelling of the choice discriminant
    # (normally field_PRESENT, but can be prefixed by the type name if there
    # is a namespace conflict)
    basic = find_basic_type(choice.exprType)
    prefix = 'Choice_NONE'
    search = choice.value['choice'].lower().replace('-', '_')
    for each in basic.Children:
        curr_choice = each.lower().replace('-', '_')
        if curr_choice == search:
            prefix = f"{type_name(choice.exprType, use_prefix=False)}_{basic.Children[each].EnumID}"
            break
    nim_string = f'{type_name(choice.exprType)}(kind: {prefix}, ' \
                 f'u: {type_name(choice.exprType)}_unchecked_union({choice.value["choice"]}: {choice_str}))'  # , u: {choice_str})'

    # Unsure if field is always called u
    # Union is anonymous in C, we dont know the typename in Nim
    # Hacky way to not have to initialize object specifically by adding statement after context init
    # stmts.append(f"%s.u.{choice.value['choice']} = {choice_str}")
    return stmts, str(nim_string), local_decl


def append_size(append):
    ''' Return a string corresponding to the length of an APPEND construct
        This function is recursive, to handle cases such as a//b//c
        that is handled as (a//b) // c -> get the length of a//b then add c
    '''
    # TODO
    result = ''
    basic = find_basic_type(append.exprType)
    if basic.Min == basic.Max:
        # Simple case when appending two fixed-length sizes
        return basic.Min
    for each in (append.left, append.right):
        if result:
            result += ' + '
        if isinstance(each, ogAST.ExprAppend):
            # Inner append -> go recursively
            result += append_size(each)
        else:
            bty = find_basic_type(each.exprType)
            if bty.Min == bty.Max:
                result += bty.Min
            else:
                # Must be a variable of type SEQOF
                _, inner, _ = expression(each, readonly=1)
                if isinstance(each, ogAST.PrimSubstring):
                    result += "{}.nCount".format(inner) #"len({})".format(inner)
                else:
                    result += "{}.nCount".format(inner)
    return result


import inspect
import re

import logging
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


def array_content(prim, values, asnty):
    return __array_content(prim, values, asnty, expression)


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
    exists = var_exists(prim.value[0], settings.VARIABLES)
    if (not exists) or is_local(prim.value[0], settings.LOCAL_VAR):  # Local Variable Access
        sep = ''
    else:  # Global Variable Access
        sep = ''

    # No special action required yet
    nim_string = f'{prim.value[0]}'

    return [], str(nim_string), []


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
            nim_string += f'({param_str}).asn1byte'
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

        if expn_type.kind.beginswith('Integer') and expn_type.Min >= 0:
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
            nim_string += min_length
        else:
            # TODO
            if isinstance(exp, ogAST.PrimSubstring):
                range_str = f"len({param_str})"
            else:
                range_str = f"len({param_str})"
            nim_string += range_str

    elif func in ('observer_status', 'num', 'val', 'to_selector',
                  'to_enum', 'exist', 'choice_to_int', 'present',):
        # TODO
        pass

    else:
        # inner procedure call (with a RETURN statement)
        # TODO
        pass

    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimIndex)
def _prim_index(prim, **kwargs):
    stmts, nim_string, local_decl = [], '', []
    ro = kwargs.get("readonly", 0)
    receiver = prim.value[0]

    receiver_stms, nim_string, receiver_decl = expression(receiver, readonly=ro)
    stmts.extend(receiver_stms)
    local_decl.extend(receiver_decl)

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
        r1_string = str(int(r1_string) + 1)
    else:
        r1_string = f"({r1_string}).asn1SccUint"
    if str.isnumeric(r2_string):
        r2_string = str(int(r2_string))
    else:
        r2_string = f"({r2_string}).asn1SccUint"

    nim_string += f' ({r1_string} ..< {r2_string})'
    stmts.extend(r1_stmts)
    stmts.extend(r2_stmts)
    local_decl.extend(r1_local)
    local_decl.extend(r2_local)

    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimSelector)
def _prim_selector(prim, **kwargs):
    ''' Selector (field access with '!' or '.' separation) '''
    stmts, ada_string, local_decl = [], '', []
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
        nim_string = f'{nim_string}.{field_name}'
    else:
        # SEQUENCE, check for field optionality first
        child = child_spelling(field_name, receiver_bty)
        if receiver_bty.Children[child].Optional == 'True' \
                and not kwargs.get("readonly", 0):
            # Must set Exist only when assigning value, not each time it is
            # accessed: this is what "readonly" ensures.
            stmts.append(f'{nim_string}.exist.{field_name} = 1;')
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

    if lbty.kind.startswith('Integer') and \
            isinstance(expr.right, (ogAST.PrimOctetStringLiteral,
                                    ogAST.PrimBitStringLiteral)):
        right_str = str(expr.right.numeric_value)

    if rbty.kind.startswith('Integer') and \
            isinstance(expr.left, (ogAST.PrimOctetStringLiteral,
                                   ogAST.PrimBitStringLiteral)):
        left_str = str(expr.left.numeric_value)

    if left_is_numeric != right_is_numeric or rbty.kind == lbty.kind:
        # No cast is needed if:
        # - one of the two sides only is a literal
        # - or if the basic types are identical
        nim_string = '({left} {op} {right})'.format(left=left_str,
                                                    op=expr.operand,
                                                    right=right_str)

    elif left_is_numeric and right_is_numeric:
        # Both sides are literals : compute the result on the fly
        nim_string = "{}".format(eval("{left} {op} {right}"
                                      .format(left=left_str,
                                              op=expr.operand,
                                              right=right_str)))

    elif rbty.kind != lbty.kind:
        # Basic types are different (one is an Integer32, eg. loop iterator)
        # => We must cast it to the type of the other side
        if lbty.kind == 'Integer32Type':
            left_str = f'({left_str}).{type_name(expr.right.exprType)}'
        else:
            right_str = f'({right_str}).{type_name(expr.left.exprType)}'
        nim_string = f'({left_str} {expr.operand} {right_str})'

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
        nim_string = f'({left_str} {expr.operand} {right_str})'
    else:
        pass  # TODO
    return code, str(nim_string), local_decl


@expression.register(ogAST.ExprAssign)
def _assign_expression(expr, **kwargs):
    code, local_decl = [], []
    strings = []
    left_stmts, left_str, left_local = expression(expr.left)
    right_stmts, right_str, right_local = expression(expr.right, readonly=1)

    basic_left = find_basic_type(expr.left.exprType)

    if (basic_left.kind == 'IA5StringType') and isinstance(expr.right, ogAST.PrimStringLiteral):
        # TODO
        pass

    elif basic_left.kind in ('SequenceOfType', 'OctetStringType', 'BitStringType'):
        rlen = f"{right_str}'Length"

        if isinstance(expr.right, ogAST.PrimSubstring):
            if not isinstance(expr.left, ogAST.PrimSubstring):
                # only if left is not a substring, otherwise syntax
                # would be wrong due to result of _prim_substring
                strings.append(f"{left_str}.Data(1..{right_str}'Length) := {right_str};")
            else:
                # left is substring: no length, direct assignment
                rlen = ""
                strings.append(f"{left_str} = {right_str};")

        elif isinstance(expr.right, ogAST.ExprAppend):
            basic_right = find_basic_type(expr.right.exprType)
            rlen = append_size(expr.right)
            strings.append("{lvar}[0 ..< {lstr}] = {rvar};"
                           .format(lvar=left_str,
                                   rvar=right_str,
                                   lstr=rlen))

        elif isinstance(expr.right, (ogAST.PrimSequenceOf,
                                     ogAST.PrimStringLiteral)):
            if not isinstance(expr.left, ogAST.PrimSubstring):
                strings.append(
                    f"{left_str} = {array_content(expr.right, right_str, basic_left)};")
            else:
                # left is substring: no length, direct assignment
                strings.append(f"{left_str} = ({right_str});")

            rlen = None
        else:
            # Right part is a variable
            strings.append(f"{left_str} := {right_str};")
            rlen = None
        if rlen and basic_left.Min != basic_left.Max:
            strings.append(f"{left_str}.Length := {rlen};")

    elif basic_left.kind.startswith('Integer') and isinstance(expr.right,
                                                              (ogAST.PrimOctetStringLiteral,
                                                               ogAST.PrimBitStringLiteral)):
        # If right is an octet string or bit string literal, use the numerical
        # value directly.
        right_str = str(expr.right.numeric_value)
        strings.append(f"{left_str} = {right_str};")

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

    if basic_type.kind != 'BooleanType':
        left_bty = find_basic_type(expr.left.exprType)
        right_bty = find_basic_type(expr.left.exprType)

        if left_bty.kind.startswith('Integer') and right_bty.kind.startswith('Integer'):
            # left and right are numbers
            nim_string = f'({left_str} {expr.operand} {right_str})'

        elif expr.right.is_raw:
            if left_bty.kind.startswith('Integer'):
                # right is raw (e.g. hex string literal) and left is a number
                if isinstance(expr.right, (ogAST.PrimBitStringLiteral, ogAST.PrimOctetStringLiteral)):
                    right_payload = str(expr.right.numeric_value)
                else:
                    right_payload = right_str

                left_payload = left_str  # + string_payload(expr.left, left_str)
                nim_string = f'({left_payload} {expr.operand} {right_payload})'

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

                local_decl.append(f'{tmp_string} : constant {type_name(expr.right.exprType)} := {right_str};')
                # code.append(f'{tmp_string} := {right_str};')

                right_str = tmp_string
                right_payload = right_str + '.Data'

        else:
            right_payload = right_str + string_payload(expr.right, right_str)

    elif isinstance(expr, ogAST.ExprImplies):
        nim_string = f'((not {left_str}) or {right_str})'
    else:
        nim_string = f'({left_str} {expr.operand}{expr.shortcircuit} {right_str})'

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
        nim_string = array_content(expr.expr, expr_str, bty_outer)
        # TODO
    else:
        nim_string = f'(not {expr_str})'

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
    not_implemented_error()


@expression.register(ogAST.ExprIn)
def _expr_in(expr, **kwargs):
    ''' IN expressions: check if item is in a SEQUENCE OF '''
    stmts, local_decl = [], []
    nim_string = ""

    left_stmts, left_str, left_local = expression(expr.left, readonly=1)
    right_stmts, right_str, right_local = expression(expr.right, readonly=1)

    local_decl.extend(left_local)
    local_decl.extend(right_local)

    stmts.extend(left_stmts)
    stmts.extend(right_stmts)

    # it is possible to test against a raw sequence of: x in { 1,2,3 }
    # in that case we create an array on the type of x, and we test
    # presence using the form "for some Value of tmpXXX => x = Value"
    if isinstance(expr.left, ogAST.PrimSequenceOf):
        sort = type_name(expr.right.exprType)
        size = expr.left.exprType.Max

        local_decl.append(f'tmp{expr.tmpVar} : constant array[ {size} , {sort} ] = ({left_str})')
        nim_string = f'(for loopvar{expr.tmpVar} of tmp{expr.tmpVar} => var = {right_str})'
    else:
        local_decl.append(f'tmp{expr.tmpVar} : bool = false;')
        nim_string = f'tmp{expr.tmpVar}'

        # stmts.append(f"in_loop_{nim_string}:")
        left_type = find_basic_type(expr.left.exprType)

        len_str = f"len({left_str})"

        if left_type.Min != left_type.Max:
            stmts.append(f"for idx in 0 ..< {len_str}:")
        else:
            stmts.append(f"for idx in {left_str}.low .. {left_str}.high:")

        stmts.append(f"if {left_str}[idx] == {right_str}:")

        stmts.append(f"{nim_string} = true")

        stmts.append(f"if {nim_string} == true: break")

    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimEnumeratedValue)
def _enumerated_value(primary, **kwargs):
    ''' Generate code for an enumerated value '''
    enumerant = primary.value[0].replace('_', '-').lower()
    basic = find_basic_type(primary.exprType)
    for each in basic.EnumValues:
        if each.lower() == enumerant:
            break
    # no "asn1Scc" prefix if the enumerated is a choice selector
    use_prefix = getattr(basic.EnumValues[each], "IsStandardEnum", True)
    prefix = __type_name(basic, use_prefix=use_prefix)
    nim_string = (prefix + basic.EnumValues[each].EnumID) # TODO
    return [], str(nim_string), []


@expression.register(ogAST.PrimChoiceDeterminant)
def _choice_determinant(primary, **kwargs):
    ''' Generate code for a choice determinant (enumerated) '''
    enumerant = primary.value[0].replace('_', '-').lower()
    for each in primary.exprType.EnumValues:
        if each.lower() == enumerant:
            break
    nim_string = primary.exprType.EnumValues[each].EnumID # TODO
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
    nim_string = '""'
    return [], str(nim_string), []


@expression.register(ogAST.PrimStringLiteral)
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

    if tmp_type == 'String':
        then_str = cond.value['then'].value.replace("'", '"')
        else_str = cond.value['else'].value.replace("'", '"')
        lens = [len(then_str), len(else_str)]
        tmp_type = 'cstring' #f'String (1 .. {max(lens) - 2})'
        # Ada require fixed-length strings, adjust with spaces
        if lens[0] < lens[1]:
            then_str = then_str[0:-1] + ' ' * (lens[1] - lens[0]) + '"'
        elif lens[1] < lens[0]:
            else_str = else_str[0:-1] + ' ' * (lens[0] - lens[1]) + '"'

    local_decl = [f'tmp{cond.value["tmpVar"]} : {tmp_type};']
    if_stmts, if_str, if_local = expression(cond.value['if'], readonly=1)
    stmts.extend(if_stmts)
    local_decl.extend(if_local)
    if not tmp_type.startswith('String'):
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
    if not tmp_type.startswith('String') and isinstance(cond.value['then'],
                                                        (ogAST.PrimSequenceOf, ogAST.PrimStringLiteral)):
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
        stmts.append('tmp{idx} = {then_str};'
                     .format(idx=cond.value['tmpVar'], then_str=then_str))
    # if then_len:
    #     stmts.append("tmp{idx} = {then_len};"
    #                  .format(idx=cond.value['tmpVar'], then_len=then_len))

    stmts.append('else:')
    else_len = None
    if not tmp_type.startswith('String') and isinstance(cond.value['else'],
                                                        (ogAST.PrimSequenceOf, ogAST.PrimStringLiteral)):
        else_str = array_content(cond.value['else'], else_str, basic_else)

    if isinstance(cond.value['else'], ogAST.ExprAppend):
        else_len = append_size(cond.value['else'])
        stmts.append("tmp{idx} = {else_str};"
                     .format(idx=cond.value['tmpVar'],
                             else_str=else_str))
    elif isinstance(cond.value['else'], ogAST.PrimSubstring):
        stmts.append("tmp{idx} = {else_str};"
                     .format(idx=cond.value['tmpVar'], else_str=else_str))
        if basic_else.Min != basic_else.Max:
            else_len = "len({})".format(else_str)
    else:
        stmts.append('tmp{idx} = {else_str};'.format(
            idx=cond.value['tmpVar'],
            else_str=else_str))
    # if else_len:
    #     stmts.append("tmp{idx}.Length := {else_len};"
    #                  .format(idx=cond.value['tmpVar'], else_len=else_len))
    # stmts.append('end if;')
    nim_string = 'tmp{idx}'.format(idx=cond.value['tmpVar'])
    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimSequence)
def _sequence(seq, **kwargs):
    stmts, local_decl = [], []
    try:
        nim_string = f"{type_name(seq.exprType)}'("
    except NotImplementedError as err:
        err = f"!!YOU FOUND A BUG!! - The type of this record is undefined: {seq.inputString}"
        raise TypeError(str(err).replace('\n', ''))

    sep = ''
    type_children = find_basic_type(seq.exprType).Children
    optional_fields = {field.lower(): {'present': False,
                                       'ref': (field, val)}
                       for field, val in type_children.items()
                       if val.Optional == 'True'}
    present_fields = []
    absent_fields = []
    for elem, value in seq.value.items():
        # Set the type of the field - easy thanks to ASN.1 flattened AST
        for each in type_children:
            if each == elem:
                elem_spec = type_children[each]
                break

        elem_specty = elem_spec.type

        # Find the basic type of the elem: if it is a number and the value
        # is an octet/bit string literal, then use the raw number
        elem_bty = find_basic_type(elem_specty)

        value_stmts, value_str, local_var = expression(value, readonly=1)

        if isinstance(value, (ogAST.PrimSequenceOf, ogAST.PrimStringLiteral)):
            if elem_bty.kind.startswith('Integer'):
                value_str = str(value.numeric_value)
            elif elem_bty.kind == 'IA5StringType':
                value_str = ia5string_raw(value)
            else:
                value_str = array_content(value, value_str, elem_bty)

        nim_string += f"{sep} {elem} => {value_str}"
        if elem.lower() in optional_fields:
            # Set optional field presence
            optional_fields[elem.lower()]['present'] = True
        sep = ', '
        stmts.extend(value_stmts)
        local_decl.extend(local_var)
    # Process optional fields
    if optional_fields:
        absent_fields = ((fd_name, fd_data['ref'])
                         for fd_name, fd_data in optional_fields.items()
                         if not fd_data['present'])
        for fd_name, fd_data in absent_fields:
            fd_type = fd_data[1].type
            if fd_type.kind == 'ReferenceType':
                value = f'{type_name(fd_type)}_Init'
            elif fd_type.kind == 'BooleanType':
                value = 'False'
            elif fd_type in ('IntegerType', 'RealType'):
                value = fd_type.Min
            nim_string += f'{sep}{fd_name}: {value}'
            sep = ', '
        nim_string += ', Exist: ('
        sep = ''
        for fd_name, fd_data in optional_fields.items():
            nim_string += f'{sep}{fd_name}: {"true" if fd_data["present"] else "false"}'
            sep = ', '
        nim_string += ')'

    nim_string += ')'
    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimSequenceOf)
def _sequence_of(seqof, **kwargs):
    stmts, local_decl = [], []
    seqof_ty = seqof.exprType
    try:
        asn_type = find_basic_type(settings.TYPES[seqof_ty.ReferencedTypeName].type)
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
        item_stmts, item_str, local_var = expression(seqof.value[i],
                                                     readonly=1)
        if isinstance(seqof.value[i],
                      (ogAST.PrimSequenceOf, ogAST.PrimStringLiteral)):
            item_str = array_content(seqof.value[i], item_str, asn_type or
                                     find_basic_type(seqof.value[i].exprType))
        elif isinstance(seqof.value[i], ogAST.PrimSubstring):
            # Put substring elements in a local variable, otherwise they may
            # not work well with some operators (e.g. Append)
            tmpVarName = f'tmp{seqof.value[i].tmpVar}'
            tmpVarSort = seqof.value[i].exprType
            local_decl.append(f'{tmpVarName} : {type_name(tmpVarSort)};')
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

    bty = find_basic_type(choice.value['value'].exprType)

    if isinstance(choice.value['value'], (ogAST.PrimSequenceOf,
                                          ogAST.PrimStringLiteral)):
        if bty.kind.startswith('Integer'):
            choice_str = choice.value['value'].numeric_value
        else:
            choice_str = array_content(choice.value['value'], choice_str, bty)

    # look for the right spelling of the choice discriminant
    # (normally field_PRESENT, but can be prefixed by the type name if there
    # is a namespace conflict)
    basic = find_basic_type(choice.exprType)
    prefix = 'CHOICE_NOT_FOUND'
    search = choice.value['choice'].lower().replace('-', '_')
    for each in basic.Children:
        curr_choice = each.lower().replace('-', '_')
        if curr_choice == search:
            prefix = basic.Children[each].EnumID
            break
    nim_string = f'(Kind => {prefix}, {choice.value["choice"]} => {choice_str})' # TODO
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
                if isinstance (each, ogAST.PrimSubstring):
                    result += "{}'Length".format(inner)
                else:
                    result += "{}.Length".format(inner)
    return result
import imp
import inspect
import re

import logging
import opengeode

from functools import singledispatch
from opengeode import ogAST, Helper

from .utils import (not_implemented_error, find_var, is_local, is_numeric, find_basic_type, type_name, child_spelling)
from .globals import *

from typing import List, Tuple

LOG = logging.getLogger(__name__)

__all__ = ['expression']


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
    ''' Single variable reference '''
    var = find_var(prim.value[0], VARIABLES)
    if (not var) or is_local(var, LOCAL_VAR):
        sep = ''
    else:
        sep = LPREFIX + '.'

    nim_string = f'{sep}{prim.value[0]}'

    return [], str(nim_string), []


@expression.register(ogAST.PrimCall)
def _prim_call(prim, **kwargs):
    stmts, nim_string, local_decl = [], '', []

    func = prim.value[0].lower()
    params = prim.value[1]['procParams']

    STD_FUNCTIONS = {
        'abs': 1, 'fix': 1, 'ceil': 1, 'floor': 1, 'float': 1, 'round': 1, 'trunc': 1,
        'power': 2, 'sin': 1, 'cos': 1, 'sqrt': 1, 'length': 1, 'num': 1, 'choice_to_int': 2,
        'shift_left': 2, 'shift_right': 2, 'to_selector': 2, 'to_enum': 2, 'present': 1, 'exist': 1,
        'val': 2
    }

    if func in STD_FUNCTIONS.keys():
        assert STD_FUNCTIONS[func] == len(params)

    NUM_PARAMS = len(params)

    # TODO

    return stmts, nim_string, local_decl


@expression.register(ogAST.PrimIndex)
def _prim_index(prim, **kwargs):
    stmts, nim_string, local_decl = [], '', []
    ro = kwargs.get("readonly", 0)

    receiver = prim.value[0]

    receiver_stms, nim_string, receiver_decl = expression(receiver,
                                                          readonly=ro)
    stmts.extend(receiver_stms)
    local_decl.extend(receiver_decl)

    index = prim.value[1]['index'][0]
    idx_stmts, idx_string, idx_var = expression(index, readonly=ro)
    if str.isnumeric(idx_string):
        idx_string = int(idx_string) + 1
    else:
        idx_string = f'1 + ({idx_string}).int32'
    # if not isinstance(receiver, ogAST.PrimSubstring):
    #     nim_string += '.Data'  # TODO
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

    # Adding 1 because SDL starts indexes at 0, ASN1 Ada types at 1
    if str.isnumeric(r1_string):
        r1_string = str(int(r1_string) + 1)
    else:
        r1_string = f"({r1_string}).int32 + 1"
    if str.isnumeric(r2_string):
        r2_string = str(int(r2_string) + 1)
    else:
        r2_string = f"({r2_string}).int32 + 1"

    # if not isinstance(receiver, ogAST.PrimSubstring):
    #     nim_string += '.Data' # TODO
    nim_string += f' [{r1_string} ..< {r2_string}]'
    stmts.extend(r1_stmts)
    stmts.extend(r2_stmts)
    local_decl.extend(r1_local)
    local_decl.extend(r2_local)

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

    receiver_bty = find_basic_type(receiver.exprType, TYPES)

    if receiver_bty.kind == 'ChoiceType':
        nim_string = f'{nim_string}.{field_name}'
    else:
        # SEQUENCE, check for field optionality first
        child = child_spelling(field_name, receiver_bty)
        if receiver_bty.Children[child].Optional == 'True' \
                and not kwargs.get("readonly", 0):
            # Must set Exist only when assigning value, not each time it is
            # accessed: this is what "readonly" ensures.
            # stmts.append(f'{nim_string}.Exist.{field_name} := 1;') # TODO
            pass
        nim_string = f'{nim_string}.{field_name}'

    return stmts, str(nim_string), local_decl


@expression.register(ogAST.PrimStateReference)
def _primary_state_reference(prim, **kwargs):
    ''' Reference to the current state '''
    return [], f'{LPREFIX}.state', []


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
    """ Operators with 2 sides """
    code, local_decl = [], []

    left_stmts, left_str, left_local = expression(expr.left, readonly=1)
    right_stmts, right_str, right_local = expression(expr.right, readonly=1)

    # Check if either side is a literal number
    right_is_numeric = is_numeric(right_str)
    left_is_numeric = is_numeric(left_str)

    lbty = find_basic_type(expr.left.exprType, TYPES)
    rbty = find_basic_type(expr.right.exprType, TYPES)

    nim_string = ''

    if lbty.kind.startswith('Integer') and \
            isinstance(expr.right, (ogAST.PrimOctetStringLiteral,
                                    ogAST.PrimBitStringLiteral)):
        right_str = str(expr.right.numeric_value)

    if rbty.kind.startswith('Integer') and \
            isinstance(expr.left, (ogAST.PrimOctetStringLiteral,
                                   ogAST.PrimBitStringLiteral)):
        left_str = str(expr.left.numeric_value)

    if (left_is_numeric != right_is_numeric) or (rbty.kind == lbty.kind):
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
            left_str = f'({left_str}).{type_name(expr.right.exprType, ASN1SCC)}'
        else:
            right_str = f'({right_str}).{type_name(expr.left.exprType, ASN1SCC)}'
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
    actual_type = type_name(expr.left.exprType, ASN1SCC)

    lbty = find_basic_type(expr.left.exprType, ASN1SCC)
    rbty = find_basic_type(expr.right.exprType, ASN1SCC)

    nim_string = ''

    basic = lbty.kind in ('IntegerType',
                          'Integer32Type',
                          'IntegerU8Type',
                          'BooleanType',
                          'EnumeratedType',
                          'ChoiceEnumeratedType')
    if basic:
        if isinstance(expr.right, (ogAST.PrimBitStringLiteral,
                                   ogAST.PrimOctetStringLiteral)):
            right_str = str(expr.right.numeric_value)
        # Cast in case a side is using a 32bits ints (eg when using Length(..))
        if lbty.kind == 'IntegerType' and rbty.kind != lbty.kind:
            right_str = f'({right_str}).{type_name(lbty, ASN1SCC)}'
        elif rbty.kind == 'IntegerType' and lbty.kind != rbty.kind:
            left_str = f'({left_str}).{type_name(rbty, ASN1SCC)}'
        nim_string = f'({left_str} {expr.operand} {right_str})'
    else:
        if asn1_type in TYPES:
            if isinstance(expr.right, (ogAST.PrimSequenceOf, ogAST.PrimStringLiteral)):
                if lbty.kind.startswith('Integer'):
                    right_str = str(expr.right.numeric_value)
                elif lbty.kind == 'IA5StringType':
                    # right_str = ia5string_raw(expr.right)
                    pass # TODO
                else:
                    # right_str = array_content(expr.right, right_str, lbty)
                    pass # TODO
            nim_string = f'{actual_type}_Equal ({left_str}, {right_str})'
        else:
            # Raw types on both left and right.... use simple operator
            nim_string = f"({left_str}) {expr.operand} ({right_str})"
        if isinstance(expr, ogAST.ExprNeq):
            nim_string = f'not {nim_string}'
    return code, str(nim_string), local_decl


@expression.register(ogAST.ExprAssign)
def _assign_expression(expr, **kwargs):
    code, local_decl = [], []
    strings = []
    left_stmts, left_str, left_local = expression(expr.left)
    right_stmts, right_str, right_local = expression(expr.right, readonly=1)

    # If left side is a string/seqOf and right side is a substring, we must
    # assign the .Data and .Length parts properly
    basic_left = find_basic_type(expr.left.exprType, TYPES)
    if (basic_left.kind == 'IA5StringType'
            and isinstance(expr.right, ogAST.PrimStringLiteral)):
        # Assignment of a raw IA5String: do not use the result of expression
        # as it represents the string as a sequence of numbers to fit
        # OCTET STRINGs.
        # def_value = ia5string_raw(expr.right)
        # strings.append(f'{left_str} = {def_value};')
        pass # TODO
    elif basic_left.kind in ('SequenceOfType', 'OctetStringType', 'BitStringType'):
        rlen = f"{right_str}'Length"

        if isinstance(expr.right, ogAST.PrimSubstring):
            if not isinstance(expr.left, ogAST.PrimSubstring):
                # only if left is not a substring, otherwise syntax
                # would be wrong due to result of _prim_substring
                strings.append(f"{left_str}[1 .. <{right_str}.len] = {right_str};")
            else:
                # left is substring: no length, direct assignment
                rlen = ""
                strings.append(f"{left_str} := {right_str};")

        elif isinstance(expr.right, ogAST.ExprAppend):
            basic_right = find_basic_type(expr.right.exprType, TYPES)
            rlen = append_size(expr.right)
            strings.append("{lvar}.Data(1..{lstr}) := {rvar};"
                           .format(lvar=left_str,
                                   rvar=right_str,
                                   lstr=rlen))

        elif isinstance(expr.right, (ogAST.PrimSequenceOf,
                                     ogAST.PrimStringLiteral)):
            if not isinstance(expr.left, ogAST.PrimSubstring):
                strings.append(
                    f"{left_str} := {array_content(expr.right, right_str, basic_left)};")
            else:
                # left is substring: no length, direct assignment
                strings.append(f"{left_str} := ({right_str});")

            rlen = None
        else:
            # Right part is a variable
            strings.append(f"{left_str} = {right_str};")
            rlen = None
        if rlen and basic_left.Min != basic_left.Max:
            strings.append(f"{left_str}.Length = {rlen};")
    elif basic_left.kind.startswith('Integer') and \
            isinstance(expr.right, (ogAST.PrimOctetStringLiteral,
                                    ogAST.PrimBitStringLiteral)):
        # If right is an octet string or bit string literal, use the numerical
        # value directly.
        right_str = str(expr.right.numeric_value)
        strings.append(f"{left_str} = {right_str};")
    elif basic_left.kind.startswith('Integer'):
        #       print '\nASSIGN:', expr.inputString,
        #       print "Left type = ",type_name(find_basic_type (expr.left.exprType)),
        #       print "- Right type = ",type_name(find_basic_type (expr.right.exprType))

        # Make sure that integers are cast to 64 bits
        # It is possible that left and right are of different types
        # (signed vs unsigned and/or 32 bits vs 64 bits).
        # The parser should have ensured that the ranges are compatible.
        # We can therefore safely cast to the left type
        basic_right = find_basic_type(expr.right.exprType, TYPES)
        cast_left, cast_right = type_name(basic_left, ASN1SCC), type_name(basic_right, ASN1SCC)
        # print (cast_left, cast_right, right_str)
        if cast_left != cast_right:
            res = f'({right_str}).{cast_left}'
        else:
            if hasattr(expr.right, "expected_type") \
                    and expr.right.expected_type is not None:

                cast_expected = type_name(expr.right.expected_type, ASN1SCC)
                if cast_expected != cast_left:
                    res = f'({right_str}).{cast_left}'
                else:
                    res = right_str
            else:
                res = right_str

        strings.append(f"{left_str} = {res};")
    else:
        strings.append(f"{left_str} = {right_str};")
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
    not_implemented_error()


@expression.register(ogAST.ExprNot)
def _not_expression(expr, **kwargs):
    not_implemented_error()


@expression.register(ogAST.ExprNeg)
def _neg_expression(expr, **kwargs):
    not_implemented_error()


@expression.register(ogAST.ExprAppend)
def _append(expr, **kwargs):
    not_implemented_error()


@expression.register(ogAST.ExprIn)
def _expr_in(expr, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimEnumeratedValue)
def _enumerated_value(primary, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimChoiceDeterminant)
def _choice_determinant(primary, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimInteger)
@expression.register(ogAST.PrimReal)
def _integer(primary, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimBoolean)
def _boolean(primary, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimNull)
def _null(primary, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimEmptyString)
def _empty_string(primary, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimStringLiteral)
def _string_literal(primary, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimConstant)
def _constant(primary, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimMantissaBaseExp)
def _mantissa_base_exp(primary, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimConditional)
def _conditional(cond, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimSequence)
def _sequence(seq, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimSequenceOf)
def _sequence_of(seqof, **kwargs):
    not_implemented_error()


@expression.register(ogAST.PrimChoiceItem)
def _choiceitem(choice, **kwargs):
    not_implemented_error()



def append_size(append, TYPES):
    ''' Return a string corresponding to the length of an APPEND construct
        This function is recursive, to handle cases such as a//b//c
        that is handled as (a//b) // c -> get the length of a//b then add c
    '''
    result = ''
    basic = find_basic_type(append.exprType, TYPES)
    if basic.Min == basic.Max:
        # Simple case when appending two fixed-length sizes
        return basic.Min
    for each in (append.left, append.right):
        if result:
            result += ' + '
        if isinstance(each, ogAST.ExprAppend):
            # Inner append -> go recursively
            result += append_size(each, TYPES)
        else:
            bty = find_basic_type(each.exprType, TYPES)
            if bty.Min == bty.Max:
                result += bty.Min
            else:
                # Must be a variable of type SEQOF
                _, inner, _ = expression(each, readonly=1)
                if isinstance (each, ogAST.PrimSubstring):
                    result += "{}.len".format(inner)
                else:
                    result += "{}.len".format(inner)
    return result
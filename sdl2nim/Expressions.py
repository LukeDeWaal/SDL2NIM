import inspect
import re

import logging
import opengeode

from functools import singledispatch
from opengeode import ogAST, Helper

from typing import List, Tuple

LOG = logging.getLogger(__name__)

__all__ = ['expression', 'not_implemented_error']


def not_implemented_error():
    raise NotImplementedError(f"{inspect.stack()[1][3]} is not implemented yet")


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
    not_implemented_error()

@expression.register(ogAST.PrimCall)
def _prim_call(prim, **kwargs):
    not_implemented_error()

@expression.register(ogAST.PrimIndex)
def _prim_index(prim, **kwargs):
    not_implemented_error()

@expression.register(ogAST.PrimSubstring)
def _prim_substring(prim, **kwargs):
    not_implemented_error()

@expression.register(ogAST.PrimSelector)
def _prim_selector(prim, **kwargs):
    not_implemented_error()

@expression.register(ogAST.PrimStateReference)
def _primary_state_reference(prim, **kwargs):
    not_implemented_error()

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
    not_implemented_error()

@expression.register(ogAST.ExprEq)
@expression.register(ogAST.ExprNeq)
def _equality(expr, **kwargs):
    not_implemented_error()

@expression.register(ogAST.ExprAssign)
def _assign_expression(expr, **kwargs):
    not_implemented_error()

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


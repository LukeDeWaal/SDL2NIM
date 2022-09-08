import inspect

from opengeode import ogAST, ogParser
from opengeode.Helper import find_basic_type as __find_basic_type


__all__ = ['not_implemented_error',
           'var_exists',
           'is_numeric',
           'traceability',
           'is_local',
           'string_payload',
           'array_content',
           'child_spelling']


def not_implemented_error():
    raise NotImplementedError(f"{inspect.stack()[1][3]} is not implemented yet")



def is_numeric(string) -> bool:
    ''' Return true if value is a number '''
    try:
        float(string)
    except ValueError:
        return False
    return True


def traceability(symbol):
    ''' Return a string with code-to-model traceability '''
    trace = ['### {line}'.format(line=l) for l in
             symbol.trace().split('\n')]
    if hasattr(symbol, 'comment') and symbol.comment:
        trace.extend(traceability(symbol.comment))
    return trace


def var_exists(var, variables):
    ''' Return a variable from the scope, with proper case '''
    if var not in variables.keys():
        return False
    else:
        return True


def is_local(var, local_var):
    ''' Check if a variable is in the global context or in a local scope
        Typically needed to select the right prefix to use '''
    return var in (loc for loc in local_var.keys())


def type_name(a_type, prefix=''):
    ''' Check the type kind and return a Nim usable type name '''
    if a_type.kind == 'ReferenceType':
        return prefix + a_type.ReferencedTypeName.replace('-', '_')
    elif a_type.kind == 'BooleanType':
        return 'bool'
    elif a_type.kind.startswith('Integer32'):
        return 'asn1SccSint32'
    elif a_type.kind.startswith('IntegerU8'):
        return 'asn1byte'
    elif a_type.kind.startswith('Integer'):
        if float(a_type.Min) >= 0:
            return 'Asn1UInt'
        else:
            return 'Asn1Int'
    elif a_type.kind == 'RealType':
        return 'asn1Real'
    elif a_type.kind.endswith('StringType'):
        return 'string'
    elif a_type.kind == 'ChoiceEnumeratedType':
        return 'asn1SccUint32'
    elif a_type.kind == 'StateEnumeratedType':
        return prefix
    elif a_type.kind == 'EnumeratedType':
        return prefix
    else:
        raise NotImplementedError(f'Type name for {a_type.kind}')


def string_payload(prim, nim_string, TYPES):
    ''' Return the data buffer of string, including range computed according
        to the length, if the string has a variable size '''
    if isinstance(prim, ogAST.PrimSubstring):
        return ''
    prim_basic = __find_basic_type(TYPES, prim.exprType)
    payload = ''
    if prim_basic.kind in ('SequenceOfType', 'OctetStringType', 'BitStringType'):
        if int(prim_basic.Min) != int(prim_basic.Max):
            payload = f'[0 ..< len({nim_string})]'
        else:
            payload = ''
    return payload


def array_content(prim, values, asnty, expression: callable):
    ''' String literal and SEQOF are given as a sequence of elements ;
    this function builds the Ada string needed to fit it in an ASN.1 array
    i.e. convert "1,2,3" to "Data => (1,2,3, others=>0), [Length => 3]"
    inputs: prim is of type PrimStringLiteral or PrimSequenceOf
    values is a string with the sequence of numbers as processed by expression
    asnty is the reference type of the string literal '''
    if isinstance(prim, ogAST.PrimEmptyString):
        return values
    if asnty.Min != asnty.Max:
        length = len(prim.value)
        if isinstance(prim, ogAST.PrimStringLiteral):
            # Quotes are kept in string literals
            length -= 2
        elif isinstance(prim, ogAST.PrimOctetStringLiteral):
            length = len(prim.hexstring)
        # Reference type can vary -> there is a Length field
        rlen = f", Length => {length}"
    else:
        rlen = ""
    if isinstance(prim, ogAST.PrimStringLiteral):
        df = '0'
    else:
        # Find a default value for the "others" field in case of SEQOF
        _, df, _ = expression(prim.value[0], readonly=1)
        if isinstance(prim.value[0], (ogAST.PrimSequenceOf,
                                      ogAST.PrimStringLiteral)):
            df = array_content(prim.value[0], df, asnty.type, expression)
    return f"[{values}]" # TODO


def child_spelling(name, bty):
    ''' Return the index in Children with the proper spelling (case, dash) '''
    if name in bty.Children:
        return name
    raise TypeError(f'Child not found: {name}')


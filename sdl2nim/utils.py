import inspect

from opengeode import Helper

__all__ = ['not_implemented_error',
           'find_var',
           'is_numeric',
           'is_local',
           'find_basic_type',
           'type_name',
           'child_spelling']


def not_implemented_error():
    raise NotImplementedError(f"{inspect.stack()[1][3]} is not implemented yet")


def find_var(variable, VARIABLES):
    for var_ in VARIABLES.keys():
        if var_.lower() == variable.lower():
            return var_
    return None


def is_local(var, LOCAL_VARS):
    ''' Check if a variable is in the global context or in a local scope
        Typically needed to select the right prefix to use '''
    return var.lower() in (loc.lower() for loc in LOCAL_VARS.keys())


def is_numeric(string: str) -> bool:
    try:
        val = float(string)
    except ValueError:
        return False
    return True


def find_basic_type(a_type, TYPES):
    return Helper.find_basic_type(TYPES, a_type)


def type_name(a_type, ASN1SCC, use_prefix=True):
    ''' Check the type kind and return a Nim usable type name '''
    if a_type.kind == 'ReferenceType':
        if use_prefix:
            return ASN1SCC + a_type.ReferencedTypeName.replace('-', '_')
        else:
            return a_type.ReferencedTypeName.replace("-", "_")
    elif a_type.kind == 'BooleanType':
        return 'Boolean'
    elif a_type.kind.startswith('Integer32'):
        return 'Integer'
    elif a_type.kind.startswith('IntegerU8'):
        return 'Interfaces.Unsigned_8'
    elif a_type.kind.startswith('Integer'):
        if float(a_type.Min) >= 0:
            return 'Asn1UInt'
        else:
            return 'Asn1Int'
    elif a_type.kind == 'RealType':
        return 'Asn1Real'
    elif a_type.kind.endswith('StringType'):
        return 'String'
    elif a_type.kind == 'ChoiceEnumeratedType':
        return 'Asn1InT'
    elif a_type.kind == 'StateEnumeratedType':
        return ASN1SCC
    elif a_type.kind == 'EnumeratedType':
        return ASN1SCC if use_prefix else ''
    else:
        raise NotImplementedError(f'Type name for {a_type.kind}')


def child_spelling(name, bty):
    ''' Return the index in Children with the proper spelling (case, dash) '''
    for each in bty.Children:
        if name.lower().replace('_', '-') == each.lower():
            return each
    raise TypeError(f'Child not found: {name}')



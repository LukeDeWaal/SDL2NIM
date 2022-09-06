import inspect

__all__ = ['not_implemented_error',
           'var_exists',
           'is_numeric']


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
        return 'int32'
    elif a_type.kind.startswith('IntegerU8'):
        return 'int8'
    elif a_type.kind.startswith('Integer'):
        if float(a_type.Min) >= 0:
            return 'Asn1UInt'
        else:
            return 'Asn1Int'
    elif a_type.kind == 'RealType':
        return 'Asn1Real'
    elif a_type.kind.endswith('StringType'):
        return 'string'
    elif a_type.kind == 'ChoiceEnumeratedType':
        return 'int16'
    elif a_type.kind == 'StateEnumeratedType':
        return prefix
    elif a_type.kind == 'EnumeratedType':
        return prefix
    else:
        raise NotImplementedError(f'Type name for {a_type.kind}')
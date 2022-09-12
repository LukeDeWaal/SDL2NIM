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
           'child_spelling',
           'ia5string_raw',
           'external_ri_list',
           'procedure_header',
           'format_nim_code']


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


def type_name(a_type, use_prefix=True, prefix=''):
    ''' Check the type kind and return a Nim usable type name '''
    if a_type.kind == 'ReferenceType':
        if use_prefix:
            return prefix + a_type.ReferencedTypeName
        else:
            return a_type.ReferencedTypeName
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
        if use_prefix:
            return prefix
        else:
            return ''
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


def ia5string_raw(prim: ogAST.PrimStringLiteral):
    ''' IA5 Strings are of type String in Ada but this is not directly
        compatible with variable-length strings as defined in ASN.1
        Since the Ada type maps to a null-terminated C type, we have to make
        a corresponding assignment, filling then non-used part of the container
        with NULL character. To know the size, we can use adaasn1rtl.getStringSize
        '''
    # TODO
    return "('" + "', '".join(prim.value[1:-1]) + "', others => Standard.ASCII.NUL)"


def external_ri_list(process, SEPARATOR, ASN1SCC):
    ''' Helper function: create a list of RI with proper signature
    Used for the formal parameters of generic packages when using process type
    '''
    result = []
    #print process.fpar
    for signal in process.output_signals:
        param_name = signal.get('param_name') or f'{signal["name"]}_param'
        param_spec = ''
        if 'type' in signal:
            typename = type_name(signal['type'])
            param_spec = f'({param_name}: {typename}): {typename}'
        result.append(f"proc RI{SEPARATOR}{signal['name']}{param_spec}")
    for proc in (proc for proc in process.procedures if proc.external):
        ri_header = f'proc RI{SEPARATOR}{proc.inputString}'
        in_params = []
        out_params = []
        in_params_spec = ''
        out_params_spec = ''
        for param in proc.fpar:
            typename = type_name(param['type'])
            if param['direction'] == 'in':
                in_params.append(f'{param["name"]} : {typename}')
            else:
                out_params.append(f'{param["name"]} : {typename}')

        in_params_spec = "({})".format(", ".join(in_params))
        ri_header += in_params_spec
        if out_params:
            out_params_spec = "({})".format(", ".join(out_params))
            ri_header += out_params_spec
        result.append(ri_header)

    for timer in process.timers:
        result.append(
                f"proc Set_{timer} (Val : {ASN1SCC}T_Uint32): {ASN1SCC}T_Uint32")
        result.append(f"proc Reset_{timer}")
    return result


def procedure_header(proc, SEPARATOR):
    ''' Build the prototype of a procedure '''
    ret_type = type_name(proc.return_type) if proc.return_type else None
    kind = 'proc' # if not proc.return_type else 'func'
    sep = f'p{SEPARATOR}' if not proc.exported else ''
    proc_name = proc.inputString
    pi_header = f'{kind} {sep}{proc_name}'
    if proc.fpar:
        pi_header += '('
        params = []
        for fpar in proc.fpar:
            typename = type_name(fpar['type'])
            if fpar.get('direction') == 'in':
                params.append('{name}: {ptype}'.format(
                        name=fpar.get('name'),
                        ptype=typename))
        pi_header += ','.join(params)
        pi_header += '):'
    if ret_type:
        pi_header += f' {ret_type}'
    return pi_header


def format_nim_code(stmts):
    ''' Indent properly the Nim code ''' # TODO
    indent = 0
    indent_pattern = '   '
    for line in stmts[:-1]:
        elems = line.strip().split()
        if elems and elems[0].startswith(('when', 'end', 'elsif', 'else')):
            indent = max(indent - 1, 0)
        if elems and elems[-1] == 'case;':  # Corresponds to end case;
            indent = max(indent - 1, 0)
        if line:
            yield indent_pattern * indent + line
        if elems and elems[-1] in ('is', 'then', 'loop', 'declare'):
            indent += 1
        if elems and elems[0] in ('begin', 'case', 'else', 'when'):
            indent += 1
        if not elems:  # newline -> decrease indent
            indent -= 1
    yield stmts[-1]
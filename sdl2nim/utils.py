import inspect
import os, sys
import shutil

from sdl2nim import settings
from opengeode import ogAST, ogParser
from opengeode.Helper import find_basic_type as __find_basic_type


__all__ = ['not_implemented_error',
           'var_exists',
           'is_numeric',
           'traceability',
           'is_local',
           'string_payload',
           'array_content',
           'type_name',
           'child_spelling',
           'ia5string_raw',
           'external_ri_list',
           'procedure_header',
           'generate_nim_definitions',
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


def type_name(a_type, use_prefix=True, prefix=settings.ASN1SCC):
    ''' Check the type kind and return a Nim usable type name '''
    if a_type.kind == 'ReferenceType':
        if use_prefix:
            return (prefix + a_type.ReferencedTypeName).replace('-', '_')
        else:
            return (a_type.ReferencedTypeName).replace('-', '_')
    elif a_type.kind == 'BooleanType':
        return 'bool'
    elif a_type.kind.startswith('Integer32'):
        return 'asn1SccSint32'
    elif a_type.kind.startswith('IntegerU8'):
        return 'asn1byte'
    elif a_type.kind.startswith('Integer'):
        if float(a_type.Min) >= 0:
            return 'asn1SccUint'
        else:
            return 'asn1SccSint'
    elif a_type.kind == 'RealType':
        return 'asn1Real'
    elif a_type.kind.endswith('StringType'):
        return 'cstring'
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
    if prim_basic.kind in ('OctetStringType', 'BitStringType'):
        if int(prim_basic.Min) != int(prim_basic.Max):
            payload = f'[0 ..< len({nim_string})]'
        else:
            payload = ''
    elif prim_basic.kind == 'SequenceOfType':
        if int(prim_basic.Min) != int(prim_basic.Max):
            payload = f'.arr[0 ..< {nim_string}.nCount.int32]'
        else:
            payload = f''
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
        rlen = f", nCount: {length}"
    else:
        rlen = ""

    split_vals = values.split(', ')
    first_val = split_vals[0]
    if isinstance(prim, ogAST.PrimStringLiteral):
        first_val = f"{first_val}.byte"
    else:
        try:
            first_val = f"{first_val}.{type_name(asnty.type)}"
        except AttributeError:
            pass
    split_vals[0] = first_val
    return f"[{', '.join(split_vals)}]" # TODO


def child_spelling(name, bty):
    ''' Return the index in Children with the proper spelling (case, dash) '''
    if name in bty.Children:
        return name
    raise TypeError(f'Child not found: {name}')


def ia5string_raw(prim: ogAST.PrimStringLiteral, fill: bool = False):
    ''' IA5 Strings are of type String in Ada but this is not directly
        compatible with variable-length strings as defined in ASN.1
        Since the Ada type maps to a null-terminated C type, we have to make
        a corresponding assignment, filling then non-used part of the container
        with NULL character. To know the size, we can use adaasn1rtl.getStringSize
        '''
    # TODO
    filled = "', '".join(prim.value[1:-1])
    empty = "', '".join([r'\0' for _ in range(101 - len(prim.value[1:-1]))])
    return "['" + filled + ("', '" + empty)*fill +  "']"


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


def generate_nim_definitions(procname: str, path: str):
    if not os.path.isdir(path):
        path = os.path.dirname(path)
    os.system(f'cp {path}/*.asn .')

    files = list(os.listdir())
    for file in files:
        if '-' in file:
            os.rename(file, file.replace('-', '_'))

    asn_files = [os.path.splitext(file)[0] for file in os.listdir() if os.path.splitext(file)[1] == '.asn']
    header_files_str = " ".join([file.split('.')[0] + '.h' for file in asn_files])

    import importlib.resources
    asn1crt = importlib.resources.path('sdl2nim', 'asn1crt.nim')
    with asn1crt as path:
        asn1crt_path = str(path.absolute())

    libdir = "~/.choosenim/toolchains/nim-*/lib/" # TODO: Specify specific nim version to use

    filestr = \
f"""
#
# NimScript build file for {procname}
#

# TODO: Add Release / Debug / Optimization Switches

#
# Tasks
#
task asn, "Generate ASN Files":
    exec "asn1scc --rename-policy 3 -typePrefix {settings.ASN1SCC} -o . -equal -c *.asn"
  
task filegen, "Generate Nim Files":
    asnTask()
    exec "cp {asn1crt_path} . && c2nim --importc {header_files_str}" 

task build, "Build Project":
    filegenTask()
    exec "nim -c --nolinking:on --nimcache:. c {procname}.nim"
    exec "gcc -o {procname}_demo *.c -I {libdir}" 

task clean, "Clean Project Folder":
    exec "rm -rf *.nim && rm -rf *.asn"
    exec "find . ! -name '{procname}_RI.c' -delete"
"""

    with open('config.nims', 'w+') as buildfile:
        buildfile.write(filestr)

    # os.system(' && '.join(commands))

    return


def format_nim_code(stmts):
    ''' Indent properly the Nim code ''' # TODO

    if not hasattr(format_nim_code, 'prev'):
        format_nim_code.prev = ''

    indent = 0
    indent_pattern = ' '*4
    for line in stmts[:-1]:
        elems = line.strip().split()
        if elems and elems[0].startswith('label'):
            if format_nim_code.prev.startswith('# end label'):
                indent = indent
            else:
                indent = max(indent - 1, 0)
        if elems and elems[0].startswith(('elif', 'else', 'of')):
            indent = max(indent - 1, 0)
        if elems and elems[0] == '#' and 'end' in elems[1] and not format_nim_code.prev.startswith('# end label'):
            indent = max(indent - 1, 0)
            if 'case' in elems[-1]:
                indent = max(indent - 1, 0)
        if line:
            yield indent_pattern * indent + line
        if elems and any([char in elems[-1] for char in ('=', ':')]) and "#" not in elems[0]:
            indent += 1
        if elems and elems[0] in ('case', 'else'):
            indent += 1
        if elems and elems[0] in ('return',):
            indent = max(indent - 1, 0)
        if not elems:  # newline -> decrease indent
            indent = max(indent - 1, 0)

        format_nim_code.prev = line
    yield stmts[-1]



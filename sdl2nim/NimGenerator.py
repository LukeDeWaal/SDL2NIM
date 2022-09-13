import inspect
import os
from itertools import product
from multiprocessing.spawn import import_main_path
import re

import logging
import opengeode
from . import settings

from functools import singledispatch
from opengeode import ogAST, Helper

from .utils import not_implemented_error, traceability, ia5string_raw, is_numeric, format_nim_code
from .Expressions import (expression, array_content, find_basic_type, string_payload, type_name, append_size,
                          external_ri_list, procedure_header)

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
                code.extend([f"for i in {range_str}:",
                             f"stdout.write (Character'Val({string}(i)));",
                             "# end loop;"])
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
    ''' Generate the code for a complete process (AST Top level)
            use instance=True to generate the code for a process type instance
            rather than the process type itself.
        '''
    # support generation of code of a process type
    if not instance:
        process.name = process.instance_of_name or process.processName
        generic = process.instance_of_name  # shortcut
        process_instance = process
        process = process.instance_of_ref or process
    else:
        process.name = process.processName
        generic = False
        process_instance = process

    settings.PROCESS_NAME = process.name

    settings.TYPES = process.dataview
    del settings.OUT_SIGNALS[:]
    del settings.PROCEDURES[:]
    settings.OUT_SIGNALS.extend(process.output_signals)
    settings.PROCEDURES.extend(process.procedures)

    for each in settings.PROCEDURES:
        process.random_generator.update(each.random_generator)

    # taste-properties module-specific flag for the Ada backend:
    # import the state data from an external module
    stop_condition = kwargs["ppty_check"] if "ppty_check" in kwargs else ""

    asn1_mods = (f'''"{mod.lower()}"''' for mod in process.asn1Modules)

    # determine if there are context parameters (defined at taste level)
    # they are passed as generic parameters in function type/instances
    has_context_params = any(mod.startswith("Context-")
                             for mod in process.asn1Modules)

    pr = process.name.lower()

    LOG.info(f'Generating Nim code for process {process.name}')

    #  Prepare the AST for code generation (flatten states, etc.)
    no_renames = Helper.code_generation_preprocessing(process)

    if not stop_condition:
        Helper.generate_asn1_datamodel(process)

    for (var_name, content) in process.variables.items():
        # filter out the aliases and put them in the local variable pool
        # to avoid unwanted prefixes when using them
        if var_name in no_renames:
            continue
        if var_name in process.aliases.keys():
            settings.LOCAL_VAR[var_name] = content
        else:
            settings.VARIABLES[var_name] = content

    settings.MONITORS.update(process.monitors)

    process_level_decl = []

    reduced_statelist = {s for s in process.full_statelist
                         if s not in process.parallel_states}

    #  When a signal is sent from the model a call to a function is emitted
    #  This function has to be provided - either by TASTE (kazoo), or by
    #  the user. Opengeode will generate a stub package for this.
    ri_stub_ads = []
    ri_stub_adb = []

    # CHOICE selector types: we created an ASN.1 type to access them
    # (in Helper.generate_asn1_datamodel), but we need conversion functions
    choice_selections = []
    for sortname, sortdef in process.user_defined_types.items():
        if sortdef.type.kind == "EnumeratedType":
            choiceTypeModule = process.mapping_sort_module[sortdef.ChoiceTypeName].replace('-', '_')
            fromMod = f'{choiceTypeModule}.{settings.ASN1SCC}{sortname}'
            toMod = f'{process.name}_Datamodel.{settings.ASN1SCC}{process.name}_{sortname}'
            choice_selections.append(
                f"proc To_{sortname} (Src : {fromMod}) return {toMod} is ({toMod}'Enum_Val (Src'Enum_Rep));")  # TODO

    # Generate the code to declare process-level context
    context_decl = []
    if not stop_condition:
        # but not in stop condition code, since we reuse the context type
        # of the state machine being observed

        ctxt = (f'{settings.LPREFIX} : {settings.ASN1SCC}{process.name.capitalize()}_Context =\n'
                '      (Init_Done: False,\n       ')
        initial_values = []
        # some parts of the context may have initial values
        for var_name, (var_type, def_value) in process.variables.items():
            if var_name in process.aliases.keys():
                # aliases are not part of the context
                continue
            if def_value:
                # Expression must be a ground expression, i.e. must not
                # require temporary variable to store computed result
                dst, dstr, dlocal = expression(def_value)
                varbty = find_basic_type(var_type)

                if varbty.kind.startswith('asn1SccSint') and \
                        isinstance(def_value, (ogAST.PrimOctetStringLiteral,
                                               ogAST.PrimBitStringLiteral)):
                    dstr = str(def_value.numeric_value)

                elif varbty.kind in ('SequenceOfType',
                                     'OctetStringType',
                                     'BitStringType'):
                    dstr = array_content(def_value, dstr, varbty)

                elif varbty.kind == 'IA5StringType':
                    dstr = ia5string_raw(def_value)
                assert not dst and not dlocal, \
                    'DCL: Expecting a ground expression'
                initial_values.append(f'{var_name}: {dstr}')

        if initial_values:
            ctxt += ",\n      ".join(initial_values) + ",\n      "
        # ctxt += "others => <>);"
        context_decl.append(ctxt)

        # Add monitors, that are variables that must be set by an external
        # module. They are not part of the global state of the process, and
        # are used by observer functions to read/write the system state
        # We don't use pointers because that is incompatible with aliases
        for mon_name, (mon_type, _) in process.monitors.items():
            context_decl.append(f"{mon_name} : {type_name(mon_type)};")

        # Add aliases
        for alias_name, (alias_sort, alias_expr) in process.aliases.items():
            if alias_name in no_renames:
                continue
            _, qualified, _ = expression(alias_expr)
            context_decl.append(f"{alias_name} : {type_name(alias_sort)} "
                                f"renames {qualified};")  # TODO

        # Add SDL constants (synonyms)
        for const in process.DV.SDL_Constants.values():
            bkind = find_basic_type(const.type).kind
            if bkind in ('IntegerType', 'RealType', 'EnumeratedType',
                         'BooleanType', 'Integer32Type', 'IntegerU8Type'):
                val = const.value
            else:
                # complex value - must be a ground expression
                _, val, _ = expression(const.value, readonly=1)
                if bkind in ('SequenceOfType', 'OctetStringType', 'BitStringType'):
                    val = array_content(const.value, val, bkind)
                elif bkind == 'IA5StringType':
                    val = ia5string_raw(const.value)
                else:
                    raise f'ERROR: constant {const.varName} value is not a ground expression'

            const_sort = const.type.ReferencedTypeName.replace('-', '_')
            context_decl.append(f"const {const.varName}:  {settings.ASN1SCC}{const_sort} = {val};")

        # The choice selections will allow to use the present operator
        # together with a variable of the -selection type
        context_decl.extend(choice_selections)
    if stop_condition:
        #  code of stop conditions must use the same type as the main process
        context_decl.append(
            f'{settings.LPREFIX} : {settings.ASN1SCC}{stop_condition}_Context \
                        renames {stop_condition}.{stop_condition}_ctxt;')  # TODO

    aggreg_start_proc = []
    start_transition = []
    # Continuous State transition id
    if not instance:
        # CS only is declared in the .ads, so that it can be seen by the simulator
        # process_level_decl.append(f'CS_Only : constant = {len(process.transitions)};')

        for name, val in process.mapping.items():
            # Test val, in principle there is a value but if the code targets
            # generation of properties, the model may have been cleaned up and
            # in that case no value would be set..
            if name.endswith('START') and name != 'START' and val:
                process_level_decl.append(f'const {name} = {str(val)};')

        # Declare start procedure for aggregate states XXX add in C generator
        # should create one START per "via" clause, TODO later
        for name, substates in process.aggregates.items():
            proc_name = f'proc {name}{settings.SEPARATOR}START'
            process_level_decl.append(f'{proc_name};')
            aggreg_start_proc.extend([f'{proc_name} is',])
            aggreg_start_proc.extend(f'Execute_Transition ({subname.statename}{settings.SEPARATOR}START);'
                                     for subname in substates)
            aggreg_start_proc.extend([f'# end {name}{settings.SEPARATOR}START;',
                                      '\n'])

        # Generate the code of the start transition (if process not empty)
        Init_Done = f'{settings.LPREFIX}.Init_Done = true;'
        rand_reset_decl = []
        for rand_g in process.random_generator:
            rand_reset_decl.append(f'Rand_{rand_g}_Pkg.Reset (Gen_{rand_g});');

        start_transition = [
            'proc Startup() = ',
            *rand_reset_decl,
            'Execute_Transition (0);'
            if process.transitions else 'pass',
            Init_Done,
            'return',
            'Startup()']

    # Generate the TASTE template
    try:
        asn1_modules = '\n'.join([f"with {dv.replace('-', '_')};\nuse {dv.replace('-', '_')};"
                                  for dv in process.asn1Modules])
        if process.asn1Modules:
            asn1_modules += '\nwith adaasn1rtl;\nuse adaasn1rtl;'
    except TypeError:
        asn1_modules = '--  No ASN.1 data types are used in this model'

    taste_template = ['### This file was generated automatically by OpenGEODE: DO NOT MODIFY IT !']

    has_cs = any(process.cs_mapping.values())

    generic_spec, instance_decl = "", ""
    if generic:
        generic_spec = "generic\n"
        ri_list = external_ri_list(process)
        if has_context_params:
            # Add context parameter to the process type generics, to make sure
            # the value of the instance is used, not the ASN1 constant of the
            # type.
            generic_spec += f"   {process.name}_ctxt : {settings.ASN1SCC}Context_{process.name};\n"
        if has_cs:
            # For continuous signals the runtime must provide Check_Queue
            generic_spec += "   with procedure Check_Queue (Res : out asn1bool);\n"
        if ri_list:
            generic_spec += "   with " + ";\n   with ".join(ri_list) + ';'
    if instance:
        instance_decl = f"with {process.instance_of_name};"

    # print process.fpar
    # FPAR could be set for Context Parameters. They are available here

    # Generate the source file (.ads) header

    # Stop conditions must import the SDL model they observe
    imp_str = f"with {stop_condition}; use {stop_condition};" \
        if stop_condition else ''

    imp_datamodel = (f"with {process.name}_Datamodel; "
                     f"use {process.name}_Datamodel;") \
        if not stop_condition and not instance else (
        f"with {stop_condition}_Datamodel; "
        f"use {stop_condition}_Datamodel;"
        if stop_condition else "")

    imp_ri = f"with {process.name}_RI;" if not generic else ""

    rand = ('with Ada.Numerics.Discrete_Random;'
            if process.random_generator else "")
    rand_decl = []
    for each in process.random_generator:
        rand_decl.extend([
            f'type Rand_{each}_ty is new asn1SccSint range 1 .. {each};',
            f'package Rand_{each}_Pkg is new  Ada.Numerics.Discrete_Random' \
            f' (Rand_{each}_ty);',
            f'Gen_{each} : Rand_{each}_Pkg.Generator;',
            f'Num_{each} : Rand_{each}_ty;'])

    nim_decl_template = [f'''\
    -- This file was generated automatically by OpenGEODE: DO NOT MODIFY IT !

    {asn1_modules}
    {imp_datamodel}
    {imp_str}
    {imp_ri}
    {instance_decl}
    {rand}
    {generic_spec}'''.strip() + f'''
    package {process.name} with Elaborate_Body is''']

    ri_stub_ads = [f'''\
    --  This file is a stub for the implementation of the required interfaces
    --  It is normally overwritten by TASTE with the actual connection to the
    --  middleware. If you use Opengeode independently from TASTE you must
    --  edit the .adb (body) with your own implementation of these functions.
    --  The body stub will be generated only once.

    {asn1_modules}

    package {process.name}_RI is

       --  In TASTE, used to return the state as char * (but uses malloc so
       --  just return null here - feel free to implement it differently)
       function To_C_Pointer (State_As_String : String) return Chars_Ptr is
          (Null_Ptr);

    ''']
    ri_stub_adb = [f'''--  Stub generated by OpenGEODE.
    --  You can edit this file, it will not be overwritten

    package body {process.name}_RI is''']

    dll_api = []
    nim_decl_template.extend(rand_decl)
    if not instance:
        nim_decl_template.extend(context_decl)

    if not generic and not instance and not stop_condition:
        # Add function allowing to trace current state as a string
        # This uses malloc and should be generated only for Linux
        # when Debug is ON
        nim_decl_template.append(
            f"function Get_State return Chars_Ptr "
            f"is ({process.name.title()}_RI.To_C_Pointer "
            f"({settings.ASN1SCC}{process.name}_States'Image ({settings.LPREFIX}.State)))"
            f" with Export, Convention => C, "
            f'Link_Name => "{process.name.lower()}_state";')

    # Declare procedure Startup in .ads
    if not generic:
        nim_decl_template.append(f'proc Startup'
                            f' with Export, Convention => C,'
                            f' Link_Name => "{process.name}_startup";')
    else:  # function type
        nim_decl_template.append(f'proc Startup;')

    # Generate the code of the procedures
    inner_procedures_code = []
    for proc in process.content.inner_procedures:
        proc_code, proc_local = generate(proc)
        process_level_decl.extend(proc_local)
        inner_procedures_code.extend(proc_code)
        if proc.exported:
            # Exported procedures must be declared in the .ads
            pi_header = procedure_header(proc)
            nim_decl_template.append(f'{pi_header};')
            if not proc.external and not generic:
                # Export for TASTE as a synchronous PI
                prefix = f'p{settings.SEPARATOR}' if not proc.exported else ''
                nim_decl_template.append(
                    f'pragma Export (C, {prefix}{proc.inputString},'
                    f' "{process.name.lower()}_PI_{proc.inputString}");')

    # Generate the code for the process-level variable declarations
    taste_template.extend(process_level_decl)

    # Generate the code of internal operators, if needed
    if process.errorstates or process.ignorestates or process.successstates:
        obs_status = [f'function Observer_State_Status return {settings.ASN1SCC}Observer_State_Kind is',
                      f'(case {settings.LPREFIX}.State is']
        if process.errorstates:
            opts = ' | '.join(f'{settings.ASN1SCC}{st}' for st in process.errorstates)
            obs_status.append(f'  of {opts} => {settings.ASN1SCC}Error_State,')
        if process.ignorestates:
            opts = ' | '.join(f'{settings.ASN1SCC}{st}' for st in process.ignorestates)
            obs_status.append(f'  of {opts} => {settings.ASN1SCC}Ignore_State,')
        if process.errorstates:
            opts = ' | '.join(f'{settings.ASN1SCC}{st}' for st in process.successstates)
            obs_status.append(f'  of {opts} => {settings.ASN1SCC}Success_State,')
        obs_status.append(f'  of others => {settings.ASN1SCC}Regular_State);')
        obs_status.append('\n')
        taste_template.extend(obs_status)

    # Add the code of the procedures definitions
    taste_template.extend(inner_procedures_code)

    # Generate the code of the START procedures of state aggregations
    # XXX to be added to C generator
    taste_template.extend(aggreg_start_proc)

    # Add the code of the DLL interface
    taste_template.extend(dll_api)

    # Generate the code for each input signal (provided interface) and timers
    for signal in process.input_signals + [
        {'name': timer} for timer in process.timers]:
        if stop_condition:
            # dont generate anything in stop_condition functions
            break

        if 'renames' in signal and signal['renames'] is not None:
            # don't generate anything if this is an observer signal
            # (a renames clause for a continuous signal)
            continue

        signame = signal.get('name', 'START')
        fake_name = False

        # Check if there is an exported procedure with the name of the signal
        ignore_export = False
        for proc in process.procedures:
            if proc.inputString.lower() == signame.lower():
                ignore_export = True

        if ignore_export:
            # this signal corresponds to the transitions triggered after
            # exported procedures have been executed (synchronous PIs, or RPS)
            # therefore it is renamed as it is not a regular PI
            fake_name = f'{signame}_Transition'

        if signame == 'START':
            continue
        pi_header = f'proc {fake_name or signame}'
        param_name = signal.get('param_name') or f'{signame}_param'
        # Add (optional) PI parameter (only one is possible in TASTE PI)
        if 'type' in signal:
            typename = type_name(signal['type'])
            pi_header += f'({param_name}: {typename}): {typename}'

        # Add declaration of the provided interface in the .ads file
        nim_decl_template.append(f'--  Provided interface "{signame}"')
        nim_decl_template.append(pi_header + ';')

        if not generic and not ignore_export:
            nim_decl_template.append(
                f'pragma Export(C, {signame},'
                f' "{process.name.lower()}_PI_{signame}");')

        pi_header += ' ='
        taste_template.append(pi_header)
        # taste_template.append('begin')

        def execute_transition(state, dest=[]):
            ''' Generate the code that triggers the transition for the current
                state/input combination '''
            input_def = process.input_mapping[signame].get(state)
            # Check for nested states to call optional exit procedures
            # (we may exit from more than one state, the exit procedures must
            #  be called in the right order)
            state_tree = state.split(settings.SEPARATOR)
            context = process
            exitlist = []
            current = ''
            trans = input_def and process.transitions[input_def.transition_id]
            while state_tree:
                current = current + state_tree.pop(0)
                for comp in context.composite_states:
                    if current.lower() == comp.statename.lower():
                        if comp.exit_procedure:
                            exitlist.append(current)
                        context = comp
                        current = current + settings.SEPARATOR
                        break
            for each in reversed(exitlist):
                # Here we add a call to the exit procedure of nested states
                # when we exit the state due to a transition in the superstate
                # not due to a return statement from within the substate
                # this other case is handled in Helper.py when flattening
                # the model.
                # The exit here is added only for transitions triggered by an
                # INPUT. The continuous signals are not processed here
                if trans and all(each.startswith(trans_st)
                                 for trans_st in trans.possible_states):
                    dest.append(f'p{settings.SEPARATOR}{each}{settings.SEPARATOR}exit;')

            if input_def:
                for inp in input_def.parameters:
                    # Assign the (optional and unique) parameter
                    # to the corresponding process variable
                    dest.append(f'{settings.LPREFIX}.{inp} = {param_name};')
                # Execute the corresponding transition
                if input_def.transition:
                    dest.append(f'Execute_Transition ({input_def.transition_id});')
                else:
                    # taste_template.append('Execute_Transition (CS_Only);')
                    # removed: CS_Only in "when others" branch
                    return False
            else:
                return False
                # removed: CS_Only in "when others" branch
                # taste_template.append('Execute_Transition (CS_Only);')
            return True

        if not instance:
            taste_template.append(f'case {settings.LPREFIX}.state:')

        def case_state(state):
            ''' Recursive function (in case of state aggregation) to generate
                the code that calls the proper transition according
                to the current state
                The input name is in signame
            '''
            if state.endswith('START'):
                return
            # taste_template.append(f'when {settings.ASN1SCC}{state} =>')
            statecase = [f'of {settings.ASN1SCC}{state}:']
            input_def = process.input_mapping[signame].get(state)
            if state in process.aggregates.keys():
                taste_template.extend(statecase)
                # State aggregation:
                # - find which substate manages this input
                # - add a switch case on the corresponding substate
                taste_template.append('#  This is a state aggregation')
                for sub in process.aggregates[state]:
                    if [a for a in sub.mapping.keys()
                        if a in process.input_mapping[signame].keys()]:
                        taste_template.append('case '
                                              f'{settings.LPREFIX}.{sub.statename}{settings.SEPARATOR}state:')
                        for par in sub.mapping.keys():
                            case_state(par)
                        taste_template.append('else:')
                        taste_template.append('Execute_Transition (CS_Only);')
                        taste_template.append('# end case;')
                        break
                else:
                    # Input is not managed in the state aggregation
                    if input_def:
                        # check if it is managed one level above
                        execute_transition(state, taste_template)
                    else:
                        taste_template.append('Execute_Transition (CS_Only);')
            else:
                if execute_transition(state, statecase):
                    taste_template.extend(statecase)

        if not instance:
            for each_state in reduced_statelist:
                case_state(each_state)
            taste_template.append('else:')
            taste_template.append('Execute_Transition (CS_Only);')
            taste_template.append('# end case;')
        else:
            inst_call = f"{process.name}_Instance.{signame}"
            if 'type' in signal:
                inst_call += f" ({param_name})"
            taste_template.append(f"{inst_call};")

        taste_template.append(f'return {param_name}')
        taste_template.append(f'# end {fake_name or signame};')
        taste_template.append('\n')

    #  add call to startup function for instances
    if instance:
        taste_template.extend(['proc Startup() =',
                               f'{process.name}_Instance.Startup;',
                               'return',
                               '# end Startup;',
                               ''])

    # for the .ads file, generate the declaration of the required interfaces
    # output signals are the asynchronous RI - only one parameter
    for signal in process.output_signals:
        sig = signal['name']
        param_name = signal.get('param_name') or f'{sig}_param'
        # Add (optional) RI parameter
        param_spec = ''
        if 'type' in signal:
            typename = type_name(signal['type'])
            param_spec = f' ({param_name} : {typename}): {typename}'
        if not generic:
            nim_decl_template.append('###  {}equired interface "{}"'
                                .format("Paramless r" if not 'type' in signal
                                        else "R", sig))

            if not instance:
                nim_decl_template.append(f'procedure RI{settings.SEPARATOR}{sig}{param_spec} '
                                    f'renames {process.name}_RI.{sig};')
            ri_stub_ads.append(f'proc {sig}{param_spec};')
            ri_stub_adb.extend([f'proc {sig}{param_spec} =', 'pass'])
            #  TASTE generates the pragma import in <function>_ri.ads
            #  therefore do not generate it in the .ads
            # nim_decl_template.append(f'pragma Import (C, RI{settings.SEPARATOR}{sig}, "{process.name.lower()}_RI_{sig}");')

    # for the .ads file, generate the declaration of the external procedures
    for proc in (proc for proc in process.procedures if proc.external):
        sig = proc.inputString
        ri_header = f'procedure RI{settings.SEPARATOR}{sig}'
        in_params = []
        out_params = []
        params_spec = ""
        for param in proc.fpar:
            typename = type_name(param['type'])
            name = param['name']
            if param['direction'] == 'in':
                in_params.append(f'{name} : {typename}')
            else:
                out_params.append(f'{typename}')

        if in_params:
            params_spec = f' ({", ".join(in_params)})'
            ri_header += params_spec
        if out_params:
            params_spec += ':' + f' ({", ".join(out_params)})'
            ri_header += params_spec

        if not generic:
            if not instance:
                # Type and instance do not need this declarations, only standalone
                # processes.
                nim_decl_template.append(f'--  Synchronous Required Interface "{sig}"')
                nim_decl_template.append(f'{ri_header} renames {process.name}_RI.{sig};')
            ri_stub_ads.append(f'proc {sig}{params_spec};')
            ri_stub_adb.extend([f'proc {sig}{params_spec} =', 'pass'])

    # for the .ads file, generate the declaration of timers set/reset functions
    for timer in process.timers:
        if stop_condition:
            # don't generate timer code for stop conditions
            break
        nim_decl_template.append(f'###  Timer {timer} SET and RESET functions')

        if not generic:
            procname = process.name.lower()
            nim_decl_template.extend([
                f'proc SET_{timer} (Val : {settings.ASN1SCC}T_UInt32): {settings.ASN1SCC}T_UInt32 =',
                f'return {process.name}_RI.Set_{timer}(Val)'])
            ri_stub_ads.append(f'proc SET_{timer} (Val : {settings.ASN1SCC}T_UInt32): {settings.ASN1SCC}T_UInt32;')
            ri_stub_adb.extend([f'proc SET_{timer} (Val : {settings.ASN1SCC}T_UInt32): {settings.ASN1SCC}T_UInt32 =', 'pass'])
            nim_decl_template.extend([f'proc RESET_{timer}() =',
                                f'return {process.name}_RI.Reset_{timer}()'])
            ri_stub_ads.append(f'proc RESET_{timer};')
            ri_stub_adb.extend([f'proc RESET_{timer}() =', 'pass'])
        else:
            # Generic functions get the SET and RESET from template
            pass

    if instance:
        # Instance of a process type, all the RIs (including timers) must
        # be gathered to instantiate the package
        pkg_decl = (f"package {process.name}_Instance is new {process.instance_of_name}")
        ri_list = [(f"RI{settings.SEPARATOR}{sig['name']}", sig['name'])
                   for sig in process.output_signals]
        if has_cs:
            ri_list.append(("Check_Queue", "Check_Queue"))
        ri_list.extend([(f"RI{settings.SEPARATOR}{proc.inputString}", proc.inputString)
                        for proc in process.procedures if proc.external])
        ri_list.extend([(f"set_{timer}", f"set_{timer}") for timer in process.timers])
        ri_list.extend([(f"reset_{timer}", f"reset_{timer}") for timer in process.timers])
        ri_inst = [f"{ri[0]} => {process.name.title()}_RI.{ri[1]}" for ri in ri_list]
        if ri_inst or has_context_params:
            pkg_decl += " ("
        if ri_inst:
            pkg_decl += f'{", ".join(ri_inst)}'
        if has_context_params:
            if ri_inst:
                pkg_decl += ", "
            # Add instance-value of the context parameters
            pkg_decl += f"{process.instance_of_name}_ctxt => {process.name}_ctxt"
        if ri_inst or has_context_params:
            pkg_decl += ")"
        nim_decl_template.append(f"{pkg_decl};")
        nim_decl_template.append(
            f"function Get_State return chars_ptr "
            f"is ({process.name}_RI.To_C_Pointer ({process.name}_Instance.{settings.LPREFIX}.State'Img))"
            f" with Export, Convention => C, "
            f'Link_Name => "{process.name.lower()}_state";')

        # Expose Execute_Transition, needed by the simulator to execute continuous signals
        nim_decl_template.extend([
            f'proc Execute_Transition (Id : asn1SccSint) =',
            f'{process.name}_Instance.Execute_Transition(Id)',
            'return'])
        nim_decl_template.append(f'const CS_Only = {process.name}_Instance.CS_Only;')

    else:
        nim_decl_template.append(f'proc Execute_Transition (Id : asn1SccSint);')
        nim_decl_template.append(f'const CS_Only = {len(process.transitions)};')

    # Transform inner labels to floating labels
    Helper.inner_labels_to_floating(process)

    # Generate the code for all transitions
    code_transitions = []
    local_decl_transitions = []
    for proc_tr in process.transitions:
        code_tr, tr_local_decl = generate(proc_tr)
        code_transitions.append(code_tr)
        local_decl_transitions.extend(tr_local_decl)

    # Generate code for the floating labels
    code_labels = []
    for label in process.content.floating_labels:
        code_label, label_decl = generate(label)
        local_decl_transitions.extend(label_decl)
        code_labels.extend(code_label)

    # Generate the code of the Execute_Transition  procedure, if needed
    if process.transitions and not instance:
        taste_template.append('proc Execute_Transition (Id : asn1SccSint) =')
        taste_template.append('trId : asn1SccSint = Id;')
        if has_cs:
            taste_template.append('Message_Pending : asn1bool = true;')

        # Declare the local variables needed by the transitions in the template
        taste_template.extend(set(local_decl_transitions))
        # taste_template.append('begin')

        # Generate a loop that ends when a next state is reached
        # (there can be chained transition when entering a nested state)
        taste_template.append('while (trId != -1):')

        # Generate the switch-case on the transition id
        taste_template.append('case trId:')

        for idx, val in enumerate(code_transitions):
            taste_template.append('of {idx}:'.format(idx=idx))
            val = ['{line}'.format(line=l) for l in val]
            if val:
                taste_template.extend(val)
            else:
                taste_template.append('pass')

        taste_template.append('of CS_Only:')
        taste_template.append('trId = -1;')
        taste_template.append('goto Continuous_Signals;')

        taste_template.append('else:')
        taste_template.append('pass')

        taste_template.append('# end case;')
        if code_labels:
            # Due to nested states (chained transitions) jump over label code
            # (NEXTSTATEs do not return from Execute_Transition)
            taste_template.append('goto Continuous_Signals;')

        # Add the code for the floating labels
        taste_template.extend(code_labels)

        taste_template.append('label Continuous_Signals: pass')

        # After completing active transition(s), check continuous signals:
        #     - Check current state(s)
        #     - For each continuous signal generate code (test+transition)
        # XXX add to C backend
        if has_cs:
            if not settings.MONITORS:
                taste_template.append('#  Process continuous signals')
                taste_template.append(f'if {settings.LPREFIX}.Init_Done:')
                taste_template.append("Check_Queue (Message_Pending);")
                taste_template.append('# end if;')
                if not generic:  # not a function type
                    nim_decl_template.append('proc Check_Queue(): asn1bool')
                    # nim_decl_template.append(f'with Import, Convention => C, '
                                        # f'Link_Name => "{process.name.lower()}_check_queue";')
            else:
                taste_template.append('#  Process observer transitions')
                taste_template.append("Message_Pending = False;")
        if has_cs:
            taste_template.extend(['if Message_Pending or trId != -1 :',
                                   'goto Next_Transition;',
                                   '# end if;'])
        # else:
        #    taste_template.append('null;')

        # Process the continuous signals in state aggregations first
        # (reminder: state aggregations = parallel states)
        done = []
        sep = 'if '
        last = ''
        # flag indicating there are CS in nested states but not at root
        need_final_endif = False
        first_of_aggreg = True
        for cs, agg in product(process.cs_mapping.items(),
                               process.aggregates.items()):
            (statename, cs_item) = cs
            (agg_name, substates) = agg

            if not cs_item:
                continue
            for each in substates:
                if statename in each.cs_mapping and each.cs_mapping[statename]:
                    if first_of_aggreg:
                        taste_template.append(
                            f'if {settings.LPREFIX}.State == {settings.ASN1SCC}{agg_name}:')
                        first_of_aggreg = False
                    need_final_endif = True
                    taste_template.append(
                        f'if {settings.LPREFIX}.{each.statename}{settings.SEPARATOR}State == '
                        f'{settings.ASN1SCC}{statename}:')
                    # Change priority 0 (no priority set) to lowest priority
                    lowest_priority = max(item.priority for item in cs_item)
                    for each in cs_item:
                        if each.priority == 0:
                            each.priority = lowest_priority + 1
                    for provided_clause in sorted(cs_item,
                                                  key=lambda itm: itm.priority):
                        taste_template.append(f'#  Priority {provided_clause.priority}')
                        trId = process.transitions.index \
                            (provided_clause.transition)
                        code, loc = generate(provided_clause.trigger,
                                             branch_to=trId,
                                             sep=sep, last=last)
                        code.append('goto Next_Transition;')
                        sep = 'elif '
                        taste_template.extend(code)
                    done.append(statename)
                    taste_template.append('# end if;')  # inner if
                    taste_template.append('# end if;')  # substate if
                    sep = 'if '
                    break

        for statename in process.cs_mapping.keys() - done:
            cs_item = process.cs_mapping[statename]
            if cs_item:
                need_final_endif = False
                first = "el" if done else ""
                taste_template.append(
                    f'{first}if {settings.LPREFIX}.State = {settings.ASN1SCC}{statename}'
                    ' then')
            # Change priority 0 (no priority set) to lowest priority
            if cs_item:
                lowest_priority = max(item.priority for item in cs_item)
            for each in cs_item:
                if each.priority == 0:
                    each.priority = lowest_priority + 1
            for provided_clause in sorted(cs_item,
                                          key=lambda itm: itm.priority):
                taste_template.append(f'#  Priority: {provided_clause.priority}')
                trId = process.transitions.index(provided_clause.transition)

                # check if we are leaving a nested state with a CS
                state_tree = statename.split(settings.SEPARATOR)
                context = process
                exitlist, exitcalls = [], []
                current = ''
                while state_tree:
                    current = current + state_tree.pop(0)
                    for comp in context.composite_states:
                        if current.lower() == comp.statename.lower():
                            if comp.exit_procedure:
                                exitlist.append(current)
                            context = comp
                            current = current + settings.SEPARATOR
                            break
                trans = process.transitions[trId]
                for each in reversed(exitlist):
                    if trans and all(each.startswith(trans_st)
                                     for trans_st in trans.possible_states):
                        exitcalls.append(f"p{settings.SEPARATOR}{each}{settings.SEPARATOR}exit;")

                code, loc = generate(provided_clause.trigger,
                                     branch_to=trId, sep=sep, last=last,
                                     exitcalls=exitcalls)
                sep = 'elif '
                taste_template.extend(code)
            if cs_item:
                taste_template.append('# end if;')  # inner if
                taste_template.append('# end if;')  # current state
            sep = 'if '

        if need_final_endif:
            taste_template.append('# end if;')

        taste_template.append('label Next_Transition: pass')
        taste_template.append('# end loop;')
        # taste_template.append('# end Execute_Transition;')
        taste_template.append('return')
        taste_template.append('\n')
    elif not instance:
        # No transitions defined, but keep the interface for CS_Only calls
        taste_template.append('proc Execute_Transition (Id : asn1SccSint) = pass;')
        taste_template.append('\n')

    # Add code of the package elaboration
    taste_template.extend(start_transition)
    taste_template.append(f'# end {process.name};')

    nim_decl_template.append(f'# end {process.name};')

    ri_stub_ads.append(f'# end {process.name}_RI;')
    ri_stub_adb.append(f'# end {process.name}_RI;')

    with open(process.name.lower() + os.extsep + 'nim', 'wb') as nim_file:
        code = '\n'.join(format_nim_code(taste_template)).encode('latin1')
        nim_file.write(code)

    with open(process.name.lower() + os.extsep + 'ads', 'wb') as nim_file:
        nim_file.write(
            '\n'.join(format_nim_code(nim_decl_template)).encode('latin1'))

    if not taste:
        with open(f"{process.name.lower()}_ri.ads", "wb") as ri_stub:
            ri_stub.write("\n".join(format_nim_code(ri_stub_ads)).encode('latin1'))
        stub_adb = f'{process.name.lower()}_ri.adb'
        # don't overwrite adb as it may contain user code
        # also don't generate if there are no RI in the system
        if not os.path.exists(stub_adb) and len(ri_stub_adb) > 2:
            with open(stub_adb, "wb") as ri_stub:
                ri_stub.write("\n".join(format_nim_code(ri_stub_adb)).encode('latin1'))

    # with open(f"{process.name.lower()}_ada.gpr", "wb") as gprada:
    #     gprada.write(ada_gpr.encode('utf-8'))

    if process_instance is not process:
        # Generate an instance of the process type, too.
        # First copy the list of timers to the instance (otherwise the
        # instance would miss some PIs and RIs to set the actual timers)
        process_instance.timers = process.timers
        # And for the same reason copy the continuous states, needed to
        # determine if Check_Queue is needed
        process_instance.cs_mapping = process.cs_mapping
        generate(process_instance, simu, instance=True, taste=taste)


@generate.register(ogAST.Output)
@generate.register(ogAST.ProcedureCall)
def _call_external_function(output, **kwargs) -> str:
    ''' Generate the code of a set of output or procedure call statement '''
    code = []
    local_decl = []

    # Add the traceability information
    code.extend(traceability(output))
    # code.extend(debug_trace())

    # Calling a procedure or RI usually needs a prefix (RI_.. or p_...)
    # Exception is the _Transition procedures called after exported PIs (RPC)
    need_prefix = True

    for out in output.output:
        signal_name = out['outputName']
        list_of_params = []

        if signal_name.lower() in ('write', 'writeln'):
            # special built-in SDL procedure for printing strings
            # supports printing of native types (int, real, bool)
            # but not yet complex ASN.1 structures (sequence/seqof/choice)
            for param in out['params'][:-1]:
                stmts, _, local = write_statement(param, newline=False)
                code.extend(stmts)
                local_decl.extend(local)
            for param in out['params'][-1:]:
                # Last parameter - add newline if necessary
                stmts, _, local = write_statement(param, newline=True if
                signal_name.lower() == 'writeln' else False)
                code.extend(stmts)
                local_decl.extend(local)
            continue
        elif signal_name.lower() == 'reset_timer':
            # built-in operator for resetting timers. param = timer name
            param, = out['params']
            p_code, p_id, p_local = expression(param, readonly=1)
            code.extend(p_code)
            local_decl.extend(p_local)
            code.append(f'RESET_{p_id};')
            continue
        elif signal_name.lower() == 'set_timer':
            # built-in operator for setting a timer: SET(1000, timer_name)
            timer_value, timer_id = out['params']
            t_code, t_val, t_local = expression(timer_value)
            p_code, p_id, p_local = expression(timer_id)
            code.extend(t_code)
            code.extend(p_code)
            local_decl.extend(t_local)
            local_decl.extend(p_local)
            # Use a temporary variable to store the timer value
            tmp_id = 'tmp' + str(out['tmpVars'][0])
            local_decl.append(f'{tmp_id} : {settings.ASN1SCC}T_UInt32;')
            code.append(f'{tmp_id} = {t_val};')
            code.append(f"SET_{p_id} ({tmp_id});")
            continue
        proc, out_sig = None, None
        try:
            out_sig, = [sig for sig in settings.OUT_SIGNALS
                        if sig['name'].lower() == signal_name.lower()]
        except ValueError:
            # Not an output, try if it is an external or inner procedure
            try:
                proc, = [sig for sig in settings.PROCEDURES
                         if sig.inputString.lower() == signal_name.lower()]
                if proc.external:
                    out_sig = proc
            except ValueError:
                # Last chance to find it: if it is an exported procedure,
                # in that case an additional signal with _Transition suffix
                # exists but is not visible in the model at this point
                for sig in settings.PROCEDURES:
                    if signal_name.lower() == f'{sig.inputString.lower()}_transition':
                        out_sig = sig
                        need_prefix = False
                        break
                else:
                    # Not there? Impossible, the parser would have barked
                    # Can happen with stop conditions because they are defined
                    # as exported but the _Transition signal was not added
                    LOG.warning(f'Could not find signal/procedure: {signal_name} - ignoring call')
                    return code, local_decl
        if out_sig:
            for idx, param in enumerate(out.get('params') or []):
                param_direction = 'in'
                try:
                    # If it is an output, there is a single parameter
                    param_type = out_sig['type']
                except TypeError:
                    # Else if it is a procedure, get the type
                    param_type = out_sig.fpar[idx]['type']
                    param_direction = out_sig.fpar[idx]['direction']

                typename = type_name(param_type)
                p_code, p_id, p_local = expression(param, readonly=1)
                code.extend(p_code)
                local_decl.extend(p_local)
                # Create a temporary variable for input parameters only
                # (If needed, i.e. if argument is not a local variable)
                if param_direction == 'in' \
                        and (not (isinstance(param, ogAST.PrimVariable)
                                  and p_id.startswith(settings.LPREFIX))  # NO FIXME WITH CTXT
                             or isinstance(param, ogAST.PrimFPAR)):
                    tmp_id = f'tmp{out["tmpVars"][idx]}'
                    # local_decl.extend(debug_trace())
                    local_decl.append(f'{tmp_id} : {typename};')
                    basic_param = find_basic_type(param_type)
                    if basic_param.kind.startswith('asn1SccSint'):
                        p_id = f"{typename} ({p_id})"
                    if isinstance(param,
                                  (ogAST.PrimSequenceOf, ogAST.PrimStringLiteral)):
                        if basic_param.kind == 'IA5StringType':
                            p_id = ia5string_raw(param)
                        elif basic_param.kind.startswith('asn1SccSint'):
                            p_id = str(param.numeric_value)
                        else:
                            p_id = array_content(param, p_id, basic_param)

                    if isinstance(param, ogAST.ExprAppend):
                        # Process Append constructs properly when they are
                        # used as raw params (e.g. callme(a//b//c))
                        # TODO: ogAST.PrimSubstring seem to be missing
                        # Check the template in def _conditional
                        app_len = append_size(param)
                        # code.extend(debug_trace())
                        code.append(f'{tmp_id}[0 ..< {app_len}] = {p_id};')
                        if basic_param.Min != basic_param.Max:
                            # Append should only apply to this case, i.e.
                            # types of varying length...
                            # code.append(f'{tmp_id}.len = {app_len};')
                            pass
                    else:
                        code.append(f'{tmp_id} = {p_id};')
                    list_of_params.append(tmp_id)
                else:
                    # Output parameters/local variables
                    list_of_params.append(p_id)
            name = out["outputName"]
            if list_of_params:
                params = ', '.join(list_of_params)
                code.append(f'RI{settings.SEPARATOR}{name}({params});')

            else:
                prefix = f'RI{settings.SEPARATOR}' if need_prefix else ''
                code.append(f'{prefix}{name};')
        else:
            # inner procedure call without a RETURN statement
            # retrieve the procedure signature
            ident = proc.inputString
            p, = [p for p in settings.PROCEDURES if p.inputString.lower() == ident.lower()]

            list_of_params = []
            for idx, param in enumerate(out.get('params', [])):
                # Expected basic type of the parameter
                param_type = p.fpar[idx]['type']
                basic_param = find_basic_type(param_type)

                p_code, p_id, p_local = expression(param, readonly=1)

                # We need to format strings properly, this depends on the expected
                # type of the procedure parameter
                if isinstance(param,
                              (ogAST.PrimSequenceOf, ogAST.PrimStringLiteral)):
                    if basic_param.kind == 'IA5StringType':
                        p_id = ia5string_raw(param)
                    elif basic_param.kind.startswith('asn1SccSint'):
                        p_id = str(param.numeric_value)
                    else:
                        p_id = array_content(param, p_id, basic_param)

                code.extend(p_code)
                local_decl.extend(p_local)
                # no need to use temporary variables, we are in pure Ada
                list_of_params.append(p_id)
            if list_of_params:
                code.append(f'p{settings.SEPARATOR}{proc.inputString}({", ".join(list_of_params)});')
            else:
                code.append(f'p{settings.SEPARATOR}{proc.inputString};')
    return code, local_decl


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
    local_scope = dict(settings.LOCAL_VAR)
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
            settings.LOCAL_VAR.update({loop['var']: (loop['type'], None)})
        else:
            # case of form: FOR x in SEQUENCE OF
            # Add iterator to the list of local variables
            settings.LOCAL_VAR.update({loop['var']: (loop['type'], None)})

            list_stmt, list_str, list_local = expression(loop['list'])
            basic_type = find_basic_type(loop['list'].exprType)
            list_payload = list_str + string_payload(loop['list'], list_str)

            stmt.extend(list_stmt)
            local_decl.extend(list_local)
            stmt.extend([f'for {loop["var"]}_idx, {loop["var"]}_val in {list_payload}:', ])
        try:
            code_trans, local_trans = generate(loop['transition'])
            if local_trans:
                stmt.extend(set(local_trans))
            stmt.extend(code_trans)
        except AttributeError:
            stmt.append('pass')
        stmt.append('# end loop;')
    # Restore list of local variables
    settings.LOCAL_VAR.clear()
    settings.LOCAL_VAR.update(local_scope)
    return stmt, local_decl


@generate.register(ogAST.Decision)
def _decision(dec, branch_to=None, sep='if ', last='end if;', exitcalls=[], **kwargs):
    not_implemented_error()


@generate.register(ogAST.Label)
def _label(lab, **kwargs):
    """
    Nim doesnt directly support goto statements.
    Need some sort of workaround: https://github.com/sls1005/nim-goto/tree/main
    """
    stmt, local_decl = [], []
    return [f'goto {lab.inputString}'], []


@generate.register(ogAST.Transition)
def _transition(tr, **kwargs):
    ''' generate the code for a transition '''
    code, local_decl = [], []
    empty_transition = all(isinstance(act, ogAST.TaskInformalText)
                           for act in tr.actions)
    for action in tr.actions:
        stmt, local_var = generate(action)
        code.extend(stmt)
        local_decl.extend(local_var)
        if isinstance(action, ogAST.Label):
            break
    else:
        if tr.terminator:
            ns = tr.terminator.inputString.strip()
            empty_transition = False
            code.extend(traceability(tr.terminator))
            if tr.terminator.label:
                code.append(f'label {ns}: pass')
            if tr.terminator.kind == 'next_state':
                history = ns in ('-', '-*')
                if tr.terminator.next_is_aggregation and not history:  # XXX add to C generator
                    code.append(f'# Entering state aggregation {tr.terminator.inputString}')
                    # First change the state (to avoid looping in continuous signals since
                    # they will be evaluated after the start transition ; if the state is
                    # still the old state, there is a risk of infinite recursion)
                    if not tr.terminator.substate:
                        code.append(
                            f'{settings.LPREFIX}.State = {settings.ASN1SCC}{tr.terminator.inputString};')
                    else:
                        # We may be already in a substate
                        code.append(f'{settings.LPREFIX}.{tr.terminator.substate}{settings.SEPARATOR}State ='
                                    f' {settings.ASN1SCC}{tr.terminator.inputString};')
                    # Call the START function of the state aggregation
                    code.append(f'{tr.terminator.next_id};')
                    code.append('trId = -1;')
                elif not history:
                    code.append(f'trId = {str(tr.terminator.next_id)};')
                    if tr.terminator.next_id == -1:
                        if not tr.terminator.substate:  # XXX add to C generator
                            code.append(f'{settings.LPREFIX}.State = {settings.ASN1SCC}{tr.terminator.inputString};')
                        else:
                            code.append(f'{settings.LPREFIX}.{tr.terminator.substate}{settings.SEPARATOR}State ='
                                        f' {settings.ASN1SCC}{tr.terminator.inputString};')
                else:
                    # "nextstate -": switch case to re-run the entry transition
                    # in case of a composite state or state aggregation
                    # and "nextstate -*" to return to the previous state
                    # (parallel states only, not composite states at the moment
                    # as the previous state is not stored)
                    if ns != "-*" and any(next_id
                                          for next_id in tr.terminator.candidate_id.keys()
                                          if next_id != -1):
                        code.append(f'case {settings.LPREFIX}.State:')
                        for nid, sta in tr.terminator.candidate_id.items():
                            if nid != -1:
                                if tr.terminator.next_is_aggregation:
                                    statement = ns != '-*' and f'{nid};' or 'trId = -1;'
                                else:
                                    statement = f'trId = {nid};'
                                states_prefix = (f"{settings.ASN1SCC}{s}" for s in sta)
                                joined_states = " | ".join(states_prefix)
                                code.extend(
                                    [f'when {joined_states}:',
                                     statement])

                        code.extend(['else:',
                                     'trId = -1;',
                                     '# end case;'])
                    else:
                        code.append('trId = -1;')
                code.append('goto Continuous_Signals;')
            elif tr.terminator.kind == 'join':
                code.append(f'goto {tr.terminator.inputString};')
            elif tr.terminator.kind == 'stop':
                pass
                # TODO
            elif tr.terminator.kind == 'return':
                string = ''
                aggregate = False
                if tr.terminator.substate:  # XXX add to C generator
                    aggregate = True
                    # within a state aggregation, a return means that one
                    # of the parallel states becomes disabled, but it does
                    # not mean that the whole state aggregation can be
                    # exited. We must set this substate to a "finished"
                    # state until all the substates are returned. Then only
                    # call the overall state aggregation exit procedures.
                    code.append(
                        f'{settings.LPREFIX}.{tr.terminator.substate}{settings.SEPARATOR}State '
                        f'= {settings.ASN1SCC}state{settings.SEPARATOR}end;')
                    cond = '{ctxt}.{sib}{sep}State = {asn1scc}state{sep}end'
                    conds = [cond.format(sib=sib,
                                         ctxt=settings.LPREFIX,
                                         sep=settings.SEPARATOR,
                                         asn1scc=settings.ASN1SCC)
                             for sib in tr.terminator.siblings
                             if sib.lower() != tr.terminator.substate.lower()]
                    code.append(f'if {" and ".join(conds)}:')
                if tr.terminator.next_id == -1:
                    retexp = tr.terminator.return_expr
                    if retexp:
                        stmts, string, local = expression(retexp, readonly=1)

                        # Check the return type in case of a procedure, in
                        # case it is a string - to format it properly
                        if isinstance(tr.terminator.context, ogAST.Procedure):
                            basic = find_basic_type(tr.terminator.context.return_type)
                            if isinstance(retexp,
                                          (ogAST.PrimSequenceOf, ogAST.PrimStringLiteral)):
                                if basic.kind == 'IA5StringType':
                                    string = ia5string_raw(retexp)
                                elif basic.kind.startswith('asn1SccSint'):
                                    string = str(retexp.numeric_value)
                                else:
                                    string = array_content(retexp, string, basic)

                        code.extend(stmts)
                        local_decl.extend(local)
                    code.append(f'return{" " + string if string else ""};')
                else:
                    code.append(f'trId =  {str(tr.terminator.next_id)};')
                    code.append('goto Continuous_Signals;')
                if aggregate:
                    code.append('else')
                    code.append('trId = -1;')
                    code.append('goto Continuous_Signals;')
                    code.append('# end if;')
    if empty_transition:
        # If transition does not have any statement, generate an Ada 'null;'
        code.append('pass')
    return code, local_decl


@generate.register(ogAST.Floating_label)
def _floating_label(label, **kwargs):
    ''' Generate the code for a floating label (label + transition) '''
    code = []
    local_decl = []
    # Add the traceability information
    code.extend(traceability(label))
    code.append(f'label {label.inputString}: pass')
    if label.transition:
        code_trans, local_trans = generate(label.transition)
        code.extend(code_trans)
        local_decl.extend(local_trans)
    else:
        code.append('return;')
    return code, local_decl


@generate.register(ogAST.Procedure)
def _inner_procedure(proc, **kwargs):
    not_implemented_error()
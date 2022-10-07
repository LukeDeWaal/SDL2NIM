

# C2Nim does not handle constants well; need to parse through file manually to find and define them
# by replacing their Nim var declarations with Nim const definitions.

def constant_extraction_manual(filename: str):
    # Find constant names
    with open(filename + '.h', 'r') as hfile:
        h_lines = hfile.readlines()

    constants = {}
    for line in h_lines:
        if line.startswith('extern const'):
            constants[line.split()[-1].strip("\n ;")] = ""
    
    # Find constant values
    with open(filename + '.c', 'r') as cfile:
        c_lines = cfile.readlines()

    for line in c_lines:
        if line.startswith('const'):
            splitvals = line.split()
            # Expect 5 values, eg: const asn1SccByteUnsigned fullbyte = 255;
            # Possibly more if is string with spaces, need to recombine those
            if len(splitvals) > 5:
                splitvals[4] = ' '.join(splitvals[4:])
                del splitvals[5:]
            name, value = splitvals[2], splitvals[4].strip('\n ;')
            if name not in constants.keys():
                raise KeyError(f"Expected {name} to be present")
            constants[name] = value
    
    # Write constants into nim file
    with open(filename + '.nim', 'r+') as nimfile:
        nim_lines = nimfile.readlines()
        
        made_changes = False
        for idx, line in enumerate(nim_lines):
            if line.startswith('var'):
                split_line = line.split()
                for const_name, const_val in constants.items():
                    if const_name == split_line[1].strip('\n :*;'):
                        typename = split_line[-1].strip('\n :*;')
                        nim_lines[idx] = f"const {const_name}*: {typename} = {const_val}"
                        made_changes = True
                    else:
                        continue
                        
        if made_changes:
            nimfile.seek(0)
            nimfile.writelines(nim_lines)
            nimfile.truncate()
    
    return constants


def constant_extraction(filename: str):
    # Find constant names
    with open(filename + '.h', 'r') as hfile:
        h_lines = hfile.readlines()

    constants = []
    for line in h_lines:
        if line.startswith('extern const'):
            constants.append(line.split()[-1].strip("\n ;"))

    # Write constants into nim file
    with open(filename + '.nim', 'r+') as nimfile:
        nim_lines = nimfile.readlines()

        made_changes = False

        for idx, line in enumerate(nim_lines):
            if line.startswith('var'):
                splitline = line.split()
                name = splitline[1].strip("\n *;:")
                typename = splitline[2].strip("\n *;:")
                if name in constants:
                    newline = f"var {name}*{{.importc: \"{name}\".}}: {typename}" # header: \"{filename}.c\",
                    nim_lines[idx] = newline
                    made_changes = True

        if made_changes:
            nimfile.seek(0)
            nimfile.writelines(nim_lines)
            nimfile.truncate()

    return constants


if __name__ == "__main__":
    import glob, os, argparse

    parser = argparse.ArgumentParser()
    parser.add_argument('--dir', dest='dir', action='store', type=str,
                        default=os.path.abspath('.'), help='set directory (default .)')
    args = parser.parse_args()
    
    constants = []
    print(args.dir, glob.glob(f"{args.dir}/*.h"))
    for file in glob.glob(f"{args.dir}/*.h"):
        name, _ = os.path.splitext(file)
        if os.path.split(name)[-1] == 'asn1crt': continue
        lst = [f"{name}.{ext}" in glob.glob(f"{args.dir}/*.{ext}") for ext in ('c', 'nim')]
        print(file, lst)
        if all(lst):
            constants.extend(constant_extraction(name))

    if constants:
        print("Found the following constants: ", end='\n\t| ')
        for k in constants[:-1]:
            print(f"{k:<15}", end='\n\t| ')
        print(f"{constants[-1]:<15}")
    else:
        print("No Constants Found")
    

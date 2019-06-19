import sys

def main():

    program = loadProgram(sys.argv[1])
    try:
        runProgram(program)
    except IndexError:
        print("ERR: Program counter out of range")
        
def runProgram(program):

    pc = 0
    registers = [0] * 12

    while True:
        incPc = True
        
        curOp  = program[pc]
        opCode = curOp[0]

        args = []
        for arg in curOp:
            args.append(resolveVal(arg, registers))
        
        if opCode == "shn":
            print(args[1])

        if opCode == "sha":
            print(chr(args[1]))

        if opCode == "mov":
            registers[resolveReg(curOp[1])] = args[2]

        if opCode == "add":
            registers[resolveReg(curOp[1])] = args[2] + args[3]

        if opCode == "sub":
            registers[resolveReg(curOp[1])] = args[2] - args[3]
           
        if opCode == "jlt":
            if(args[2] < args[3]):
                pc = args[1]
                incPc = False

        if opCode == "jgt":
            if(args[2] > args[3]):
                pc = args[1]
                incPc = False

        if opCode == "jet":
            if(args[2] == args[3]):
                pc = args[1]
                incPc = False

        if opCode == "jmp":
            pc = args[1]
            incPc = False

        if opCode == "inp":
            print("> ", end="")
            registers[resolveReg(curOp[1])] = int(input())
            
        if opCode == "hlt":
            break    

        if(incPc):
            pc += 1

        if(pc == len(program)):
            pc = 0
            
def resolveVal(val, reg):
    if(val[0] == "#"):
        return int(val[1:])
    if(val[0] == "r"):
        return reg[resolveReg(val)]
    #print("not resolved: " + val)
        
def resolveReg(reg):
    return int(reg[1:])
    
def loadProgram(filename):
    program = []
    with open(filename) as f:
        for line in f:
            noComLine = line.split(';')[0].strip()
            if noComLine:
                program.append(noComLine.split(" "))
    return program
    
if __name__ == "__main__":
    main()
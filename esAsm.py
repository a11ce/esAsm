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
        
        if opCode == "shn":
            print(resolveVal(curOp[1], registers))

        if opCode == "sha":
            print(chr(resolveVal(curOp[1], registers)))

        if opCode == "mov":
            registers[resolveReg(curOp[1])] = resolveVal(curOp[2], registers)

        if opCode == "add":
            registers[resolveReg(curOp[1])] = resolveVal(curOp[2], registers) + resolveVal(curOp[3], registers)

        if opCode == "gto":
            pc = resolveVal(curOp[1], registers)
            incPc = False

        
        if opCode == "hlt":
            break    

        if(incPc):
            pc += 1
            
def resolveVal(val, reg):
    if(val[0] == "#"):
        return int(val[1:])
    if(val[0] == "r"):
        return reg[resolveReg(val)]
        
def resolveReg(reg):
    return int(reg[1:])
    
def loadProgram(filename):
    program = []
    with open(filename) as f:
        for line in f:
            noComLine = line.split(';')[0]
            program.append(noComLine.rstrip().split(" "))
    return program
    
if __name__ == "__main__":
    main()
import sys


def main():
    program, labels = loadProgram(sys.argv[1])

    labelCount = 0

    print("#include <iostream>")
    print("using namespace std;")
    print("int main()")
    print("{")
    for i in range(12):
        print("\t" + "int r" + str(i) + " = 0;")

    print()

    for curOp in program:

        for i in range(len(curOp)):
            if (curOp[i][0] == "#"):
                curOp[i] = curOp[i][1:]
            if (curOp[i] in labels):
                curOp[i] = str(labels[curOp[i]])

        opCode = curOp[0]

        print("\tl" + str(labelCount) + ":")
        #print(curOp)

        if opCode == "shn":
            print("\t" + "cout << " + curOp[1] + " << endl;")

        if opCode == "sha":
            print("\t" + "cout << (char)" + curOp[1] + " << endl;")

        if opCode == "mov":
            print("\t" + curOp[1] + " = " + curOp[2] + ";")

        if opCode == "add":
            print("\t" + curOp[1] + " = " + curOp[2] + " + " + curOp[3] + ";")

        if opCode == "sub":
            print("\t" + curOp[1] + " = " + curOp[2] + " - " + curOp[3] + ";")

        if opCode == "jlt":
            print("\t" + "if (" + curOp[2] + " < " + curOp[3] + "){goto l" +
                  curOp[1] + ";}")

        if opCode == "jgt":
            print("\t" + "if (" + curOp[2] + " > " + curOp[3] + "){goto l" +
                  curOp[1] + ";}")

        if opCode == "jet":
            print("\t" + "if (" + curOp[2] + " == " + curOp[3] + "){goto l" +
                  curOp[1] + ";}")

        if opCode == "jmp":
            print("\t" + "goto l" + curOp[1] + ";")

        if opCode == "inp":
            print("\t" + "cin >> " + curOp[1] + ";")

        if opCode == "hlt":
            print("\t" + "exit(0);")

        # https://twitter.com/ppcinstructions/status/559753895757742083
        # Behavior of esAsm is undefined if the computer is on fire, so it
        # can be ignored.
        if opCode == "mficofsr":
            print("\t" + curOp[1] + " = 0;")

        labelCount += 1
        print()

    print("\t" + "goto l0;")
    print("}")


def loadProgram(filename):
    program = []
    labels = {}
    with open(filename) as f:
        for line in f:
            noComLine = line.split(';')[0].strip()
            if noComLine:
                #print(noComLine[-1])
                if noComLine[-1] == ":":
                    labels[noComLine[:-1]] = len(program)
                else:
                    program.append(noComLine.split(" "))
    #print(program)
    return program, labels


if __name__ == "__main__":
    main()

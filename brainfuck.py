#!/usr/bin/env python
import array, itertools, sys

class Interpreter:
    def __init__(self, program, data_size=30000, in_buffer=sys.stdin.buffer,
            out_buffer=sys.stdout.buffer):
        self.in_buffer = in_buffer
        self.out_buffer = out_buffer

        self.data = array.array('B', itertools.repeat(0, data_size))
        self.data_ptr = 0
        self.data_size = data_size

        self.prog = program
        self.prog_ptr = 0
        self.prog_size = len(self.prog)

    def run(self):
        while self.prog_ptr < self.prog_size:
            self.run_single_instruction()

    def run_single_instruction(self):
        if self.prog[self.prog_ptr] == ">":
            self.data_ptr += 1

            if self.data_ptr == self.data_size:
                self.data_ptr = 0
        elif self.prog[self.prog_ptr] == "<":
            self.data_ptr -= 1

            if self.data_ptr == -1:
                self.data_ptr = self.data_size - 1
        elif self.prog[self.prog_ptr] == "+":
            if self.data[self.data_ptr] == 255:
                self.data[self.data_ptr] = 0
            else:
                self.data[self.data_ptr] += 1
        elif self.prog[self.prog_ptr] == "-":
            if self.data[self.data_ptr] == 0:
                self.data[self.data_ptr] = 255
            else:
                self.data[self.data_ptr] -= 1
        elif self.prog[self.prog_ptr] == ".":
            self.out_buffer.write(bytes(self.data[self.data_ptr:self.data_ptr+1]))
        elif self.prog[self.prog_ptr] == ",":
            self.data[self.data_ptr] = ord(self.in_buffer.read(1) or b"\0")
        elif self.prog[self.prog_ptr] == "[":
            if self.data[self.data_ptr] == 0:
                depth = 1
                while (self.prog_ptr < self.prog_size) and (depth > 0):
                    self.prog_ptr += 1
                    if self.prog[self.prog_ptr] == "[":
                        depth += 1
                    elif self.prog[self.prog_ptr] == "]":
                        depth -= 1
        elif self.prog[self.prog_ptr] == "]":
            if self.data[self.data_ptr] != 0:
                depth = 1
                while (self.prog_ptr > 0) and (depth > 0):
                    self.prog_ptr -= 1
                    if self.prog[self.prog_ptr] == "]":
                        depth += 1
                    elif self.prog[self.prog_ptr] == "[":
                        depth -= 1
        else:
            pass  # not an instruction, ignore

        self.prog_ptr += 1

    def transpile(self):
        self.out_buffer.write(b"#include <stdio.h>\n")
        self.out_buffer.write(b"#include <string.h>\n")
        self.out_buffer.write(b"int main(int argc, char *argv[]) {\n")
        self.out_buffer.write("char data[{}];\n".format(str(self.data_size)).encode("utf-8"))
        self.out_buffer.write("char *ptr=memset(data,0,{});\n".format(str(self.data_size)).encode("utf-8"))
        self.out_buffer.write("int in=0;\n".format(str(self.data_size)).encode("utf-8"))
        for c in self.prog:
            self.out_buffer.write(self.transpile_single_instruction(c))
        self.out_buffer.write(b"\n")
        self.out_buffer.write(b"return 0;\n")
        self.out_buffer.write(b"}\n")

    def transpile_single_instruction(self, instruction):
        if instruction == ">":
            return b"++ptr;"
        elif instruction == "<":
            return b"--ptr;"
        elif instruction == "+":
            return b"++*ptr;"
        elif instruction == "-":
            return b"--*ptr;"
        elif instruction == ".":
            return b"putchar(*ptr);"
        elif instruction == ",":
            return b"in=getchar();*ptr=((in==EOF)?0:in);"
        elif instruction == "[":
            return b"while(*ptr){"
        elif instruction == "]":
            return b"}"
        else:
            return b""

if __name__ == "__main__":
    bf = Interpreter(open(sys.argv[2]).read())
    if sys.argv[1].startswith("r"):
        bf.run()
    elif sys.argv[1].startswith("t"):
        bf.transpile()

nga-scala is an implementation of the nga virtual machine written in scala 3.
The purpose of this is to allow raw images to be run on top of the JVM. 

NGA is a very simple machine and is considered to be a proper MISC (Minimal Instruction Set Computer).

The Forth implementation has 30 instructions, including support for I/O, with values of 32 bit width. 

The machine is stack based and uses two stacks, one called the data stack, and one called the address stack.
The address stack is used to keep state of a call trace, allowing one to return from subroutines as one would expect from other computers, while the data stack allows one to push and pop from the stack data to be operated on. This allows the computer to only keep three registers: the instruction pointer, the data pointer, and the address pointer. 

The implementation also provides support for instruction packing allowing for superior memory utilization. 

Example: 

```scala
import nga.*

// LIT, LIT, ADD, END
val img = Array ( 0x1, 1, 0x1, 2, 0x11, 0x1A )
val inst = NGAInstance( img )
inst.run

// Output in Data Stack -> 3

val asm_img = s"""call foo
call main
and
end
:main
lit 1
ret
:foo
lit 2
ret""".assembleAndRun

// Output in Data Stack -> 0 

'''

VM implemented here in C:
https://github.com/crcx/nga.git

Forth documentation here: 
http://forth.works/chapters/internals/nga.html


Development Goals:

- [x] Implement working VM and interface
- [x] Implement tests
- [ ] Write documentation for VM code
- [ ] Load raw images from multiple sources
- [x] Implement Assembler and inline assmebling
- [ ] Implement REPL 

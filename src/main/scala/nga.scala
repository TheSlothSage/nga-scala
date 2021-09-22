package nga

import scala.collection.immutable.LinearSeq
/*  Description of the size of opcodes and cells using Scala Standard
 *
 * Byte: 8 bit
 * Short : 16 bit -- (C: WORD)
 * Int : 32 bit   -- (C: DWORD) 
 * long : 64 bit  -- (C: QWORD)
 *
*/

type Code = Byte
type Data = Byte
type Word = Int 

class Stack:
  
  type SizeT = Data 

  private var elements: List[SizeT] = Nil

  def push (x: SizeT): Unit = elements = x :: elements 
  
  def tos : SizeT = 
    
    if size > 0 then 
      (elements.head)  

    else
      0 
  
  def nos : SizeT = 
    
    if size > 1 then
      (elements.tail).head // TODO throw errror if size is 1

    else
      0
  
  def last : SizeT = elements.last
  
  def pop : SizeT =
    val top = tos
    elements = elements.tail
    top

  def size : Int = elements.size

class Memory (src: Array[Word]):
  
  type SizeT = Word
  
  var ip = 0
  val arr = src.flatMap(_.unpack)

  def getOffset(i: Int) : Op = arr(ip + i)
  def get : Op = 

    if ip < arr.size then
       arr(ip)
  
    else 
      VM_END()
  
// -------------------------------------------------------------------------

trait Op (o: Code):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null
  val code = o
  null 

// Does nothing, used for timing or alignment.

class VM_NOP extends Op(0):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    mem.ip = mem.ip + 1
    mem.get.exec(addr, data, mem)
/*
 *  LIT is followed by a value to push to the stack. 
 *  Increments SP and IP and pushes the value at the incremented IP stack
 *  C++ impl -> 
 *
 *   void lit (word* memory, int& sp, int& ip) {
 *      sp++;
 *      TOS = memory [ip++]      
 *   }
*/

class VM_LIT extends Op(1):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    data.push (mem.getOffset(1).toCode)
    mem.ip = mem.ip + 2
    mem.get.exec(addr, data, mem)
    null
/*
 *  Duplicates top of the stack 
 *  C++ impl ->
 *
 *  void dup (word* data, int& sp) {
 *    sp++; 
 *    data [sp] = data [sp-1]; 
 *  }
*/

class VM_DUP extends Op(2):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    data.push(data.nos)
    mem.ip = mem.ip + 1
    mem.get.exec(addr, data, mem)

/*
 *  Removes value from the top of the stack 
 *  C++ impl ->
 *  
 *  void drop (word* data, int& sp) {
 *      data[sp] = 0;
 *      if (--sp < 0) {
 *          ip = MEM_DEPTH;    
 *      }
 *  }
*/ 

class VM_DRP extends Op(3): 
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null
/*
 *  Switch the top and second to the top entries in the stack.
 *  C++ impl -> 
 *
 *  void swap (word* data, int& sp) {
 *      word a;
 *      a = data [sp];
 *      data [sp] = data [sp-1];
 *      data [sp-1] = a; 
 *  }
*/
class VM_SWP extends Op(4):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

/*
 *  Moves the value at the top of the data stack to the address stack
 *  C++ impl ->
 *
 *  void push (word* data, word* addr, int& sp, int& rp) {
 *      rp++;
 *      addr [rp] = data [sp];
 *      drop (data, sp); 
 *  }
*/ 

class VM_PSH extends Op(5): 
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

/*
 *  Moves top item on the address stack to the data stack
 *  C++ impl ->
 *
 *  void pop (word* data, word* addr, int& sp, int& rp) {
 *      sp++;
 *      data [sp] = addr [--rp];    
 *  }
*/

class VM_POP extends Op(6): 
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

/*
 *  Calls a subroutine with address at the top of the stack
 *  C++ impl -> 
 *
 *  void call (word* data, word* addr, int& sp, int& rp, int& ip) {
 *      addr [rp++] = ip; 
 *      jump (data, ip, sp);
 *  }
*/

class VM_CAL extends Op(7): 
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null
/*
 *  Conditional call taking a flag and a pointer to an address
 *  C++ impl ->
 *
 *  void ccall (word* data, word* addr, int& sp, int& rp, int& ip) {
 *      int a, b;
 *
 *      a = data [sp]; // addr
 *      drop (data, sp); 
 *      
 *      b = data [sp]; // flag
 *      drop (data, sp); 
 *
 *      if (!b) {
 *          addr [rp++];
 *          ip = a - 1; 
 *      }
 *  }
*/ 

class VM_CCAL extends Op(8):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null 

/*
 *  Returns flow to instruction after last call
 *  C++ impl ->
 *
 *  void return (word* addr, int& rp, int& ip) {
 *      ip = addr [--rp]; 
 *  }
*/ 

class VM_RET extends Op(9):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_EQ extends Op(10): 
  def exec (addr: Stack, data: Stack, mem: Memory) : Null  = null

class VM_NEQ extends Op(11):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_LT extends Op(12):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_GT extends Op(13): 
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_FCH extends Op(14):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null
  
class VM_STR extends Op(15):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null
  
class VM_ADD extends Op(16):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_SUB extends Op(17):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_MUL extends Op(18):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_DIV extends Op(19):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_AND extends Op(20):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_OR extends Op(21):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_XOR extends Op(22):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_SFT extends Op(23):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_ZRET extends Op(24):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_END extends Op(25):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null 
    

class VM_CPU extends Op(26):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null 

class VM_ERR extends Op(127):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null

class VM_NOTHING extends Op(126):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = null 

def getOperation (c : Code) : Op = c match
  case 0x0  => VM_NOP() 
  case 0x1  => VM_LIT()
  case 0x2  => VM_DUP()
  case 0x3  => VM_DRP()
  case 0x4  => VM_SWP()
  case 0x5  => VM_PSH()
  case 0x6  => VM_POP()
  case 0x7  => VM_CAL()
  case 0x8  => VM_CCAL()
  case 0x9  => VM_RET()
  case 0xA  => VM_EQ()
  case 0xB  => VM_NEQ()
  case 0xC  => VM_LT()
  case 0xD  => VM_GT()
  case 0xE  => VM_FCH()
  case 0xF  => VM_STR()
  case 0x10 => VM_ADD()
  case 0x11 => VM_SUB()
  case 0x12 => VM_MUL()
  case 0x13 => VM_DIV()
  case 0x14 => VM_AND()
  case 0x15 => VM_OR()
  case 0x16 => VM_XOR()
  case 0x17 => VM_SFT()
  case 0x18 => VM_ZRET()
  case 0x19 => VM_END()
  case 0x1A => VM_CPU()
  case _    => VM_ERR()  

extension (xs: Op)
  def toCode : Code = 
    xs.code

/* Extension to Word allowing conversion to unpacked Arrays of opcodes 
 * ex. 769.unpack -> Array(VM_NOP, VM_NOP, VM_DRP, VM_LIT) */ 

extension[T] (xs: Word)
  def unpack : Array[Op] =

    val a   = xs & 0xff
    val b   = (xs >> 8)  & 0xff 
    val c   = (xs >> 16) & 0xff
    val d   = (xs >> 24) & 0xff
    
    // Reverse the order for littlEndian
    val arr = Array(d, c, b, a).map(_.toByte)
    
    arr.map(getOperation)

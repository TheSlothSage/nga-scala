package nga

import scala.annotation.tailrec

/* Singleton storing options for VM runtime */ 

object VM_Options: 

  type Word = Int
  type Code = Byte
  type Data = Word

  
  /* Enable instruction packing. */ 

  var optionPackingEnable = true

  /* Not yet implemented. Allows for on the fly assmebling */ 

  // var optionAssemblerMode = false 

  val optionVersionString = "nga-scala v0.1a"
  

  def initVMOptions =
    if optionPackingEnable then
      type Code = Byte
    
    else 
      type Code = Int


/* ------------------------------------------------------------------ * 
 *  Description of the size of opcodes and cells using Scala Standard *
 *                                                                    *
 * Byte: 8 bit                                                        *
 * Short : 16 bit -- (C: WORD)                                        *
 * Int : 32 bit   -- (C: DWORD)                                       *
 * long : 64 bit  -- (C: QWORD)                                       *
 * ------------------------------------------------------------------ */

type Word = VM_Options.Word
type Code = VM_Options.Code
type Data = VM_Options.Data



/* ----------------------------------------------------- *
 * Address and Data stacks  will use this implementation * 
 * ----------------------------------------------------- */


class Stack:
  type SizeT = Data

  private var elements: List[SizeT] = Nil

  def push (x: SizeT): Unit = elements = x :: elements 
  
  /* If the stack is too small for these, just return 0. */ 
  
  def tos : SizeT =
    elements.head    
  
  def nos : SizeT = 
    (elements.tail).head
  
  def last : SizeT = elements.last
  
  def pop : SizeT =
    val top = tos
    elements = elements.tail
    top

  def size : Int = elements.size


/* -----------------------------------------------------------------------------*
 *  Represents executable memory as well as data being pushed by LIT.           *
 *  Also stores the instruction pointer and methods to retrieve next operations *
 *  Takes a Word array and translates it to subclasses of Op()                  *
 * -----------------------------------------------------------------------------*/ 


class Memory (src: Array[Word]):
  
  type SizeT = Word
  
  var ip = 0
  private var opIp = 0

  var arr = src
  private var opArr = src(0).unpack

  def setIp (newip: SizeT) = 
    ip = newip

  def inc : SizeT = 
    ip = ip + 1
    ip

  def dec : SizeT = 
    ip = ip - 1
    ip

  def getOffset(i: Int) : SizeT =
    arr(ip + i)

  def getFrom (i: Int) : SizeT = 
    arr(i)

  def get : SizeT = 
    arr(ip)
  
  def next : Word = 
    arr(inc) 

  def nextOp : Op =   
    
    if VM_Options.optionPackingEnable == false then
      opArr = arr(inc).unpack
      
    else
      
      if opIp == 4 then
        opIp = 0
        opArr = arr(inc).unpack
        
      else
        opIp = opIp + 1
    
    opArr(opIp)

  def execHere (addr: Stack, data: Stack, mem: Memory) : Null =
    opArr(ip).tailExec(addr, data, mem)
  
  val execThisHere = execHere(_: Stack, _: Stack, this)



extension (xs: Word)
  def unpack : Array[Op] = 
    val a   = xs & 0xff
    val b   = (xs >> 8)  & 0xff 
    val c   = (xs >> 16) & 0xff
    val d   = (xs >> 24) & 0xff

    // Reverse the order for little Endian
    val arr = Array(d, c, b, a).map(_.toByte)
    arr.map(getOperation) 

class NGAInstance(image: Array[Word]): 
  
  val addr, data = Stack()
  val mem = Memory(image)

  def start : Null = 
    mem.execThisHere(addr, data)
  
// ----------------------------------------------------------------------

trait Op (o: Code): 
  def exec (addr: Stack, data: Stack, mem: Memory) : Null
  val code = o

  /* Allows for efficent tail recursion */
  @tailrec 
  final def tailExec (addr: Stack, data: Stack,  mem: Memory) : Null = 
    this.code match 
      case 26   => null
      case 127  => null
      case _    => this.exec(addr, data, mem)
   
    mem.nextOp.tailExec (addr, data, mem) 

  def end : Null = null

def getOperation (c : Code) : Op = c match
  case 0x0  => VM_NOP() 
  case 0x1  => VM_LIT()
  case 0x2  => VM_DUP()
  case 0x3  => VM_DRP()
  case 0x4  => VM_SWP()
  case 0x5  => VM_PSH()
  case 0x6  => VM_POP()
  case 0x7  => VM_JMP()
  case 0x8  => VM_CAL()
  case 0x9  => VM_CCAL()
  case 0xA  => VM_RET()
  case 0xB  => VM_EQ()
  case 0xC  => VM_NEQ()
  case 0xD  => VM_LT()
  case 0xE  => VM_GT()
  case 0xF  => VM_FCH()
  case 0x10 => VM_STR()
  case 0x11 => VM_ADD()
  case 0x12 => VM_SUB()
  case 0x13 => VM_MUL()
  case 0x14 => VM_DIV()
  case 0x15 => VM_AND()
  case 0x16 => VM_OR()
  case 0x17 => VM_XOR()
  case 0x18 => VM_SFT()
  case 0x19 => VM_ZRET()
  case 0x1A => VM_END()
  case _    => VM_ERR()  

// Does nothing, used for timing or alignment.
sealed class VM_NOP extends Op(0):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null =
    end 


// LIT \\
/* ---------------------------------------------------------------------- *
 *  LIT is followed by a value to push to the stack.                      * 
 *  Pushes the value at the incremented IP stack *
 * ---------------------------------------------------------------------- */


sealed class VM_LIT extends Op(1):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
     
    data.push( mem.next )
    end 



// DUP \\
/* ----------------------------------*
 *  Duplicates top of the stack      *
 *  C++ impl ->                      *
 * ----------------------------------*/


sealed class VM_DUP extends Op(2):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
  
    data.push(data.nos)
    end 



// DRP \\
/* ---------------------------------------- *
 *  Removes value from the top of the stack *
 * ---------------------------------------- */ 


sealed class VM_DRP extends Op(3): 
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
      
      val drop = data.pop
      end



// SWP \\
/* ----------------------------------------------------------- * 
 *  Switch the top and second to the top entries in the stack. *
 * ----------------------------------------------------------- */


sealed class VM_SWP extends Op(4):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    
    val a = data.pop
    val b = data.pop

    data.push(a)
    data.push(b)
    end



// PSH \\
/* ------------------------------------------------------------------ *
 *  Moves the value at the top of the data stack to the address stack *
 * ------------------------------------------------------------------ */ 


sealed class VM_PSH extends Op(5): 
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    addr.push (data.tos)
    data.pop
    end


// POP \\
/* ------------------------------------------------------ *
 *  Moves top item on the address stack to the data stack *
 * ------------------------------------------------------ */   


sealed class VM_POP extends Op(6): 
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    
    data.push( addr.pop )
    end


sealed class VM_JMP extends Op(7):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null =
    
    mem.setIp(data.pop - 1) 
    end


// CAL \\
/* ---------------------------------------------------------------- *
 *  Calls a subroutine with address at the top of the stack         *
 * ---------------------------------------------------------------- */ 


sealed class VM_CAL extends Op(8): 
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    addr.push(mem.ip)
    VM_JMP().exec(addr, data, mem) 
    end


// CCAL \\
/* ----------------------------------------------------------------- *
 *  Conditional call taking a flag and a pointer to an address       *
 * ----------------------------------------------------------------- */ 


sealed class VM_CCAL extends Op(9):
   final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val a = data.pop // addr 
    val b = data.pop // flag

    if b != 0 then
      addr.push (mem.ip)
      data.push (a)
      VM_JMP().exec(addr, data, mem)

    end


// RET \\
/* --------------------------------------------- *
 *  Returns flow to instruction after last call  *
 * --------------------------------------------- */                

sealed class VM_RET extends Op(10):
   final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    
    mem.setIp(addr.pop)
    end

sealed class VM_EQ extends Op(11): 
   final def exec (addr: Stack, data: Stack, mem: Memory) : Null  = 

    val top = data.pop
    val nxt = data.pop

    if nxt == top then
      data.push(-1) 

    else 
      data.push(0)

    data.push(top)
    end

sealed class VM_NEQ extends Op(12):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop 

    if nxt != top then
      data.push(-1)
    
    else 
      data.push(0)

    data.push(top) 
    end

sealed class VM_LT extends Op(13):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    
    val top = data.pop
    val nxt = data.pop 

    if nxt < top then
      data.push(-1)
   
    else
      data.push(0)
  
    data.push(top)
    end

sealed class VM_GT extends Op(14): 
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    if nxt > top then
      data.push(-1)
    
    else
      data.push(0)


    data.push(top)
    end

sealed class VM_FCH extends Op(15):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    
    var top = data.pop 

    top match
      case -1   =>  data.push(data.size)
      case -2   =>  data.push(addr.size)
      case -3   =>  data.push(mem.arr.size)

      case _    =>  data.push( mem.getFrom( top ).toByte )

    end
  
sealed class VM_STR extends Op(16):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

   // do the thing

    end
  
sealed class VM_ADD extends Op(17):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop 
    val nxt = data.pop

    data.push(nxt + top)
    end

sealed class VM_SUB extends Op(18):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    data.push (nxt - top)
    end

sealed class VM_MUL extends Op(19):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    data.push (nxt * top)
    end

sealed class VM_DIV extends Op(20):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.nos / data.tos
    val nxt = data.nos % data.nos
    
    data.pop
    data.pop

    data.push(nxt)
    data.push(top)
    end

sealed class VM_AND extends Op(21):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    data.push( top & nxt )
    end

sealed class VM_OR extends Op(22):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    data.push( top | nxt )
    end

sealed class VM_XOR extends Op(23):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    data.push( top ^ nxt )
    end

sealed class VM_SFT extends Op(24):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    var top = data.pop
    var nxt = data.pop

    if top < 0 then 
      nxt = nxt << (top * -1)
    
    else if nxt < top then
      nxt = nxt >> top
    
    else
      nxt = nxt >> top

    data.push(nxt)
    end
      

sealed class VM_ZRET extends Op(25):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    if (data.pop) == 0 then
      mem.setIp(addr.pop)

    end

sealed class VM_END extends Op(26):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = end 

sealed class VM_ERR extends Op(127):
  def exec (addr: Stack, data: Stack, mem: Memory) : Null = end


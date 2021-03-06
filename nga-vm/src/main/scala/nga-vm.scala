package nga
 
  /* Singleton storing options for VM runtime */ 
  object VM_Options: 

    type Word = Int
    type Code = Byte
    type Data = Word

  
    /* Enable instruction packing. */ 

    var optionPackingEnable = false
    var optionDebug = true
    var optionMaxAddrDepth = 256 // Not used
    var optionMaxDataDepth = 512 // Not used

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
 * Byte: 8 bit -- (C: Char)                                           *
 * Short : 16 bit -- (C: WORD)                                        *
 * Char : 16 bit -- (C: unsigned WORD)                                *
 * Int : 32 bit   -- (C: DWORD)                                       *
 * long : 64 bit  -- (C: QWORD)                                       *
 * ------------------------------------------------------------------ */

type Word = VM_Options.Word
type Code = VM_Options.Code
type Data = VM_Options.Data


import scala.annotation.tailrec


/* Define toCode based on packing option */ 

extension (xs: Int) 
  def toCode : Code = 
    xs.toByte

/* ----------------------------------------------------- *
 * Address and Data stacks  will use this implementation * 
 * ----------------------------------------------------- */


private class Stack:
  type SizeT = Data

  private var elements: List[SizeT] = Nil

  def push (x: SizeT): Unit = 
    elements = x :: elements 
  
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
 *  Represents executable memory.                                               *
 *  Also stores the instruction pointer and methods to retrieve next operations *
 *  Represents stored images as Word arrays                                     *
 * -----------------------------------------------------------------------------*/ 

private class Memory (src: Array[Word]):
  
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

  def getFrom (i: Int) : SizeT = 
    arr(i)

  def get : SizeT = 
    arr(ip)
  
  def getOp : Op =
    if VM_Options.optionPackingEnable == true then
      opArr(opIp)
    else
      opArr(3)

  def next : Word = 
    arr(inc) 

  def nextOp : Op =  
    if VM_Options.optionPackingEnable == true then
      
      if opIp >= 3 then
        opIp = 0
        opArr = next.unpack 
      else
        opIp = opIp + 1
      
      getOp
    else
      getOperation(arr(inc).toCode) 

extension (xs: Word)
  def unpack : Array[Op] = 
    val a   = xs & 0xff
    val b   = (xs >> 8)  & 0xff 
    val c   = (xs >> 16) & 0xff
    val d   = (xs >> 24) & 0xff

    // Reverse the order for little Endian
    val arr = Array(d, c, b, a).map(_.toByte)
    arr.map(getOperation) 
  
// ----------------------------------------------------------------------

private trait Op (o: Code): 
  def exec (addr: Stack, data: Stack, mem: Memory) : Null 
  val code = o

  /* Allows for efficent tail recursion */
  @tailrec 
  final def tailExec (addr: Stack, data: Stack,  mem: Memory) : Null = 

    this.exec(addr, data, mem)
    
    if mem.ip >= (mem.arr.size) then
      null
    
    else
      mem.nextOp.tailExec (addr, data, mem)  


  def end : Null = null

private def getOperation (c : Code) : Op = c match
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
private sealed class VM_NOP extends Op(0):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null =
    end 


// LIT \\
/* ---------------------------------------------------------------------- *
 *  LIT is followed by a Cell with datato push to the stack.              * 
 *  Pushes the value at the incremented IP                                *
 * ---------------------------------------------------------------------- */

private sealed class VM_LIT extends Op(1):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    
    data.push( mem.next )
    end 


// DUP \\
/* ----------------------------------*
 *  Duplicates top of the stack      *
 * ----------------------------------*/

private sealed class VM_DUP extends Op(2):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
  
    data.push(data.tos)
    end 



// DRP \\
/* ---------------------------------------- *
 *  Removes value from the top of the stack *
 * ---------------------------------------- */ 

private sealed class VM_DRP extends Op(3): 
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
      
      val drop = data.pop
      end



// SWP \\
/* ----------------------------------------------------------- * 
 *  Switch the top and second to the top entries in the stack. *
 * ----------------------------------------------------------- */

private sealed class VM_SWP extends Op(4):
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

private sealed class VM_PSH extends Op(5): 
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    addr.push (data.pop)
    end


// POP \\
/* ------------------------------------------------------ *
 *  Moves top item on the address stack to the data stack *
 * ------------------------------------------------------ */   

private sealed class VM_POP extends Op(6): 
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    
    data.push( addr.pop )
    end


// JMP \\
/* ------------------------------------------- *
 *  Jumps to address stored on the Data stack  *
 * ------------------------------------------- */

private sealed class VM_JMP extends Op(7):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null =
    
    mem.setIp(data.pop - 1) 
    end


// CAL \\
/* ---------------------------------------------------------------- *
 *  Calls a subroutine with address at the top of the stack         *
 * ---------------------------------------------------------------- */ 

private sealed class VM_CAL extends Op(8): 
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    addr.push(mem.ip)
    VM_JMP().exec(addr, data, mem) 
    end


// CCAL \\
/* ----------------------------------------------------------------- *
 *  Conditional call taking a flag and a pointer to an address       *
 * ----------------------------------------------------------------- */ 

private sealed class VM_CCAL extends Op(9):
   final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val a = data.pop // addr 
    val b = data.pop // flag

    if b != 0 then
      addr.push (mem.ip)
      data.push (a)
      VM_JMP().exec(addr, data, mem)

    end


// RET \\
/* ----------------------------------------------------------------------------------- *
 *  Returns flow to instruction after last call by popping top value of Address stack  *
 * ----------------------------------------------------------------------------------- */                

private sealed class VM_RET extends Op(10):
   final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    
    mem.setIp(addr.pop)
    end

// EQ, NEQ, LT, GT \\
/* ---------------------------------------- *
 * Each evaluates the specified conditions  *
 * if false pushes 0, else any other value  *
 *                                          *
 * EQ:  if eq                               *
 * NEQ: if ~eq                              *
 * LT:  if less than                        *
 * GT:  if greater than                     *                                           
 * ---------------------------------------- */

private sealed class VM_EQ extends Op(11):                                      
   final def exec (addr: Stack, data: Stack, mem: Memory) : Null  = 

    val top = data.pop
    val nxt = data.pop

    if nxt == top then
      data.push(-1) 

    else 
      data.push(0)

    data.push(top)
    end

private sealed class VM_NEQ extends Op(12):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop 

    if nxt != top then
      data.push(-1)
    
    else 
      data.push(0)

    data.push(top) 
    end

private sealed class VM_LT extends Op(13):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    
    val top = data.pop
    val nxt = data.pop 

    if nxt < top then
      data.push(-1)
   
    else
      data.push(0)
  
    data.push(top)
    end

private sealed class VM_GT extends Op(14): 
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    if nxt > top then
      data.push(-1)
    
    else
      data.push(0)
    
    data.push(top)
    end


// FCH \\
/* -------------------------------------------- *
 * Fetches data from an address in the image.   *
 * With signed numbers, it can be used to query * 
 * information about VM state.                  *
 * -------------------------------------------- */ 

private sealed class VM_FCH extends Op(15):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    
    var top = data.pop 

    top match
      case -1   =>  data.push(data.size)
      case -2   =>  data.push(addr.size)
      case -3   =>  data.push(mem.arr.size)

      case _    =>  data.push( mem.getFrom( top ).toCode )

    end

private sealed class VM_STR extends Op(16):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop
    
    mem.arr(top) = nxt
    end
  
private sealed class VM_ADD extends Op(17):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
  
    val top = data.pop
    val nxt = data.pop

    data.push(nxt + top)
    end

private sealed class VM_SUB extends Op(18):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    data.push (nxt - top)
    end

private sealed class VM_MUL extends Op(19):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    data.push (nxt * top)
    end

private sealed class VM_DIV extends Op(20):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.nos / data.tos
    val nxt = data.nos % data.nos
    
    data.pop
    data.pop

    data.push(nxt)
    data.push(top)
    end

private sealed class VM_AND extends Op(21):
 final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    data.push( top & nxt )
    end

private sealed class VM_OR extends Op(22):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    data.push( top | nxt )
    end

private sealed class VM_XOR extends Op(23):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    val top = data.pop
    val nxt = data.pop

    data.push( top ^ nxt )
    end

private sealed class VM_SFT extends Op(24):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

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
      

private sealed class VM_ZRET extends Op(25):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 

    if (data.pop) == 0 then
      mem.setIp(addr.pop)

    end

private sealed class VM_END extends Op(26):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = 
    mem.setIp( mem.arr.size )
    end

private sealed class VM_ERR extends Op(127):
  final def exec (addr: Stack, data: Stack, mem: Memory) : Null = end


trait Instance[B] (bind: Array[B]=>Array[Word], buf: Array[B]): 
  val data, addr = Stack()
  
  def run : Instance[B] = 
    val mem = Memory( bind(buf) ) 
    mem.getOp.tailExec(addr, data, mem) 
    this

case class DefaultInstance(arr: Array[Word]) extends Instance[Word]((i)=>i, arr) 

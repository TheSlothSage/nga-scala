package nga

import scala.annotation.tailrec
import scala.language.postfixOps

type StateT = ( List[String], List[Token] )


/* Tokens which will be scanned for and used to check grammar */ 

abstract class Token

case class Operation (opCode: Code) extends Token
case class Num (rval: Word) extends Token
case class ID (src: String) extends Token

/* Tag indicates a location in code */ 
case class Tag (id: String) extends Token

/* ---------------------------------------------------------------*/ 

def checkFlowControl (src: Code) : Boolean = 

    if src == 0x7 || src == 0x8 || src == 0x9 || src == 0xA || src == 0x19 || src == 0x26 then 
        true

    else 
        false 

def getOpCode (src: String) : Option[Code] = src match

    case "nop"   => Some(0x0)
    case "lit"   => Some(0x1)
    case "dup"   => Some(0x2)
    case "drop"  => Some(0x3)
    case "swp"   => Some(0x4)
    case "push"  => Some(0x5)
    case "pop"   => Some(0x6)
    case "jmp"   => Some(0x7)
    case "call"  => Some(0x8)
    case "ccal"  => Some(0x9)
    case "ret"   => Some(0xA)
    case "eq"    => Some(0xB)
    case "neq"   => Some(0xC)
    case "lt"    => Some(0xD)
    case "gt"    => Some(0xE)
    case "fetch" => Some(0xF)
    case "store" => Some(0x10)
    case "add"   => Some(0x11)
    case "sub"   => Some(0x12)
    case "mul"   => Some(0x13)
    case "div"   => Some(0x14)
    case "and"   => Some(0x15)
    case "or"    => Some(0x16)
    case "xor"   => Some(0x17)
    case "shift" => Some(0x18)
    case "zret"  => Some(0x19)
    case "end"   => Some(0x1A)
    case _       => None

/* -------------------------------------------------------------- */

/* parsers which will match different types of lexemes and return a parseable token */

def matchNum (state: StateT) : StateT = state match
    case (List(), xs) => state
    case (head :: tail, xs) if head.forall( _.isDigit) => ( tail, xs.appended(Num(head.toInt) ) )
    case _ => state 


def matchID (state: StateT) : StateT = state match 
    case (List(), xs) => state
    case (head :: tail, xs) if !(head(0).isDigit) => ( tail, xs.appended(ID(head)) ) 
    case _ => state
 
def matchTag (state: StateT) : StateT = state match 
    case (List(), xs) => state
    case (head :: tail, xs) if head.startsWith(":") => ( tail, xs.appended( Tag(head.substring(1)) ) )  
    case _ => state
 
def matchOperation (state: StateT) : StateT = state match 
    case (List(), xs) => state
    case (head :: tail, xs) => getOpCode( head ) match

        case Some(op) => ( tail, xs.appended( Operation(op) ) ) 
        case None => state

    case null => state


/* Scans line to match tokens */

def evalScanLine (src: StateT) : List[Token] = 

    val state = src . tag . op . id . num 
  
    if state(0).size == 0 then 
        state(1) 

    else
        evalScanLine(state)

def scanLine (src: String) : List[Token] = evalScanLine( ( src.split("\\s+").toList, List[Token]() ) ) 

extension (xs: StateT)

    def num = matchNum(xs)
    def id = matchID(xs) 
    def tag = matchTag(xs)
    def op = matchOperation(xs)

extension (xs: String) 

    def tokenizeThisLine : List[Token] = 
        scanLine(xs)

    def tokenizeMultiLine : Array[List[Token]] = 
        xs.split("\\n+") . map(_.tokenizeThisLine) 
/* -------------------------------------------------------------------- */ 

/* Parseable tokens will be converted to these, which will be used to create the image */ 

abstract class Parsed

case class FlowControlNoVal (opCode: Code) extends Parsed
case class FlowControlWithID (opcode: Code, rval: String) extends Parsed
case class FlowControlWithNum (opcode: Code, rval: Word) extends Parsed
case class PackableNoVal (opCode: Code) extends Parsed
case class PackableWithNum (opcode: Code, rval: Word) extends Parsed
case class PackableWithID (opcode: Code, id: String) extends Parsed 
case class ValidTag(id: String) extends Parsed

case class ParseError (what: String) extends Parsed

/* Parser which evaluate tokens based on a grammar using pattern matching */ 

/* Check grammar of instructions and add ParseErrors for inavlid formats. Grammar:
* < Flow Control > ID
* < Flow Control > Num
* < Other Op > Num 
* < Tag >
*/ 

def parseLine (src: List[Token]) : List[Parsed] = 
    src match

        case Operation(x) :: Num (y) :: Nil if checkFlowControl(x) => 
            FlowControlWithNum(x, y) :: Nil
        
        case Operation(x) :: ID (y) :: Nil if checkFlowControl(x) => 
            FlowControlWithID(x, y) :: Nil
        
        case Operation(x) :: Nil if checkFlowControl(x) => 
            FlowControlNoVal(x) :: Nil
        
        case Operation(x) :: Nil if !checkFlowControl(x)=> 
            PackableNoVal(x) :: Nil
        
        case Operation(x) :: Num(y) :: Nil => 
            PackableWithNum(x, y) :: Nil

        case Tag(x) :: Nil => 
            ValidTag(x) :: Nil 
        
        case Operation(x) :: ID (y) :: Nil if !checkFlowControl(x) => 
            ParseError("expected rval of num, got ID") :: Nil 
        
        case Num(x) :: Nil => 
            ParseError("expected tag or operation, got num") :: Nil
        
        case ID(x) :: _ => 
            ParseError("invalid format: expected op rval, op, or :tag, got ID _") :: Nil

        case Num(x) :: _ =>
            ParseError("invalid format: expected op rval, op, or :tag, got Num _") :: Nil

        case _ :: Tag(x) :: _ => 
            ParseError("invalid format: expected :tag, got lval :tag") :: Nil

        case Tag(x) :: y :: _ => 
            ParseError("invalid format: expected :tag, got :tag lvals") :: Nil 
        
        case x :: y :: z :: _ => 
            ParseError("too many args") :: Nil

        case _ => ParseError("unknown error") :: Nil
 

def catchParseErrors (parsed: List[Parsed], line: Int = 0) : List[ (String, Int) ] = parsed match

    case ParseError(x) :: tail =>  
        val ret = ("["+line.toString+"] Parse Error - " + x, line)

        if parsed.size > 1 then
            ret :: catchParseErrors( tail, line + 1 )

        else 
            List(ret)   

    case _ => 
        
        if parsed.size > 1 then
            catchParseErrors( parsed.tail, line + 1 )

        else 
            Nil 


extension (xs: String)
    
    def parseThisLine : List[Parsed] = 
        parseLine(xs.tokenizeThisLine)  

    def parseThisMultiLine : List[Parsed] = 
        xs.tokenizeMultiLine . flatMap(parseLine) . toList

/* ------------------------------------------------------------------ */

/* Now we pack the values if enabled in VM_Options.optionPackingEnable */ 
    

abstract class ImageFormat

case class ImagePack (dat: Word) extends ImageFormat
case class ImageData (dat: Word) extends ImageFormat
case class ImageDataResolve (id: String) extends ImageFormat
case class ImageResolveSource (id: String) extends ImageFormat

/* State to return from the pack function */ 
case class PackState (rest: List[Parsed], data: PackTuple, packed: PackTuple, ret: Boolean) 


type PackTuple = (Option[ImageFormat], Option[ImageFormat], Option[ImageFormat], Option[ImageFormat]) 

/* Immutable and pure. It may seem like we are going overkill on pattern matching, but this is the cleanest way
 * to do this without confusion, and also keeps it in the vein of funcional programming. Might want to edit this
 * if there turns out to be a better way. 
*/

def newPackState (parsed: List[Parsed]) : PackState = 

    PackState(parsed, (None, None, None, None), (None, None, None, None), false) 


def packNextSlot (src: ImageFormat, tup: PackTuple) : PackTuple = tup match
    
    case (None, None, None, None) => (Some(src), None, None, None) 
    case (Some(a), None, None, None) => (Some(a), Some(src), None, None)
    case (Some(a), Some(b), None, None) => (Some(a), Some(b), Some(src), None)
    case (Some(a), Some(b), Some(c), None) => (Some(a), Some(b), Some(c), Some(src))
    
    /* we will do checks to ensure this doesn't happen and we don't have to messily handle errors */ 
    case _ => tup 

def checkIfFull (src: PackTuple) : Boolean = src match
    
    case ( Some(a), Some(b), Some(c), Some(d) ) => true
    case _ => false 

def checkIfEmpty (src: PackTuple) : Boolean = src match

    case ( None, None, None, None ) => true
    case _ => false


def setStateRet (src: PackState) : PackState = 
    PackState( src.rest, src.data, src.packed, true )

extension (xs: PackState)
    
    def isFull : Boolean = checkIfFull(xs.packed) || checkIfFull(xs.data) 
    def isEmpty : Boolean = checkIfEmpty(xs.packed) || checkIfEmpty(xs.data) 
    
    def setRet : PackState = setStateRet(xs) 

    def packSlot (op: ImageFormat, dat: Option[ImageFormat] = None) : PackState = 

        if xs.isFull then 
            xs.setRet

        else dat match

            case Some(dataArg) => 
                PackState( xs.rest.tail, packNextSlot(dataArg, xs.data), packNextSlot(op, xs.packed), xs.ret )

            case None =>
                PackState( xs.rest.tail, xs.data, packNextSlot(op, xs.packed), xs.ret) 
         
   

/* 
 * This step can't be fused with simply turning each parsed into Images since packing changes the locations of data
 * This will allow us to resolve tags, and references to them, and not break flow control instructions 
 * 
 * This code is not very performant, but it is done in-between assembling the image, and so it's more performant
 * than packing afterwards. 
*/

def convertImageFormatOnce (src: PackState) : PackState = src match 

    /* This is adhoc. Either pack or don't, but keep PackState */ 
    
    case PackState( ValidTag(x) :: tail, dat, packed, done ) if src.isEmpty =>
        src.packSlot( ImageResolveSource(x) ).setRet
    

    case PackState( FlowControlNoVal(x) :: tail, dat, packed, done ) if src.isEmpty =>
        src.packSlot( ImagePack(x) ).setRet

    case PackState( FlowControlWithNum(x, y) :: tail, dat, packed, done ) if src.isEmpty => 
        
        convertImageFormatOnce ( 
            PackState( PackableWithNum(1, y) :: FlowControlNoVal(x) :: tail, dat, packed, done ) 
        )
    
    case PackState( FlowControlWithID(x, y) :: tail, dat, packed, done ) if src.isEmpty =>
        
        convertImageFormatOnce (
            PackState( PackableWithID(1, y) :: FlowControlNoVal(x) :: tail, dat, packed, done ) 
        )

    case PackState( PackableNoVal(x) :: tail, dat, packed, done ) => 
    
        if VM_Options.optionPackingEnable then src.packSlot( ImagePack(x) )
        else src.packSlot( ImagePack(x) ).setRet

    case PackState( PackableWithID(x, y) :: tail, dat, packed, done ) =>

        if x == 1 then
            if VM_Options.optionPackingEnable then src.packSlot( ImagePack(x), Some(ImageDataResolve(y)) )
            else src.packSlot( ImagePack(x), Some( ImageDataResolve(y) ) ).setRet
        else
            convertImageFormatOnce ( 
                PackState( PackableWithID(1, y) :: PackableNoVal(x) :: tail, dat, packed, done ) 
            ) 
    
    case PackState( PackableWithNum(x, y) :: tail, dat, packed, done ) => 
        
        /* lit */ 
        if x == 1 then
            if VM_Options.optionPackingEnable then src.packSlot( ImagePack(x), Some(ImageData(y)) )
            else src.packSlot( ImagePack(x), Some( ImageData(y) ) ).setRet 

        else
            convertImageFormatOnce (
                PackState( PackableWithNum(1, y) :: PackableNoVal(x) :: tail, dat, packed, done ) 
            )

    case _ =>
        src.setRet

def convertImageFormat (src: PackState) : List[PackTuple] = 

    val stepped = convertImageFormatOnce(src) 

    if stepped.rest.size == 0 then
        List( stepped.packed, stepped.data )
    
    else if stepped.isFull || stepped.ret then
        val toAppend = List( stepped.packed, stepped.data )  
        
        val nextState = newPackState( stepped.rest ) 
         
        toAppend ::: convertImageFormat( nextState ) 
    
    else
        convertImageFormat( stepped )

extension (xs: List[Parsed])
    def toImageFormat : List[PackTuple] = 
        convertImageFormat( newPackState(xs) )



/* -------------------------------------------------------------------------------------------- */

/* Last step is to resolve the locations of IDs and then converting List[PackTuple] -> Image */ 

def matchResolve(src: Option[ImageFormat], addr: Int, look: String) : Option[ImageFormat] = src match

    case Some( ImageDataResolve(str) ) if look == str => Some(ImageData(addr))
    case _ => src

def matchNotLabel(src: PackTuple, id: String) : Boolean = src match

    case (Some(ImageResolveSource(str)), None, None, None) if str == id => false
    case _ => true

def replaceID (id: String, addr: Int, src: PackTuple) : PackTuple = 

    val a = matchResolve(src(0), addr, id)
    val b = matchResolve(src(1), addr, id)
    val c = matchResolve(src(2), addr, id)
    val d = matchResolve(src(3), addr, id)

    (a, b, c, d)

extension (xs: PackTuple) 
    def resolveThis(id: String, addr: Int) : PackTuple = replaceID(id, addr, xs)  

def recResolve (src: List[PackTuple], addr: Int = 0) : Option[(String, Int)] = if src.size == 0 then None else 
        
    src match
    
        case (Some(ImageResolveSource(id)), None, None, None) :: tail => Some((id, addr)) 
        case _ => recResolve( src.tail, addr + 1 )


def resolve(src: List[PackTuple]) : List[PackTuple] = recResolve(src) match 

    case Some((id, addr)) => 
        println((id, addr))
        resolve(src
            .filter( i => matchNotLabel(i, id)) 
            .map(_.resolveThis(id, addr))
        )

    case None => src
 

/* Turn PackTuple to Array[Word] */ 

def tupleToImage (src: PackTuple) : Array[Word] = src match
             
    case ( Some(ImagePack(a)), None, None, None ) => 
        Array( a )
    case ( Some(ImagePack(a)), Some(ImagePack(b)), None, None) => 
        Array( (b << 16) + (a << 24) ) 
    case ( Some(ImagePack(a)), Some(ImagePack(b)), Some(ImagePack(c)), None ) => 
        Array( (c << 8) + (b << 16) + (a << 24) ) 
    case ( Some(ImagePack(a)), Some(ImagePack(b)), Some(ImagePack(c)), Some(ImagePack(d)) ) => 
        Array( d + (c << 8) + (b << 16) + (a << 24) )
    
    case ( Some(ImageData(a)), None, None, None ) =>
        Array(a) 
    
    case ( Some(ImageData(a)), Some(ImageData(b)), None, None ) =>
        Array(a,b)

    case ( Some(ImageData(a)), Some(ImageData(b)), Some(ImageData(c)), None ) =>
        Array(a, b, c)

    case ( Some(ImageData(a)), Some(ImageData(b)), Some(ImageData(c)), Some(ImageData(d)) ) =>
        Array(a, b, c, d)

    case _ => 
        Array() 

    

extension (xs: List[PackTuple])
    def resolveLabels : List[PackTuple] = resolve(xs) 
    def toImage : Array[Word] = xs.flatMap(tupleToImage).toArray

extension (xs: String) 
    def toTuple : List[PackTuple] = xs
        .parseThisMultiLine
        .toImageFormat
        .filter(_ != (None, None, None, None))
    def assembleThis : Array[Word] = xs
        .parseThisMultiLine
        .toImageFormat
        .filter(_ != (None, None, None, None))
        .resolveLabels
        .toImage


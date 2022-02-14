package nga 



import scala.annotation.tailrec
import scala.language.postfixOps
import scala.collection.mutable.HashMap

private type StateT = ( List[String], List[Token] )


/* Tokens which will be scanned for and used to check grammar */ 

private abstract class Token

private case class Operation (opCode: Code) extends Token
private case class Num (rval: Word) extends Token
private case class ID (src: String) extends Token

/* Tag indicates a well formed label in code which is a source to be resolved */ 
private case class Tag (id: String) extends Token

/* ---------------------------------------------------------------*/ 

private def checkFlowControl (src: Code) : Boolean = 

    if src == 0x7 || src == 0x8 || src == 0x9 || src == 0xA || src == 0x19 || src == 0x26 then 
        true

    else 
        false 

private def getOpCode (src: String) : Option[Code] = src match

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

private def matchNum (state: StateT) : StateT = state match
    case (Nil, xs) => state
    case (head :: tail, xs) if head.forall( _.isDigit) => ( tail, xs.appended(Num(head.toInt) ) )
    case _ => state 


private def matchID (state: StateT) : StateT = state match 
    case (Nil, xs) => state
    case (head :: tail, xs) if !(head(0).isDigit) => ( tail, xs.appended(ID(head)) ) 
    case _ => state
 
private def matchTag (state: StateT) : StateT = state match 
    case (Nil, xs) => state
    case (head :: tail, xs) if head.startsWith(":") => ( tail, xs.appended( Tag(head.substring(1)) ) )  
    case _ => state
 
private def matchOperation (state: StateT) : StateT = state match 
    case (Nil, xs) => state
    case (head :: tail, xs) => getOpCode( head ) match

        case Some(op) => ( tail, xs.appended( Operation(op) ) ) 
        case None => state

    case null => state


/* Scans line to match tokens */

private def evalScanLine (src: StateT) : List[Token] = 

    val state = src . tag . op . id . num 
  
    if state(0).size == 0 then 
        state(1) 

    else
        evalScanLine(state)

private def scanLine (src: String) : List[Token] = evalScanLine( ( src.split("\\s+").toList, List[Token]() ) ) 

/* -------------------------------------------------------------------- */ 

/* Parseable tokens will be converted to these, which will be used to create the image */ 

private abstract class Parsed

private case class FlowControl (opCode: Word) extends Parsed
private case class Packable (opCode: Word) extends Parsed
private case class Resolve (id: String) extends Parsed 
private case class Data (dat: Word) extends Parsed
private case class Label(id: String) extends Parsed

private case class ErrorMessage (what: String) extends Parsed

/* Parser which evaluate tokens based on a grammar using pattern matching */ 

/* Check grammar of instructions and add ParseErrors for inavlid formats. Grammar:
* < Flow Control > ID
* < Flow Control > Num
* < Other Op > Num 
* < Tag >
*/ 

private def parseLine (src: List[Token]) : List[Parsed] = 
    src match

        case (Operation(0xF) | Operation(0x10)) :: tail if VM_Options.optionPackingEnable => 
            ErrorMessage("store and fetch are not supported with inline packing") :: Nil 

        case Operation(x) :: Num (y) :: Nil if checkFlowControl(x) => 
            Packable(1) :: Data(y) :: FlowControl(x) :: Nil
        
        case Operation(x) :: ID (y) :: Nil if checkFlowControl(x) => 
            Packable(1) :: Resolve(y) :: FlowControl(x) :: Nil
        
        case Operation(x) :: Nil if checkFlowControl(x) => 
            FlowControl(x) :: Nil
        
        case Operation(x) :: Nil if !checkFlowControl(x)=> 
            Packable(x) :: Nil
        
        case Operation(x) :: Num(y) :: Nil => 
            
            if x == 1 then
                Packable(x) :: Data(y) :: Nil
            
            else
                Packable(1) :: Data(y) :: Packable(x) :: Nil 

        case Tag(x) :: Nil => 
            Label(x) :: Nil 
        
        case Operation(x) :: ID (y) :: Nil if !checkFlowControl(x) =>
            Packable(1) :: Resolve(y) :: Packable(15) :: Packable(x) :: Nil 
        
        case Tag(x) :: Num(y) :: Nil => 
            Label(x) :: Data(y) :: Nil

        case ID(x) :: _ => 
            ErrorMessage("invalid format: expected op rval, op, or :tag, got ID _") :: Nil

        case Num(x) :: _ =>
            ErrorMessage("invalid format: expected op rval, op, or :tag, got Num _") :: Nil

        case _ :: Tag(x) :: _ => 
            ErrorMessage("invalid format: expected :tag, got lval :tag") :: Nil

        case Tag(x) :: y :: _ => 
            ErrorMessage("invalid format: expected :tag, got :tag lvals") :: Nil 
        
        case x :: y :: z :: _ => 
            ErrorMessage("too many args") :: Nil

        case _ => ErrorMessage("unknown error") :: Nil
 


private def collectParseErrors (parsed: List[Parsed], line: Int = 0) : List[String] = parsed match

    case ErrorMessage(x) :: tail =>  
        val ret = "["+line.toString+"] Parse Error - " + x

        if parsed.size > 1 then
            ret :: collectParseErrors( tail, line + 1 )

        else 
            ret :: Nil    

    case _ => 
        
        if parsed.size > 1 then
            collectParseErrors( parsed.tail, line + 1 )

        else 
            Nil

private def canContinue (src: List[Parsed]) : Either[List[Parsed], String] = 
    
    val li = collectParseErrors(src) 
    
    if li.size > 0 then 
        Right(li.reduceLeft( _ + "\n" + _ )) 

    else 
        Left(src) 


private def continueOrNone(src: List[Parsed]) : Option[ List[Parsed] ] = canContinue(src) match
        
    case Left(x) => Some(x) 
    case Right(x) => 
        System.err.println(x + "\n\n") 
        None


private case class PackState (op: List[Parsed], dat: List[Parsed])
private val emptyPackState = PackState(List[Parsed]() , List[Parsed]()) 



private def packOpcode (src: List[Parsed]) : Option[Parsed] = 

    src match

    case Packable(a) :: Nil => 
        Some( Packable( a ) )
    
    case Packable(a) :: Packable(b) :: Nil => 
        Some( Packable( b  + (a << 8) ) )
    
    case Packable(a) :: Packable(b) :: Packable(c) :: Nil => 
        Some( Packable( c + (b << 8) + (a << 16) ))  

    case Packable(a) :: Packable(b) :: Packable(c) :: Packable(d) :: Nil => 
        Some( Packable( (d + (c << 8) + (b << 16) + (a << 24)) ) )

    case FlowControl(a) :: Nil => 
        Some( FlowControl(a) )

    case Label(a) :: Nil => 
        Some( Label(a) )

    case _ => 
        None


/* Looks up to 4 positions away and reorders the list so that data is to the right and operations are to the left */
/* Will be used to pack the data */

private def takePackableWithData (src: List[Parsed], state: PackState = emptyPackState) : (List[Parsed], PackState) = 


    /* Only take 4 (word) at a time */
    if state.op.size < 4 && state.dat.size <= 4 then src match
       
        /* Must match this as well since if we don't the packer might miss data and screw up ordering */
        case Packable(x) :: Data(y) :: tail => 
            takePackableWithData( tail, state.addOp(Packable(x)).addDat(Data(y)) )
       

        /* If it doesn't match above, then it is fine */ 
        case Packable(x) :: tail => 
            takePackableWithData( tail, state.addOp(Packable(x)) )


        case Data(x) :: tail => 
            takePackableWithData( tail, state.addDat(Data(x)) )

        case Resolve(x) :: tail => 
            takePackableWithData( tail, state.addDat(Resolve(x)) ) 

        case FlowControl(x) :: tail => 

            if state.isEmpty then
                (tail, state.addOp(FlowControl(x)))
            
            else
                (src, state)

        case Label(x) :: tail => 

            if state.isEmpty then
                (tail, state.addOp(Label(x)))

            else
                (src, state)

        case _ =>
            (src, state)

    else
       (src, state)



def pack (src: List[Parsed]) : List[Parsed] = 

    if VM_Options.optionPackingEnable then

        val li = takePackableWithData( src )   
        val packedOption = packOpcode( li(1).op ) 

        val packed = packedOption match 
        
            case Some(x) => x
            case None => ErrorMessage("Cannot pack values") 

        if li(0).size > 0 then
            packed :: li(1).dat ::: pack (li(0)) 

        else 
            packed :: li(1).dat
        
    else
        src


extension (xs: PackState) 
    private def addOp (src: Parsed) : PackState = 
        PackState( xs.op ::: src :: Nil, xs.dat )

    private def addDat (src: Parsed) : PackState = 
        PackState( xs.op, xs.dat ::: src :: Nil ) 

    private def isEmpty : Boolean = 

        if xs.op.size == 0 && xs.dat.size == 0 then true
        else false

    private def isFull : Boolean = 
        
        if xs.op.size >= 4 && xs.dat.size >= 4 then true
        else false

private def collectLabelsToMap (
    src: List[Parsed],
    addr: Int = 0, 
    map: HashMap[String, Int] = HashMap.empty[String, Int]
    ) : (HashMap[String, Int], List[Parsed]) =

    if addr < src.size then src(addr) match
        
        case Label(x) => 
            val newSrc = src.filter(i => i match

                case Label(id) if id == x => false
                case _ => true
            )
            collectLabelsToMap( newSrc, addr + 1, map += (x -> addr ))
        case _ => collectLabelsToMap( src, addr + 1, map )

    else
        (map, src) 


private def resolveAndExtract (src: List[Parsed]) : Array[Word] = 

    val result = collectLabelsToMap(src)
    result(1)
    .map( i => i match

        case Resolve(x) => result(0)(x)
        case FlowControl(x) => x
        case Packable(x) => x
        case Data(x) => x
    
    ) .toArray

extension[F] (xs: Option[F]) 
    private def <+> [A] (f: F => A) : Option[A] = xs match

        case Some(value) => Some( f(value) )
        case None => None 

    private def +>[A] (f: F => Option[A]) : Option[A] = xs match

        case Some(value) => f(value) 
        case None => None

    private def <+[A] (f: Option[F] => A) : Option[A] = xs match
        
        case Some(value) => Some(f( xs ))
        case None => None

extension (xs: StateT)

    private def num = matchNum(xs)
    private def id = matchID(xs) 
    private def tag = matchTag(xs)
    private def op = matchOperation(xs)

extension (xs: List[Parsed]) 
    private def continue : Option[ List[Parsed] ] = continueOrNone(xs) 

extension (xs: String)
    
    def assemble : Option[Array[Word]] = 
        val parsed = xs . split("\\n") . flatMap( parseLine compose scanLine ).toList
        
        (parsed.continue <+> pack) +> continue <+> resolveAndExtract

    def assembleAndRun : Option[Instance[Word]] = xs.assemble match

        case Some(arr) =>  
            Some(DefaultInstance(arr).run)  
        case None =>
            None

/* Interfaces */ 

trait InlineAssembler[A] (f: A=>String, buf: A): 
        
    def assembleTo[B](g: Array[Word] => B) : Option[B] = 
        f(buf).assemble <+> g
        
    def assembleAndRun[F, B] : Option[Instance[Word]] = 
        f(buf).assembleAndRun

class DefaultAssembler (s: String) extends InlineAssembler[String]((s) => s, s) 

package nga

import scala.annotation.tailrec
import scala.language.postfixOps

type TokenPair = (Option[String], Option[String])
type Image = Array[Word] 

def getTokenPair (src: String) : Option[TokenPair] = 
  val arr = src.split("\\s+") 
  
  arr.size match
    case 0 => Some( (None, None) )
    case 1 => Some( (Some( arr(0) ), None) )
    case 2 => Some( (Some( arr(0) ), Some( arr(1) ) ) )
    case _ => None

def parse (src: TokenPair) : Option[Array[Word]] = 
  src match
    case (Some("nop"),  None)     => Some(Array(0)) 
    
    case (Some("lit"),  Some(x))  => Some(Array(1, x.toInt))
    
    case (Some("dup"),  None)     => Some(Array(2))
    
    case (Some("drop"), None)     => Some(Array(3))
    
    case (Some("swap"), None)     => Some(Array(4))
    
    case (Some("push"), None)     => Some(Array(5))

    case (Some("pop"),  None)     => Some(Array(6))
     
    case (Some("jump"),  None)     => Some(Array(7))

    case (Some("call"), None)     => Some(Array(8))
    
    case (Some("ccal"), None)     => Some(Array(9))
    
    case (Some("ret"),  None)     => Some(Array(10))
    
    case (Some("eq"),   None)     => Some(Array(11))
    
    case (Some("neq"),  None)     => Some(Array(12))
    
    case (Some("lt"),   None)     => Some(Array(13))
    
    case (Some("gt"),   None)     => Some(Array(14))
    
    case (Some("fetch"), None)      => Some(Array(15))
    
    case (Some("store"), None)      => Some(Array(16))

    case (Some("add"), None)      => Some(Array(17))

    case (Some("sub"), None)      => Some(Array(18))

    case (Some("mul"), None)      => Some(Array(19))

    case (Some("div"), None)      => Some(Array(20))

    case (Some("and"), None)      => Some(Array(21))

    case (Some("or"), None)       => Some(Array(22))

    case (Some("xor"), None)      => Some(Array(23))

    case (Some("shft"), None)    => Some(Array(24))
   
    case (Some("zret"), None)    => Some(Array(25))
    
    case (Some("end"), None)     => Some(Array(26))

    case (None, None)         => Some(Array())
    case (Some(""), None)     => Some(Array())
    case (Some(""), Some("")) => Some(Array()) 
    case (_, _) => None 

/* Currently does not support Packing at this time */ 

def assemble (src: String) : Image = 
 
  val img = src.split("\\n+") . flatMap(getTokenPair) . flatMap(parse) . flatten 

  if VM_Options.optionPackingEnable == false then
    img

  else 
    VM_Options.optionPackingEnable = false 
    img

extension (xs: String)
  def toImage = 
    assemble(xs)

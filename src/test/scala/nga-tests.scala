package nga

import org.scalatest.funsuite.AnyFunSuite

val simpleStackTestNoPacking = Array (0x1, 10, 0x1A)
val simpleStackTestPacking = Array(0x11A, 10)

/* (1+1) -> (2*2) -> (4/4) -> (1 - 0) -> (1 ^ 1) */

class TestSuite extends AnyFunSuite:
  
  test("Construct instance") {
    val inst = NGAInstance(Array(0)) 
  }

  test("Run NOP - w/o packing") {
    VM_Options.optionPackingEnable = false
    
    val inst = NGAInstance(Array(0, 0x1A))
    inst.run
  }

  test("Run NOP - w/ packing") {
    VM_Options.optionPackingEnable = true

    val inst = NGAInstance(Array(0x1A)) 
    inst.run
  }
  


package nga

import org.scalatest.funsuite.AnyFunSuite

val simpleStackTestNoPacking = Array (0x1, 10, 0x1A)
val simpleStackTestPacking = Array(0x11A, 10)

val short_test = s"""
lit 1
dup
add 
end
""".toImage

/* (1+1) -> (2*2) -> (4/4) -> (1 - 0) -> (1 ^ 1) */

val operator_test = s"""
lit 1
dup 
add 

lit 2
mul

lit 4
div

swp
sub

lit -1 
mul 

lit 1
xor 

end
""".toImage


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

  test("Run small assmebled binary") {
    VM_Options.optionPackingEnable = false

    val inst = NGAInstance(short_test)
    inst.run
    assert(inst.data.tos == 2)
  }
  
  test("Test operations with assembled binary") {
    VM_Options.optionPackingEnable = false

    val inst = NGAInstance(operator_test)
    inst.run
    assert(inst.data.tos == 0)
  }
  


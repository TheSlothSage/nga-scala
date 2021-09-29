package vm

import org.scalatest.funsuite.AnyFunSuite

val simpleStackTestNoPacking = Array (0x1, 10, 0x1A)
val simpleStackTestPacking = Array(0x11A, 10)



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

  test("Push 10 to stack - w/o packing") {
    VM_Options.optionPackingEnable = false

    val inst = NGAInstance(simpleStackTestNoPacking)
    inst.run
    assert(inst.data.tos == 10)
  }

  test("Push 10 to stack - w/ packing") {
    VM_Options.optionPackingEnable = true
    
    val inst = NGAInstance(simpleStackTestPacking)
    inst.run
    assert(inst.data.tos == 10) 
  }

  test("Add w/o packing") {
    VM_Options.optionPackingEnable = false

    val inst = NGAInstance(Array(1, 10, 1, 20, 17, 26))
    inst.run
    assert(inst.data.tos == 30)
  }
 
  test("Add w/ packing") {
    VM_Options.optionPackingEnable = true

    val inst = NGAInstance(Array(0x101111A, 10, 20))
    inst.run
    assert(inst.data.tos == 30)
  }


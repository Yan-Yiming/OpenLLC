/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package openLLC

import chisel3._
import chisel3.util._
import utility._
import coupledL2.tl2tl._
import coupledL2.tl2chi._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

class AtomicsMainPipe(implicit p: Parameters) extends LLCBundle {
  val task = new Task()
  val dataisfromDS = Bool()
  val datafromDS = new DSBlock()
}

class AMOALU() extends Module {
  val io = IO(new Bundle {
//    val mask = Input(UInt((operandBits/8).W))
//    val opcode = Input(Bits(3.W))
//    val param = Input(Bits(3.W))
//    val lhs = Input(Bits(operandBits.W))
//    val rhs = Input(Bits(operandBits.W))
    val opt = Input(UInt(7.W))
    val out = Output(UInt(64.W))
    val data1 = Input(UInt(64.W))
    val data2 = Input(UInt(64.W))
  })
  def REQ_OPCODE_WIDTH      = 7
  def AtomicStore_ADD       = 0x28.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_CLR       = 0x29.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_EOR       = 0x2A.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_SET       = 0x2B.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_SMAX      = 0x2C.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_SMIN      = 0x2D.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_UMAX      = 0x2E.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_UMIN      = 0x2F.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_ADD        = 0x30.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_CLR        = 0x31.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_EOR        = 0x32.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_SET        = 0x33.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_SMAX       = 0x34.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_SMIN       = 0x35.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_UMAX       = 0x36.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_UMIN       = 0x37.U(REQ_OPCODE_WIDTH.W)
  def AtomicSwap            = 0x38.U(REQ_OPCODE_WIDTH.W)
  def AtomicCompare         = 0x39.U(REQ_OPCODE_WIDTH.W)
  val isAMOADD = (io.opt === AtomicLoad_ADD)
  val isAMOSWAP = (io.opt === AtomicSwap)

  io.out := Mux(isAMOADD, io.data1 + io.data2, io.data1)

//  val max = io.opcode === ArithmeticData && (io.param === 1.U || io.param === 3.U) // M_XA_MAX || M_XA_MAXU
//  val min = io.opcode === ArithmeticData && (io.param === 0.U || io.param === 2.U) // M_XA_MIN || M_XA_MINU
//  val add = io.opcode === ArithmeticData && io.param === 4.U // M_XA_ADD
//  val logic_and = io.opcode === LogicalData && (io.param === 1.U || io.param === 2.U) // M_XA_OR || M_XA_AND
//  val logic_xor = io.opcode === LogicalData && io.param === 0.U // M_XA_XOR || M_XA_OR

}

class AtomicsUnitL3(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val fromMainPipe = Flipped(ValidIO(new AtomicsMainPipe()))

    val fromResponseUnit = Flipped(ValidIO(new AtomicsInfo()))

    val AMOrefillTask = DecoupledIO(new Task())
    val datafromAMO = Output(UInt(64.W))
    val blockfromAMO = Output(new DSBlock())

    val blockReqArq = Output(Bool())
  })

  
  def data_trans_blockto64(data_block: UInt, offset: UInt): UInt = {
    val off_valid = (offset === 0.U) || (offset === 8.U) || (offset === 16.U) || (offset === 32.U) ||
      (offset === 40.U) || (offset === 48.U) || (offset === 56.U) || (offset === 24.U)
    assert(off_valid, "amo_data not alias L3")
    val data_ret = (data_block >> (offset * 8.U)).asUInt
    data_ret(63, 0)
  }
  def data_trans_64toblock(data_block: UInt, offset: UInt, new_data: UInt): UInt = {
    val off_valid = (offset === 0.U) || (offset === 8.U) || (offset === 16.U) || (offset === 32.U) ||
      (offset === 40.U) || (offset === 48.U) || (offset === 56.U) || (offset === 24.U)
    assert(off_valid, "amo_data not alias L3")

    val OFFSET = (offset * 8.U)
    val tmp1 = (1.U << OFFSET) - 1.U
    val tmp2 = data_block & tmp1

    val new_data_block0 = (data_block >> (OFFSET + 64.U)).asUInt
    val new_data_block1 = ((new_data_block0 << 64.U) | new_data).asUInt
    val new_data_block2 = (new_data_block1 << OFFSET).asUInt | tmp2

    new_data_block2
  }

  val amoalu = Module(new AMOALU)
  val s_invalid :: s_getMainPipe :: s_getResponseUnit :: Nil = Enum(3)
  val state = RegInit(s_invalid)
  val dataisfromDS = RegInit(false.B)
  val datafromDS = RegInit(0.U(512.W))
  val datafromL2 = RegInit(0.U(64.W))
  val datafromSNPorMEM = RegInit(0.U(512.W))
  val Result = RegInit(0.U(64.W))
  val databacktoL2 = RegInit(0.U(64.W))
  val offset = RegInit(0.U(6.W))

  val Task = RegInit(0.U.asTypeOf(new Task()))

  when (state === s_invalid) {
    when (io.fromMainPipe.valid) {
      dataisfromDS := io.fromMainPipe.bits.dataisfromDS
      datafromDS := io.fromMainPipe.bits.datafromDS.data.asUInt
      offset := io.fromMainPipe.bits.task.off
      Task := io.fromMainPipe.bits.task
      state := s_getMainPipe
    }
  }

  when (state === s_getMainPipe) {
    when (io.fromResponseUnit.valid) {
      datafromL2 := io.fromResponseUnit.bits.ncbwrdata
      datafromSNPorMEM := io.fromResponseUnit.bits.old_data.asUInt
      state := s_getResponseUnit
    }
  }

  when (state === s_getResponseUnit) {
    Result := amoalu.io.out
    when (io.AMOrefillTask.fire) {
      state := s_invalid
    }
  }

//  amoalu.io.mask := io.fromMainPipe.atomicsRequest.bits.amo_mask
//  amoalu.io.opcode := io.fromMainPipe.atomicsRequest.bits.opcode
//  amoalu.io.param := io.fromMainPipe.atomicsRequest.bits.param
//  amoalu.io.lhs := io.fromMainPipe.atomicsRequest.bits.old_data
//  amoalu.io.rhs := io.fromMainPipe.atomicsRequest.bits.amo_data
  val old_data_block = Mux(dataisfromDS, datafromDS, datafromSNPorMEM)
  amoalu.io.data1 := datafromL2
  amoalu.io.data2 := data_trans_blockto64(old_data_block, offset)
  amoalu.io.opt := Task.chiOpcode

  io.AMOrefillTask.valid := (state === s_getResponseUnit)
  io.AMOrefillTask.bits := Task
  io.AMOrefillTask.bits.AMOrefillTask := true.B

  io.datafromAMO := data_trans_blockto64(old_data_block, offset)
  io.blockfromAMO := data_trans_64toblock(old_data_block, offset, Result).asTypeOf(new DSBlock())
  io.blockReqArq := !(state === s_invalid)
}
package goosea.device

import goosea.utils._
import goosea.mem._

sealed trait Device {
  def name: String
  def vendorID: U16
  def deviceID: U16
  def init: Option[Seq[(U64, U64)]]
  def destroy: Option[Unit]
  def dmaRead(addr: U64):Option[Mem]
  def dmaWrite(addr: U64):Option[Mem]
  def mmioRead(addr:U64):Option[Byte]
  def mmioWrite(addr:U64,value:Byte):Option[Unit]
  def isInterrupting:Option[U64]
  // TODO
}
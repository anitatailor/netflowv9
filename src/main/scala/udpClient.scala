package starter
import java.net.DatagramPacket
import java.net.DatagramSocket
import java.nio.ByteBuffer

object udp_echo_server {

  def intToIp(i: Int): String =
    return ((i >> 24) & 0xFF) + "." +
    ((i >> 16) & 0xFF) + "." +
    ((i >> 8) & 0xFF) + "." +
    (i & 0xFF);
  val bufsize = 65535 // max UDP buffer size
  val port = 9999
  val netflowHeaderSize = 24
  val netflowRecordSize = 48
  val netflowMaxCount = 30

  def main(args: Array[String]): Unit = {

    println("udp echoserver started...")

    val sock = new DatagramSocket(port)
    val buf = new Array[Byte](bufsize)
    val packet = new DatagramPacket(buf, bufsize)

    while (true) {
      sock.receive(packet)

      println("received packet from: " + packet.getAddress() + "Of Length" + packet.getLength())
      val byteArray = packet.getData()
      val size = byteArray.size
      val byteBuffer = ByteBuffer.wrap(byteArray);
      val version = byteBuffer.getShort(0)
      val div: Short = 1
      println("Version: " + byteBuffer.getShort(0))
      println("Count: " + byteBuffer.getShort(2))
      val pkt = NetFlowV9ExportPacket(byteBuffer, size)
      /*
		for ( i <- 1 to count) {
				println("Reading first netwflow packet")
  			println("srcIP: " + intToIp(byteBuffer.getInt(offset + 0)))
  			println("dstIp: " + intToIp(byteBuffer.getInt(offset + 4)))
  			println("inputIfIndex: " + byteBuffer.getShort(offset + 12))
  			println("outputIfIndex: " + byteBuffer.getShort(offset + 14))
  			println("numPackets: " + byteBuffer.getInt(offset + 16))
  			println("first: " + byteBuffer.getInt(offset + 24))
  			println("last: " + byteBuffer.getInt(offset + 28))
  			println("srcPort: " + byteBuffer.getShort(offset + 30))
  			println("dstPort: " + byteBuffer.getShort(offset + 32))
				offset = offset + netflowRecordSize
		}
     */
    }

    /*
  println("count: " + byteBuffer.getInt(12))
  println("count: " + byteBuffer.getInt(16))
  println("count: " + byteBuffer.getShort(20))
  println("count: " + byteBuffer.getShort(22))
   */
  }
}

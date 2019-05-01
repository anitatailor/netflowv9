package starter
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

abstract class NetflowRecord extends Serializable

// after V9 header there will be a flowset header

case class NetflowV9FlowSetHeader(val flowsetId: Short, val flowsetLength: Short)

object NetFlowV9ExportPacket {
  private val headerSize = 20

  def intToIp(i: Int): String =
    return ((i >> 24) & 0xFF) + "." +
    ((i >> 16) & 0xFF) + "." +
    ((i >> 8) & 0xFF) + "." +
    (i & 0xFF);

  def apply(byteBuffer: ByteBuffer, pktLength: Int) = {

    // pkt has header
    // multiple flowsets
    // if it is template flowset it will have multiple templates

    var packetOffset = headerSize // starts just after header
    var version = byteBuffer.getShort(0)
    var count = byteBuffer.getShort(2)
    var flowsetCounter = 0
    println("Netflow version " + version + "Count " + count + "pktLength:" + pktLength)

    // extract  v9 header out of byteBuffer
    val throwBuffer = new Array[Byte](headerSize)
    byteBuffer.get(throwBuffer, 0, headerSize)
    //val flows = scala.collection.mutable.ArrayBuffer[Flow[_]]()
    while (flowsetCounter < count) {
      // read and remove from byteBuffer
      val flowsetId = byteBuffer.getShort()
      var flowsetLength: Int = byteBuffer.getShort().toInt
      println("FlowSet Id " + flowsetId + "FlowsetLength " + flowsetLength)
      flowsetLength = flowsetLength - 4 // remove flowsetid and it's length

      // flowsetLength includes here template id and field cound
      flowsetId match {
        case 0 => // template flowset
          println("Template FlowSet (" + flowsetId + ")")
          do {
            // throw away template id and field count
            val templateId = byteBuffer.getShort()
            val fieldCount = byteBuffer.getShort()
            println(
              "Template ID (" + templateId + ")" + "Field count" + fieldCount + "remaining buffer" + byteBuffer
                .remaining()
            )

            // add 4 for templateId and field count, each field has type and length
            val templateSize = fieldCount * 4
            if (templateSize < pktLength) {
              val templateBufArray = new Array[Byte](templateSize)
              // possition of byteBuffer is imcremented by templateSize after below call
              byteBuffer.get(templateBufArray, 0, templateSize)
              val templateBufer = ByteBuffer.wrap(templateBufArray)
              println("Read Template Buffer of Size  (" + templateSize + ")")
              NetFlowV9Templates(templateBufer, templateId, fieldCount)
              for (i <- 1 to fieldCount) {
                var fieldType = templateBufer.getShort()
                var fieldLen = templateBufer.getShort()
                //     	   			println("Field Type:" + fieldType + "Field Len:" + fieldLen)
              }
              flowsetLength = flowsetLength - templateSize - 4
            }
          } while (byteBuffer.remaining() <= flowsetLength)
          flowsetCounter += 1

        case 1 => // option template flowset
          println("OptionTemplate FlowSet (" + flowsetId + ")")
          flowsetCounter += 1

        case a: Short if a > 255 => // data flowset - flowsetId is same as template id
          println(
            "Data flow set TemplateId (" + a + ")" + "length" + flowsetLength + "Remaining in Buffer:" + byteBuffer
              .remaining()
          )
          // it has more than one records as per defined by Template
          NetFlowV9Templates.templateList.get(a) match {
            case Some(template: HashMap[String, Int]) => {
              println("Found template")
              if (a.toInt == 256) {
                val recordLengthAsPerTempl = NetFlowV9Templates.getRecordLength(template)

                val noOfDataRecords = flowsetLength / recordLengthAsPerTempl
                for (i <- 1 to noOfDataRecords) {
                  // possition of byteBuffer is imcremented by templateSize after below call
                  val dataByteArray = new Array[Byte](recordLengthAsPerTempl)
                  byteBuffer.get(dataByteArray, 0, recordLengthAsPerTempl)
                  val dataBuffer = ByteBuffer.wrap(dataByteArray)
                  val pkt = NetFlowV9Data(dataBuffer, template)
                }
              }
            }
            case None => None
          }

          flowsetCounter += 1
        case a: Short =>
          println("Unexpected TemplateId (" + a + ")")
          flowsetCounter += 1

      }
    }
  }
}

object NetFlowV9Templates {

  // tree map of templateId and HashMap[String, AnyVal] which has field offset and it's val
  val templateList = new HashMap[Short, HashMap[String, Int]]()

  def apply(templateBuffer: ByteBuffer, templateId: Short, fieldCount: Short) = {

    println("In apply of NetFlowV9Templates  (" + templateId + ")")
    var templateMap = new HashMap[String, Int]() { override def default(key: String) = -1 }
    var offset = 0
    var idx = 0
    var templBufferOffset, dataFlowSetOffset = 0
    while (idx < fieldCount) {
      val typeName = templateBuffer.getShort(templBufferOffset)
      val typeLen = templateBuffer.getShort(templBufferOffset + 2)
      if (typeName < 93 && typeName > 0) {
        println("Inseting tyeName" + typeName + "Offset" + dataFlowSetOffset + "Length")
        templateMap.put("offset_" + typeName, dataFlowSetOffset)
        templateMap.put("length_" + typeName, typeLen)
        //templateMap += ("offset_" + typeName -> dataFlowSetOffset, "length_" + typeName -> typeLen)
      }
      dataFlowSetOffset += typeLen
      templBufferOffset += 4
      idx += 1
    }
    val recordLength = dataFlowSetOffset
    templateMap.put("recordLength", dataFlowSetOffset)

    // template read, insert it to template list
    val key = templateId
    templateList.remove(key)
    templateList.put(key, templateMap)
  }

  def typeOffset(typeName: TemplateFields.Value, templateMap: HashMap[String, Int]): Int = {

//				println ("Fetching tyeName" + typeName.id)
    val key = "offset_" + typeName.id
    templateMap.getOrElse(key, -1)
  }

  def typeLen(typeName: TemplateFields.Value, templateMap: HashMap[String, Int]): Int = {

    val key = "length_" + typeName.id
    templateMap.getOrElse(key, -1)
  }

  def getRecordLength(templateMap: HashMap[String, Int]): Int = {

    val key = "recordLength"
    templateMap.getOrElse(key, -1)
  }
}

object NetFlowV9Data {
  import TemplateFields._

  def intToIp(i: Int): String =
    return ((i >> 24) & 0xFF) + "." +
    ((i >> 16) & 0xFF) + "." +
    ((i >> 8) & 0xFF) + "." +
    (i & 0xFF);

  def apply(byteBuffer: ByteBuffer, template: HashMap[String, Int]): NetFlowV9Data = {

    // get offset of L4_SRC_PORT from template and no of bytes to read . thats length.
    //println("In apply of netflow V9 data")

    var offset = 0
    var length = 1
//NetFlowV9Templates.typeLen(IPV4_SRC_ADDR,template)
    offset = NetFlowV9Templates.typeOffset(IPV4_SRC_ADDR, template)
    val srcAddr = byteBuffer.getInt(offset)
    offset = NetFlowV9Templates.typeOffset(IPV4_DST_ADDR, template)
    val dstAddr = byteBuffer.getInt(offset)
    offset = NetFlowV9Templates.typeOffset(L4_SRC_PORT, template)
    val srcPort = byteBuffer.getShort(offset)
    offset = NetFlowV9Templates.typeOffset(L4_DST_PORT, template)
    val dstPort = byteBuffer.getShort(offset)

    offset = NetFlowV9Templates.typeOffset(LAST_SWITCHED, template)
    val flowEndMs = byteBuffer.getInt(offset)
    offset = NetFlowV9Templates.typeOffset(FIRST_SWITCHED, template)
    val flowStartMs = byteBuffer.getInt(offset)

    /*
offset = NetFlowV9Templates.typeOffset(DIRECTION,template)
				println ("Fetching offset" + offset)
var direction:Byte = 0
if (offset != -1)
 direction = byteBuffer.get(offset)
				println ("Fetching offset" + offset)
     */
    offset = NetFlowV9Templates.typeOffset(INPUT_SNMP, template)
    val inputIfIndex = byteBuffer.getInt(offset)
    offset = NetFlowV9Templates.typeOffset(OUTPUT_SNMP, template)
    val outputIfIndex = byteBuffer.getInt(offset)
    offset = NetFlowV9Templates.typeOffset(InBYTES, template)

    val numOctets = byteBuffer.getInt(offset)
    offset = NetFlowV9Templates.typeOffset(InPKTS, template)
    val numPkts = byteBuffer.getInt(offset)

    offset = NetFlowV9Templates.typeOffset(MPLS_LABEL_1, template)
    val mplsLabel_1 = byteBuffer.getInt(offset) >> 4 // it coutn be 3 bytes
    offset = NetFlowV9Templates.typeOffset(MPLS_LABEL_2, template)
    val mplsLabel_2 = byteBuffer.getInt(offset) >> 4
    offset = NetFlowV9Templates.typeOffset(MPLS_TOP_LABEL_TYPE, template)
    val mplsLabelType = byteBuffer.get(offset)

    println(
      "SRC IP" + intToIp(srcAddr) + "DstIP" + intToIp(dstAddr) + "Num PKT" + numPkts + "MPLS label1:" + mplsLabel_1 + "MPLS label2:" + mplsLabel_2
    )
    //NetFlowV9Data(flowStartMs,flowEndMs,direction,srcAddr,srcPort,dstAddr,dstPort,inputIfIndex,outputIfIndex,numPkts,numOctets)
    NetFlowV9Data(
      flowStartMs,
      flowEndMs,
      srcAddr,
      srcPort,
      dstAddr,
      dstPort,
      inputIfIndex,
      outputIfIndex,
      mplsLabelType,
      mplsLabel_1,
      mplsLabel_2,
      numPkts,
      numOctets
    )
  }
}

case class NetFlowV9Data(
  flowStartMs: Int,
  flowEndMs: Int,
  //direction:Byte,
  srcAddr: Int,
  srcPort: Short,
  dstAddr: Int,
  dstPort: Short,
  inputIfIndex: Int,
  outputIfIndex: Int,
  mplsLabelType: Byte,
  mplsLabel_1: Int,
  mplsLabel_2: Int,
  /*
  tcpFlag:Byte,
  prot:Byte,
  tos:Byte,
  srcAs:Short,
  dstAs:Short,
   */
  numPkts: Int,
  numOctets: Int
) extends NetflowRecord

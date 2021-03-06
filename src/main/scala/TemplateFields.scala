package starter

/*
 TEMPLS 256
Field Type:22Field Len:4^M
Field Type:21Field Len:4^M
Field Type:6Field Len:1^M
Field Type:4Field Len:1^M
Field Type:5Field Len:1^M
Field Type:10Field Len:4^M
Field Type:14Field Len:4^M
Field Type:7Field Len:2^M
Field Type:11Field Len:2^M
Field Type:8Field Len:4^M
Field Type:12Field Len:4^M
Field Type:2Field Len:4^M
Field Type:1Field Len:4^M
Field Type:47Field Len:4^M
Field Type:70Field Len:4^M
Field Type:71Field Len:4^M
Field Type:46Field Len:1^M



Fetching tyeName8
Fetching offset32
Fetching tyeName12
Fetching offset36
Fetching tyeName7
Fetching offset20
Fetching tyeName11
Fetching offset22
Fetching tyeName21
Fetching offset4
Fetching tyeName22
Fetching offset0
Fetching tyeName10
Fetching offset12
Fetching tyeName14
Fetching offset16
Fetching tyeName1
Fetching offset44
Fetching tyeName2
Fetching offset40
Fetching tyeName70
Fetching offset-1

 */
object TemplateFields extends Enumeration {
  val InBYTES = Value(1, "InBytes")
  val InPKTS = Value(2, "InPkts")
  val FLOWS = Value(3, "Flows")
  val PROT = Value(4, "Proto")
  val SRC_TOS = Value(5, "SrcTOS")
  val TCP_FLAGS = Value(6, "TCPFlags")
  val L4_SRC_PORT = Value(7, "L4SrcPort")
  val IPV4_SRC_ADDR = Value(8, "IPv4SrcAddr")
  val SRC_MASK = Value(9, "SrcMask")
  val INPUT_SNMP = Value(10, "InputSNMP")
  val L4_DST_PORT = Value(11, "L4DstPort")
  val IPV4_DST_ADDR = Value(12, "IPv4DstAddr")
  val DST_MASK = Value(13, "DstMask")
  val OUTPUT_SNMP = Value(14, "OutputSNMP")
  val IPV4_NEXT_HOP = Value(15, "IPv4NextHop")
  val SRC_AS = Value(16, "SrcAS")
  val DST_AS = Value(17, "DstAS")
  val BGP_IPV4_NEXT_HOP = Value(18, "BGPIPv4NextHop")
  val IPM_DPKTS = Value(19, "IPMpkts")
  val IPM_DOCTETS = Value(20, "IPMoctets")
  val LAST_SWITCHED = Value(21, "LastSwitched")
  val FIRST_SWITCHED = Value(22, "FirstSwitched")
  val OutBYTES = Value(23, "OutBytes")
  val OutPKTS = Value(24, "OutPkts")
  val MIN_PKT_LEN = Value(25, "MinPktLen")
  val MAX_PKT_LEN = Value(26, "MaxPktLen")
  val IPV6_SRC_ADDR = Value(27, "IPv6SrcAddr")
  val IPV6_DST_ADDR = Value(28, "IPv6DstAddr")
  val IPV6_SRC_MASK = Value(29, "IPv6SrcMask")
  val IPV6_DST_MASK = Value(30, "IPv6DstMask")
  val IPV6_FLOW_LABEL = Value(31, "IPv6FlowLabel")
  val ICMP_TYPE = Value(32, "ICMPtype")
  val MUL_IGMP_TYPE = Value(33, "MulIGMPtype")
  val SAMPLING_INTERVAL = Value(34, "SamplingInterval")
  val SAMPLING_ALGORITHM = Value(35, "SamplingAlgo")
  val FLOW_ACTIVE_TIMEOUT = Value(36, "FlowActiveTimeout")
  val FLOW_INACTIVE_TIMEOUT = Value(37, "FlowInactiveTimeout")
  val ENGINE_TYPE = Value(38, "EngineType")
  val ENGINE_ID = Value(39, "EngineID")
  val TOTAL_BYTES_EXPORTED = Value(40, "TotalBytesExported")
  val TOTAL_EXPORT_PKTS_SENT = Value(41, "TotalExportPktsSent")
  val TOTAL_FLOWS_EXPORTED = Value(42, "TotalFlowsExported")
  val IPV4_SRC_PREFIX = Value(44, "IPv4SrcPrefix")
  val IPV4_DST_PREFIX = Value(45, "IPv4DstPrefix")
  val MPLS_TOP_LABEL_TYPE = Value(46, "MPLSTopLabelType")
  val MPLS_TOP_LABEL_IP_ADDR = Value(47, "MPLSTopLabelIPAddr")
  val FLOW_SAMPLER_ID = Value(48, "FlowSamplerId")
  val FLOW_SAMPLER_MODE = Value(49, "FlowSamplerMode")
  val FLOW_SAMPLER_RANDOM_INTERVAL = Value(50, "FlowSamplerRandomInterval")
  val MIN_TTL = Value(52, "MinTTL")
  val MAX_TTL = Value(53, "MaxTTL")
  val IPV4_IDENT = Value(54, "IPv4Ident")
  val DST_TOS = Value(55, "DstTOS")
  val IN_SRC_MAC = Value(56, "InSrcMAC")
  val OUT_DST_MAC = Value(57, "OutDstMAC")
  val SRC_VLAN = Value(58, "SrcVLAN")
  val DST_VLAN = Value(59, "DstVLAN")
  val IP_PROTOCOL_VERSION = Value(60, "IPVersion")
  val DIRECTION = Value(61, "Direction")
  val IPV6_NEXT_HOP = Value(62, "IPv6NextHop")
  val BGP_IPV6_NEXT_HOP = Value(63, "BGPIPv6NextHop")
  val IPV6_OPTION_HEADERS = Value(64, "IPv6OptionHeaders")
  val MPLS_LABEL_1 = Value(70, "MPLSLabel1")
  val MPLS_LABEL_2 = Value(71, "MPLSLabel2")
  val MPLS_LABEL_3 = Value(72, "MPLSLabel3")
  val MPLS_LABEL_4 = Value(73, "MPLSLabel4")
  val MPLS_LABEL_5 = Value(74, "MPLSLabel5")
  val MPLS_LABEL_6 = Value(75, "MPLSLabel6")
  val MPLS_LABEL_7 = Value(76, "MPLSLabel7")
  val MPLS_LABEL_8 = Value(77, "MPLSLabel8")
  val MPLS_LABEL_9 = Value(78, "MPLSLabel9")
  val MPLS_LABEL_10 = Value(79, "MPLSLabel10")
  val IN_DST_MAC = Value(80, "InDstMAC")
  val OUT_SRC_MAC = Value(81, "OutSrcMAC")
  val IF_NAME = Value(82, "IfName")
  val IF_DESC = Value(83, "IfDesc")
  val SAMPLER_NAME = Value(84, "SamplerName")
  val IN_PERMANENT_BYTES = Value(85, "InPermanentBytes")
  val IN_PERMANENT_PKTS = Value(86, "InPermanentPkts")
}
// vim: set ts=4 sw=4 et:

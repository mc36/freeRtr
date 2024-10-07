/*
 * Copyright 2019-present GEANT RARE project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed On an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// source here:
// https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml

#ifndef _IP_PROTOCOL_P4_
#define _IP_PROTOCOL_P4_

const bit<8> IP_PROTOCOL_HOPOPT = 0;
const bit<8> IP_PROTOCOL_ICMP = 1;
const bit<8> IP_PROTOCOL_IGMP = 2;
const bit<8> IP_PROTOCOL_GGP = 3;
const bit<8> IP_PROTOCOL_IPV4 = 4;
const bit<8> IP_PROTOCOL_ST = 5;
const bit<8> IP_PROTOCOL_TCP = 6;
const bit<8> IP_PROTOCOL_CBT = 7;
const bit<8> IP_PROTOCOL_EGP = 8;
const bit<8> IP_PROTOCOL_IGP = 9;
const bit<8> IP_PROTOCOL_BBN_RCC_MON = 10;
const bit<8> IP_PROTOCOL_NVP_II = 11;
const bit<8> IP_PROTOCOL_PUP = 12;
const bit<8> IP_PROTOCOL_ARGUS = 13;
const bit<8> IP_PROTOCOL_EMCON = 14;
const bit<8> IP_PROTOCOL_XNET = 15;
const bit<8> IP_PROTOCOL_CHAOS = 16;
const bit<8> IP_PROTOCOL_UDP = 17;
const bit<8> IP_PROTOCOL_TMUX = 18;
const bit<8> IP_PROTOCOL_DCN_MEAS = 19;
const bit<8> IP_PROTOCOL_HMP = 20;
const bit<8> IP_PROTOCOL_PRM = 21;
const bit<8> IP_PROTOCOL_XNS_IDP = 22;
const bit<8> IP_PROTOCOL_TRUNK_1 = 23;
const bit<8> IP_PROTOCOL_TRUNK_2 = 24;
const bit<8> IP_PROTOCOL_LEAF_1 = 25;
const bit<8> IP_PROTOCOL_LEAF_2 = 26;
const bit<8> IP_PROTOCOL_RDP = 27;
const bit<8> IP_PROTOCOL_IRTP = 28;
const bit<8> IP_PROTOCOL_ISO_TP4 = 29;
const bit<8> IP_PROTOCOL_NETBLT = 30;
const bit<8> IP_PROTOCOL_MFE_NSP = 31;
const bit<8> IP_PROTOCOL_MERIT_INP = 32;
const bit<8> IP_PROTOCOL_DCCP = 33;
const bit<8> IP_PROTOCOL_3PC = 34;
const bit<8> IP_PROTOCOL_IDPR = 35;
const bit<8> IP_PROTOCOL_XTP = 36;
const bit<8> IP_PROTOCOL_DDP = 37;
const bit<8> IP_PROTOCOL_IDPR_CMTP = 38;
const bit<8> IP_PROTOCOL_TP = 39;
const bit<8> IP_PROTOCOL_IL = 40;
const bit<8> IP_PROTOCOL_IPV6 = 41;
const bit<8> IP_PROTOCOL_SDRP = 42;
const bit<8> IP_PROTOCOL_IPV6_ROUTE = 43;
const bit<8> IP_PROTOCOL_IPV6_FRAG = 44;
const bit<8> IP_PROTOCOL_IDRP = 45;
const bit<8> IP_PROTOCOL_RSVP = 46;
const bit<8> IP_PROTOCOL_GRE = 47;
const bit<8> IP_PROTOCOL_DSR = 48;
const bit<8> IP_PROTOCOL_BNA = 49;
const bit<8> IP_PROTOCOL_ESP = 50;
const bit<8> IP_PROTOCOL_AH = 51;
const bit<8> IP_PROTOCOL_I_NLSP = 52;
const bit<8> IP_PROTOCOL_SWIPE = 53;
const bit<8> IP_PROTOCOL_NARP = 54;
const bit<8> IP_PROTOCOL_MOBILE = 55;
const bit<8> IP_PROTOCOL_TLSP = 56;
const bit<8> IP_PROTOCOL_SKIP = 57;
const bit<8> IP_PROTOCOL_IPV6_ICMP = 58;
const bit<8> IP_PROTOCOL_IPV6_NONXT = 59;
const bit<8> IP_PROTOCOL_IPV6_OPTS = 60;
const bit<8> IP_PROTOCOL_CFTP = 62;
const bit<8> IP_PROTOCOL_SAT_EXPAK = 64;
const bit<8> IP_PROTOCOL_KRYPTOLAN = 65;
const bit<8> IP_PROTOCOL_RVD = 66;
const bit<8> IP_PROTOCOL_IPPC = 67;
const bit<8> IP_PROTOCOL_SAT_MON = 69;
const bit<8> IP_PROTOCOL_VISA = 70;
const bit<8> IP_PROTOCOL_IPCV = 71;
const bit<8> IP_PROTOCOL_CPNX = 72;
const bit<8> IP_PROTOCOL_CPHB = 73;
const bit<8> IP_PROTOCOL_WSN = 74;
const bit<8> IP_PROTOCOL_PVP = 75;
const bit<8> IP_PROTOCOL_BR_SAT_MON = 76;
const bit<8> IP_PROTOCOL_SUN_ND = 77;
const bit<8> IP_PROTOCOL_WB_MON = 78;
const bit<8> IP_PROTOCOL_WB_EXPAK = 79;
const bit<8> IP_PROTOCOL_ISO_IP = 80;
const bit<8> IP_PROTOCOL_VMTP = 81;
const bit<8> IP_PROTOCOL_SECURE_VMTP = 82;
const bit<8> IP_PROTOCOL_VINES = 83;
const bit<8> IP_PROTOCOL_TTP   = 84;
const bit<8> IP_PROTOCOL_IPTM = 84;
const bit<8> IP_PROTOCOL_NSFNET_IGP = 85;
const bit<8> IP_PROTOCOL_DGP  = 86;
const bit<8> IP_PROTOCOL_TCF = 87;
const bit<8> IP_PROTOCOL_EIGRP = 88;
const bit<8> IP_PROTOCOL_OSPFIGP = 89;
const bit<8> IP_PROTOCOL_SPRITE_RPC = 90;
const bit<8> IP_PROTOCOL_LARP = 91;
const bit<8> IP_PROTOCOL_MTP = 92;
const bit<8> IP_PROTOCOL_AX25 = 93;
const bit<8> IP_PROTOCOL_IPIP = 94;
const bit<8> IP_PROTOCOL_MICP = 95;
const bit<8> IP_PROTOCOL_SCC_SP = 96;
const bit<8> IP_PROTOCOL_ETHERIP = 97;
const bit<8> IP_PROTOCOL_ENCAP = 98;
const bit<8> IP_PROTOCOL_GMTP = 100;
const bit<8> IP_PROTOCOL_IFMP = 101;
const bit<8> IP_PROTOCOL_PNNI = 102;
const bit<8> IP_PROTOCOL_PIM = 103;
const bit<8> IP_PROTOCOL_ARIS = 104;
const bit<8> IP_PROTOCOL_SCPS = 105;
const bit<8> IP_PROTOCOL_QNX = 106;
const bit<8> IP_PROTOCOL_A_N = 107;
const bit<8> IP_PROTOCOL_IPCOMP = 108;
const bit<8> IP_PROTOCOL_SNP = 109;
const bit<8> IP_PROTOCOL_COMPAQ_PEER = 110;
const bit<8> IP_PROTOCOL_IPX_IN_IP = 111;
const bit<8> IP_PROTOCOL_VRRP = 112;
const bit<8> IP_PROTOCOL_PGM = 113;
const bit<8> IP_PROTOCOL_L2TP = 115;
const bit<8> IP_PROTOCOL_DDX = 116;
const bit<8> IP_PROTOCOL_IATP = 117;
const bit<8> IP_PROTOCOL_STP = 118;
const bit<8> IP_PROTOCOL_SRP = 119;
const bit<8> IP_PROTOCOL_UTI = 120;
const bit<8> IP_PROTOCOL_SMP = 121;
const bit<8> IP_PROTOCOL_SM = 122;
const bit<8> IP_PROTOCOL_PTP = 123;
const bit<8> IP_PROTOCOL_ISISoIPV4 = 124;
const bit<8> IP_PROTOCOL_FIRE = 125;
const bit<8> IP_PROTOCOL_CRTP = 126;
const bit<8> IP_PROTOCOL_CRUDP = 127;
const bit<8> IP_PROTOCOL_SSCOPMCE = 128;
const bit<8> IP_PROTOCOL_IPLT = 129;
const bit<8> IP_PROTOCOL_SPS = 130;
const bit<8> IP_PROTOCOL_PIPE = 131;
const bit<8> IP_PROTOCOL_SCTP = 132;
const bit<8> IP_PROTOCOL_FC   = 133;
const bit<8> IP_PROTOCOL_RSVP_E2E_IGNORE = 134;
const bit<8> IP_PROTOCOL_MOBILITY_HEADER = 135;
const bit<8> IP_PROTOCOL_UDPLITE = 136;
const bit<8> IP_PROTOCOL_MPLS_IN_IP = 137;
const bit<8> IP_PROTOCOL_MANET = 138;
const bit<8> IP_PROTOCOL_HIP  = 139;
const bit<8> IP_PROTOCOL_SHIM6 = 140;
const bit<8> IP_PROTOCOL_WESP = 141;
const bit<8> IP_PROTOCOL_ROHC = 142;
const bit<8> IP_PROTOCOL_SRL2 = 143;
const bit<8> IP_PROTOCOL_NSH = 145;

#endif // _IP_PROTOCOL_P4_


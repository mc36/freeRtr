/*
 * Copyright 2019-present GÉANT RARE project
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
// https://www.iana.org/assignments/ieee-802-numbers/ieee-802-numbers.xhtml 

const bit<16> ETHERTYPE_IPV4              = 0x0800;
const bit<16> ETHERTYPE_ARP               = 0x0806;
const bit<16> ETHERTYPE_WOL               = 0x0842;
const bit<16> ETHERTYPE_TRILL             = 0x22f3;
const bit<16> ETHERTYPE_SRVP              = 0x22ea;
const bit<16> ETHERTYPE_DECNET            = 0x6003;
const bit<16> ETHERTYPE_ETHERNET          = 0x6558;
const bit<16> ETHERTYPE_RARP              = 0x8035;
const bit<16> ETHERTYPE_ETHERTALK         = 0x809b;
const bit<16> ETHERTYPE_AARP              = 0x80f3;
const bit<16> ETHERTYPE_VLAN              = 0x8100;
const bit<16> ETHERTYPE_IPX               = 0x8137;
const bit<16> ETHERTYPE_NOVELL            = 0x8138;
const bit<16> ETHERTYPE_QNX_QNET          = 0x8204;
const bit<16> ETHERTYPE_IPV6              = 0x86dd;
const bit<16> ETHERTYPE_ETHERNET_FC       = 0x8808;
const bit<16> ETHERTYPE_LACP              = 0x8809;
const bit<16> ETHERTYPE_COBRANET          = 0x8819;
const bit<16> ETHERTYPE_MPLS_UCAST        = 0x8847;
const bit<16> ETHERTYPE_MPLS_MCAST        = 0x8848;
const bit<16> ETHERTYPE_PPPOE_DISCOVERY   = 0x8863;
const bit<16> ETHERTYPE_PPPOE_SESSION     = 0x8864;
const bit<16> ETHERTYPE_INTEL_ADV_NET_SVC = 0x8864;
const bit<16> ETHERTYPE_JUMBO_FRAMES      = 0x8870;
const bit<16> ETHERTYPE_HOME_PLUG         = 0x887b;
const bit<16> ETHERTYPE_EAPOLAN           = 0x888e;
const bit<16> ETHERTYPE_PROFINET          = 0x8892;
const bit<16> ETHERTYPE_ETHERSOUND        = 0x8896;
const bit<16> ETHERTYPE_HYPERSCSI         = 0x889a;
const bit<16> ETHERTYPE_ATAOETHERNET      = 0x88a2;
const bit<16> ETHERTYPE_ETHERCAT          = 0x88a4;
const bit<16> ETHERTYPE_PBB_SPB           = 0x88a8;
const bit<16> ETHERTYPE_POWERLINK         = 0x88ab;
const bit<16> ETHERTYPE_GOOSE             = 0x88b8;
const bit<16> ETHERTYPE_GSE_MS            = 0x88b9;
const bit<16> ETHERTYPE_SVT               = 0x88ba;
const bit<16> ETHERTYPE_LLDP              = 0x88cc;
const bit<16> ETHERTYPE_SERCOS            = 0x88cd;
const bit<16> ETHERTYPE_WSMP              = 0x88dc;
const bit<16> ETHERTYPE_HOMEPLUG_AV_MME   = 0x88e1;
const bit<16> ETHERTYPE_MRP               = 0x88e3;
const bit<16> ETHERTYPE_MAC_SEC           = 0x88e5;
const bit<16> ETHERTYPE_PBB               = 0x88e7;
const bit<16> ETHERTYPE_PTP               = 0x88f7;
const bit<16> ETHERTYPE_NC_SI             = 0x88f8;
const bit<16> ETHERTYPE_PRP               = 0x88fb;
const bit<16> ETHERTYPE_CFM               = 0x8902;
const bit<16> ETHERTYPE_FCOE              = 0x8906;
const bit<16> ETHERTYPE_FCOE_INIT         = 0x8914;
const bit<16> ETHERTYPE_ROCE              = 0x8915;
const bit<16> ETHERTYPE_TTE               = 0x891d;
const bit<16> ETHERTYPE_VNTAG             = 0x8926;
const bit<16> ETHERTYPE_HSR               = 0x892f;
const bit<16> ETHERTYPE_NSH               = 0x894f;
const bit<16> ETHERTYPE_ETHERNET_CTP      = 0x9000;
const bit<16> ETHERTYPE_QINQ              = 0x9100;
const bit<16> ETHERTYPE_VERITAS_LLT       = 0xCAFE;



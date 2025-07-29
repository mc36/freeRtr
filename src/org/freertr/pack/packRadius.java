package org.freertr.pack;

import org.freertr.addr.addrEui;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.cry.cryHashMd5;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

/**
 * remote authentication dialin user (rfc2865) packet
 *
 * @author matecsaba
 */
public class packRadius {

    /**
     * create instance
     */
    public packRadius() {
    }

    /**
     * port number
     */
    public final static int port = 1812;

    /**
     * accounting port number
     */
    public final static int port2 = 1813;

    /**
     * size of header
     */
    public final static int size = 20;

    /**
     * shared secret
     */
    public String secret;

    /**
     * type of packet
     */
    public int code;

    /**
     * id number
     */
    public int idnt;

    /**
     * authenticator value
     */
    public byte[] auther;

    /**
     * access request
     */
    public final static int typeAccReq = 1;

    /**
     * access accept
     */
    public final static int typeAccAcc = 2;

    /**
     * access reject
     */
    public final static int typeAccRej = 3;

    /**
     * accounting request
     */
    public final static int typeAcoReq = 4;

    /**
     * accounting response
     */
    public final static int typeAcoRep = 5;

    /**
     * access challenge
     */
    public final static int typeAccChl = 11;

    /**
     * status server
     */
    public final static int typeSttSrv = 12;

    /**
     * status client
     */
    public final static int typeSttCln = 13;

    /**
     * user name
     */
    public final static int tlvUsrNam = 1;

    /**
     * user password
     */
    public final static int tlvUsrPwd = 2;

    /**
     * chap password
     */
    public final static int tlvChpPwd = 3;

    /**
     * nas ip address
     */
    public final static int tlvNasAdr4 = 4;

    /**
     * nas port
     */
    public final static int tlvNasPrt = 5;

    /**
     * service type
     */
    public final static int tlvSrvTyp = 6;

    /**
     * framed protocol
     */
    public final static int tlvFrmPrt = 7;

    /**
     * framed ip addresss
     */
    public final static int tlvFrmAdr4 = 8;

    /**
     * framed ip netmask
     */
    public final static int tlvFrmMsk4 = 9;

    /**
     * framed routing
     */
    public final static int tlvFrmRtn = 10;

    /**
     * filter id
     */
    public final static int tlvFilter = 11;

    /**
     * framed mtu
     */
    public final static int tlvFrmMtu = 12;

    /**
     * framed compression
     */
    public final static int tlvFrmCmp = 13;

    /**
     * login ip host
     */
    public final static int tlvLgnHst4 = 14;

    /**
     * login service
     */
    public final static int tlvLgnSvc = 15;

    /**
     * login tcp port
     */
    public final static int tlvLgnTcp = 16;

    /**
     * reply message
     */
    public final static int tlvReply = 18;

    /**
     * callback number
     */
    public final static int tlvCllNum = 19;

    /**
     * callback id
     */
    public final static int tlvCllId = 20;

    /**
     * framed route
     */
    public final static int tlvFrmRou4 = 22;

    /**
     * framed ipx network
     */
    public final static int tlvFrmIpx = 23;

    /**
     * state
     */
    public final static int tlvState = 24;

    /**
     * class
     */
    public final static int tlvClass = 25;

    /**
     * vendor specific
     */
    public final static int tlvVendor = 26;

    /**
     * session timeout
     */
    public final static int tlvSesTim = 27;

    /**
     * idle timeout
     */
    public final static int tlvIdlTim = 28;

    /**
     * termination action
     */
    public final static int tlvTrmAct = 29;

    /**
     * called station id
     */
    public final static int tlvCalled = 30;

    /**
     * calling station id
     */
    public final static int tlvCalling = 31;

    /**
     * nas identifier
     */
    public final static int tlvNasId = 32;

    /**
     * proxy state
     */
    public final static int tlvPrxSta = 33;

    /**
     * login lat service
     */
    public final static int tlvLatSrv = 34;

    /**
     * login lat mode
     */
    public final static int tlvLatMod = 35;

    /**
     * login lat group
     */
    public final static int tlvLatGrp = 36;

    /**
     * framed appletalk link
     */
    public final static int tlvFrmLnk = 37;

    /**
     * framed appletalk network
     */
    public final static int tlvFrmNet = 38;

    /**
     * framed appletalk zone
     */
    public final static int tlvFrmZon = 39;

    /**
     * account status
     */
    public final static int tlvActSta = 40;

    /**
     * account delay
     */
    public final static int tlvActDel = 41;

    /**
     * account input octets
     */
    public final static int tlvActInB = 42;

    /**
     * account output octets
     */
    public final static int tlvActOtB = 43;

    /**
     * account session id
     */
    public final static int tlvActSes = 44;

    /**
     * account authentic
     */
    public final static int tlvActAut = 45;

    /**
     * account session time
     */
    public final static int tlvActTim = 46;

    /**
     * account input packets
     */
    public final static int tlvActInP = 47;

    /**
     * account output packets
     */
    public final static int tlvActOtP = 48;

    /**
     * account terminate cause
     */
    public final static int tlvActTer = 49;

    /**
     * account multi session id
     */
    public final static int tlvActMul = 50;

    /**
     * account link count
     */
    public final static int tlvActLnk = 51;

    /**
     * account input gigawords
     */
    public final static int tlvActInG = 52;

    /**
     * account output gigawords
     */
    public final static int tlvActOtG = 53;

    /**
     * chap challenge
     */
    public final static int tlvChpChl = 60;

    /**
     * nas port type
     */
    public final static int tlvPrtTyp = 61;

    /**
     * port limit
     */
    public final static int tlvPrtLim = 62;

    /**
     * login lat port
     */
    public final static int tlvLatPrt = 63;

    /**
     * tunnel type
     */
    public final static int tlvTunTyp = 64;

    /**
     * tunnel medium
     */
    public final static int tlvTunMed = 65;

    /**
     * tunnel client
     */
    public final static int tlvTunCln = 66;

    /**
     * tunnel server
     */
    public final static int tlvTunSrv = 67;

    /**
     * acct tunnel connection
     */
    public final static int tlvTunAct = 68;

    /**
     * tunnel password
     */
    public final static int tlvTunPwd = 69;

    /**
     * arap password
     */
    public final static int tlvArpPwd = 70;

    /**
     * arap features
     */
    public final static int tlvArpFtr = 71;

    /**
     * arap zone access
     */
    public final static int tlvArpZon = 72;

    /**
     * arap security
     */
    public final static int tlvArpSec = 73;

    /**
     * arap security data
     */
    public final static int tlvArpDat = 74;

    /**
     * password retry
     */
    public final static int tlvPwdTry = 75;

    /**
     * prompt
     */
    public final static int tlvPrmpt = 76;

    /**
     * connect info
     */
    public final static int tlvConInf = 77;

    /**
     * configuration token
     */
    public final static int tlvCfgTkn = 78;

    /**
     * eap message
     */
    public final static int tlvEapMsg = 79;

    /**
     * message authenticator
     */
    public final static int tlvMsgAut = 80;

    /**
     * tunnel private group id
     */
    public final static int tlvTunPrv = 81;

    /**
     * tunnel assignment id
     */
    public final static int tlvTunAsg = 82;

    /**
     * tunnel preference
     */
    public final static int tlvTunPrf = 83;

    /**
     * arap challenge response
     */
    public final static int tlvArpChl = 84;

    /**
     * acct interim interval
     */
    public final static int tlvActInt = 85;

    /**
     * acct tunnel packets lost
     */
    public final static int tlvTunLst = 86;

    /**
     * nas port id
     */
    public final static int tlvNasPoi = 87;

    /**
     * framed pool
     */
    public final static int tlvFrmPol4 = 88;

    /**
     * cui
     */
    public final static int tlvCui = 89;

    /**
     * framed client auth id
     */
    public final static int tlvFrmCln = 90;

    /**
     * framed server auth id
     */
    public final static int tlvFrmSrv = 91;

    /**
     * nas filter rule
     */
    public final static int tlvNasFlt = 92;

    /**
     * originating line info
     */
    public final static int tlvOrgLin = 94;

    /**
     * nas ipv6 address
     */
    public final static int tlvNasAdr6 = 95;

    /**
     * framed interface id
     */
    public final static int tlvFrmIfi = 96;

    /**
     * framed ipv6 prefix
     */
    public final static int tlvFrmPrf = 97;

    /**
     * login ipv6 host
     */
    public final static int tlvLgnHst6 = 98;

    /**
     * framed ipv6 route
     */
    public final static int tlvFrmRou6 = 99;

    /**
     * framed ipv6 pool
     */
    public final static int tlvFrmPol6 = 100;

    /**
     * error cause
     */
    public final static int tlvErrCau = 101;

    /**
     * eap key name
     */
    public final static int tlvEapKey = 102;

    /**
     * digest response
     */
    public final static int tlvDgsRsp = 103;

    /**
     * digest realm
     */
    public final static int tlvDgsRlm = 104;

    /**
     * digest nonce
     */
    public final static int tlvDgsNon = 105;

    /**
     * digest response auth
     */
    public final static int tlvDgsAut = 106;

    /**
     * digest next nonce
     */
    public final static int tlvDgsNno = 107;

    /**
     * digest method
     */
    public final static int tlvDgsMet = 108;

    /**
     * digest uri
     */
    public final static int tlvDgsUri = 109;

    /**
     * digest qop
     */
    public final static int tlvDgsQop = 110;

    /**
     * digest algorithm
     */
    public final static int tlvDgsAlg = 111;

    /**
     * digest entity body hash
     */
    public final static int tlvDgsEnt = 112;

    /**
     * digest cnonce
     */
    public final static int tlvDgsCno = 113;

    /**
     * digest nonce count
     */
    public final static int tlvDgsNoc = 114;

    /**
     * digest username
     */
    public final static int tlvDgsUsr = 115;

    /**
     * digest opaque
     */
    public final static int tlvDgsOpq = 116;

    /**
     * digest auth param
     */
    public final static int tlvDgsPrm = 117;

    /**
     * digest aka auts
     */
    public final static int tlvDgsAka = 118;

    /**
     * digest domain
     */
    public final static int tlvDgsDom = 119;

    /**
     * digest stale
     */
    public final static int tlvDgsStl = 120;

    /**
     * digest ha1
     */
    public final static int tlvDgsHa1 = 121;

    /**
     * sip aor
     */
    public final static int tlvSipAor = 122;

    /**
     * delegated ipv6 prefix
     */
    public final static int tlvDelPfx = 123;

    /**
     * mip6 feature vector
     */
    public final static int tlvMipFtr = 124;

    /**
     * mip6 home link prefix
     */
    public final static int tlvMipHom = 125;

    /**
     * operator name
     */
    public final static int tlvOprNam = 126;

    /**
     * location information
     */
    public final static int tlvLocInf = 127;

    /**
     * location data
     */
    public final static int tlvLocDat = 128;

    /**
     * basic location policy rules
     */
    public final static int tlvLocBas = 129;

    /**
     * extended location policy rules
     */
    public final static int tlvLocExt = 130;

    /**
     * location capable
     */
    public final static int tlvLocCap = 131;

    /**
     * requested location info
     */
    public final static int tlvLocReq = 132;

    /**
     * framed management protocol
     */
    public final static int tlvFrmMgt = 133;

    /**
     * management transport protection
     */
    public final static int tlvMgtPrt = 134;

    /**
     * management policy id
     */
    public final static int tlvMgtPol = 135;

    /**
     * management privilege level
     */
    public final static int tlvMgtPrv = 136;

    /**
     * pkm ss cert
     */
    public final static int tlvPkmSsc = 137;

    /**
     * pkm ca cert
     */
    public final static int tlvPkmCac = 138;

    /**
     * pkm config settings
     */
    public final static int tlvPkmCfg = 139;

    /**
     * pkm cryptosuite list
     */
    public final static int tlvPkmCrp = 140;

    /**
     * pkm said
     */
    public final static int tlvPkmSai = 141;

    /**
     * pkm sa descriptor
     */
    public final static int tlvPkmSad = 142;

    /**
     * pkm auth key
     */
    public final static int tlvPkmKey = 143;

    /**
     * ds lite tunnel name
     */
    public final static int tlvDslTun = 144;

    /**
     * mobile node identifier
     */
    public final static int tlvMobNod = 145;

    /**
     * service selection
     */
    public final static int tlvSrvSel = 146;

    /**
     * pmip6 home lma ipv6 address
     */
    public final static int tlvPmiHom6 = 147;

    /**
     * pmip6 visited lma ipv6 address
     */
    public final static int tlvPmiVis6 = 148;

    /**
     * pmip6 home lma ipv4 address
     */
    public final static int tlvPmiHom4 = 149;

    /**
     * pmip6 visited lma ipv4 address
     */
    public final static int tlvPmiVis4 = 150;

    /**
     * pmip6 home hn prefix
     */
    public final static int tlvPmiHomH = 151;

    /**
     * pmip6 visited hn prefix
     */
    public final static int tlvPmiVisH = 152;

    /**
     * pmip6 home interface id
     */
    public final static int tlvPmiHomI = 153;

    /**
     * pmip6 visited interface id
     */
    public final static int tlvPmiVisI = 154;

    /**
     * pmip6 home ipv4 hoa
     */
    public final static int tlvPmiHomO = 155;

    /**
     * pmip6 visited ipv4 hoa
     */
    public final static int tlvPmiVisO = 156;

    /**
     * pmip6 home dhcp4 server address
     */
    public final static int tlvPmiHomD4 = 157;

    /**
     * pmip6 visited dhcp4 server address
     */
    public final static int tlvPmiVisD4 = 158;

    /**
     * pmip6 home dhcp6 server address
     */
    public final static int tlvPmiHomD6 = 159;

    /**
     * pmip6 visited dhcp6 server address
     */
    public final static int tlvPmiVisD6 = 160;

    /**
     * pmip6 home ipv4 gateway
     */
    public final static int tlvPmiHomG = 161;

    /**
     * pmip6 visited ipv4 gateway
     */
    public final static int tlvPmiVisG = 162;

    /**
     * eap lower layer
     */
    public final static int tlvEapLay = 163;

    /**
     * gss acceptor service name
     */
    public final static int tlvGssNam = 164;

    /**
     * gss acceptor host name
     */
    public final static int tlvGssHst = 165;

    /**
     * gss acceptor service specifics
     */
    public final static int tlvGssSpc = 166;

    /**
     * gss acceptor realm name
     */
    public final static int tlvGssRlm = 167;

    /**
     * framed ipv6 address
     */
    public final static int tlvFrmAdr6 = 168;

    /**
     * dns server ipv6 address
     */
    public final static int tlvDnsSrv6 = 169;

    /**
     * route ipv6 information
     */
    public final static int tlvRouInf6 = 170;

    /**
     * delegated ipv6 prefix pool
     */
    public final static int tlvDelPol6 = 171;

    /**
     * stateful ipv6 address pool
     */
    public final static int tlvStaPol6 = 172;

    /**
     * ipv6 6rd configuration
     */
    public final static int tlv6rdCfg = 173;

    /**
     * allowed called station id
     */
    public final static int tlvAlwSta = 174;

    /**
     * eap peer id
     */
    public final static int tlvEapPer = 175;

    /**
     * eap server id
     */
    public final static int tlvEapSrv = 176;

    /**
     * mobility domain id
     */
    public final static int tlvMobDom = 177;

    /**
     * preauth timeout
     */
    public final static int tlvPreTim = 178;

    /**
     * network id name
     */
    public final static int tlvNetName = 179;

    /**
     * eapol announcement
     */
    public final static int tlvEapAno = 180;

    /**
     * wlan hessid
     */
    public final static int tlvWlaHes = 181;

    /**
     * wlan venue info
     */
    public final static int tlvWlaVen = 182;

    /**
     * wlan venue language
     */
    public final static int tlvWlaLng = 183;

    /**
     * wlan venue name
     */
    public final static int tlvWlaNam = 184;

    /**
     * wlan reason code
     */
    public final static int tlvWlaRea = 185;

    /**
     * wlan pairwise cipher
     */
    public final static int tlvWlaPai = 186;

    /**
     * wlan group cipher
     */
    public final static int tlvWlaGrp = 187;

    /**
     * wlan akm suite
     */
    public final static int tlvWlaAkm = 188;

    /**
     * wlan group management cipher
     */
    public final static int tlvWlaMgt = 189;

    /**
     * wlan rf band
     */
    public final static int tlvWlaBnd = 190;

    /**
     * user name
     */
    public String valUsrNam = null;

    /**
     * user password
     */
    public String valUsrPwd = null;

    /**
     * chap id
     */
    public int valChpIdn = -1;

    /**
     * chap password
     */
    public byte[] valChpPwd = null;

    /**
     * nas ip address
     */
    public addrIPv4 valNasAdr4 = null;

    /**
     * nas port
     */
    public int valNasPrt = -1;

    /**
     * service type
     */
    public int valSrvTyp = -1;

    /**
     * framed protocol
     */
    public int valFrmPrt = -1;

    /**
     * framed ip addresss
     */
    public addrIPv4 valFrmAdr4 = null;

    /**
     * framed ip netmask
     */
    public addrIPv4 valFrmMsk4 = null;

    /**
     * framed routing
     */
    public int valFrmRtn = -1;

    /**
     * filter id
     */
    public String valFilter = null;

    /**
     * framed mtu
     */
    public int valFrmMtu = -1;

    /**
     * framed compression
     */
    public int valFrmCmp = -1;

    /**
     * login ip host
     */
    public addrIPv4 valLgnHst4 = null;

    /**
     * login service
     */
    public int valLgnSvc = -1;

    /**
     * login tcp port
     */
    public int valLgnTcp = -1;

    /**
     * reply message
     */
    public String valReply = null;

    /**
     * callback number
     */
    public String valCllNum = null;

    /**
     * callback id
     */
    public String valCllId = null;

    /**
     * framed route
     */
    public String valFrmRou4 = null;

    /**
     * framed ipx network
     */
    public int valFrmIpx = -1;

    /**
     * state
     */
    public String valState = null;

    /**
     * class
     */
    public String valClass = null;

    /**
     * session timeout
     */
    public int valSesTim = -1;

    /**
     * idle timeout
     */
    public int valIdlTim = -1;

    /**
     * termination action
     */
    public int valTrmAct = -1;

    /**
     * called station id
     */
    public String valCalled = null;

    /**
     * calling station id
     */
    public String valCalling = null;

    /**
     * nas identifier
     */
    public String valNasId = null;

    /**
     * proxy state
     */
    public String valPrxSta = null;

    /**
     * login lat service
     */
    public String valLatSrv = null;

    /**
     * login lat mode
     */
    public String valLatMod = null;

    /**
     * login lat group
     */
    public String valLatGrp = null;

    /**
     * framed appletalk link
     */
    public int valFrmLnk = -1;

    /**
     * framed appletalk network
     */
    public int valFrmNet = -1;

    /**
     * framed appletalk zone
     */
    public String valFrmZon = null;

    /**
     * account status
     */
    public int valActSta = -1;

    /**
     * account delay
     */
    public int valActDel = -1;

    /**
     * account input octets
     */
    public long valActInB = -1;

    /**
     * account output octets
     */
    public long valActOtB = -1;

    /**
     * account session id
     */
    public String valActSes = null;

    /**
     * account authentic
     */
    public int valActAut = -1;

    /**
     * account session time
     */
    public int valActTim = -1;

    /**
     * account input packets
     */
    public int valActInP = -1;

    /**
     * account output packets
     */
    public int valActOtP = -1;

    /**
     * account terminate cause
     */
    public int valActTer = -1;

    /**
     * account multi session id
     */
    public String valActMul = null;

    /**
     * account link count
     */
    public int valActLnk = -1;

    /**
     * chap challenge
     */
    public byte[] valChpChl = null;

    /**
     * nas port type
     */
    public int valPrtTyp = -1;

    /**
     * port limit
     */
    public int valPrtLim = -1;

    /**
     * login lat port
     */
    public String valLatPrt = null;

    /**
     * tunnel type
     */
    public int valTunTyp = -1;

    /**
     * tunnel medium
     */
    public int valTunMed = -1;

    /**
     * tunnel client
     */
    public String valTunCln = null;

    /**
     * tunnel server
     */
    public String valTunSrv = null;

    /**
     * acct tunnel connection
     */
    public String valTunAct = null;

    /**
     * tunnel password
     */
    public String valTunPwd = null;

    /**
     * arap password
     */
    public String valArpPwd = null;

    /**
     * arap features
     */
    public String valArpFtr = null;

    /**
     * arap zone access
     */
    public int valArpZon = -1;

    /**
     * arap security
     */
    public int valArpSec = -1;

    /**
     * arap security data
     */
    public String valArpDat = null;

    /**
     * password retry
     */
    public int valPwdTry = -1;

    /**
     * prompt
     */
    public int valPrmpt = -1;

    /**
     * connect info
     */
    public String valConInf = null;

    /**
     * configuration token
     */
    public String valCfgTkn = null;

    /**
     * eap message
     */
    public String valEapMsg = null;

    /**
     * message authenticator
     */
    public String valMsgAut = null;

    /**
     * tunnel private group id
     */
    public String valTunPrv = null;

    /**
     * tunnel assignment id
     */
    public String valTunAsg = null;

    /**
     * tunnel preference
     */
    public int valTunPrf = -1;

    /**
     * arap challenge response
     */
    public String valArpChl = null;

    /**
     * acct interim interval
     */
    public int valActInt = -1;

    /**
     * acct tunnel packets lost
     */
    public int valTunLst = -1;

    /**
     * nas port id
     */
    public String valNasPoi = null;

    /**
     * framed pool
     */
    public String valFrmPol4 = null;

    /**
     * cui
     */
    public String valCui = null;

    /**
     * framed client auth id
     */
    public String valFrmCln = null;

    /**
     * framed server auth id
     */
    public String valFrmSrv = null;

    /**
     * nas filter rule
     */
    public String valNasFlt = null;

    /**
     * originating line info
     */
    public String valOrgLin = null;

    /**
     * nas ipv6 address
     */
    public addrIPv6 valNasAdr6 = null;

    /**
     * framed interface id
     */
    public addrEui valFrmIfi = null;

    /**
     * framed ipv6 prefix
     */
    public addrPrefix<addrIPv6> valFrmPrf = null;

    /**
     * login ipv6 host
     */
    public addrIPv6 valLgnHst6 = null;

    /**
     * framed ipv6 route
     */
    public String valFrmRou6 = null;

    /**
     * framed ipv6 pool
     */
    public String valFrmPol6 = null;

    /**
     * error cause
     */
    public int valErrCau = -1;

    /**
     * eap key name
     */
    public String valEapKey = null;

    /**
     * digest response
     */
    public String valDgsRsp = null;

    /**
     * digest realm
     */
    public String valDgsRlm = null;

    /**
     * digest nonce
     */
    public String valDgsNon = null;

    /**
     * digest response auth
     */
    public String valDgsAut = null;

    /**
     * digest next nonce
     */
    public String valDgsNno = null;

    /**
     * digest method
     */
    public String valDgsMet = null;

    /**
     * digest uri
     */
    public String valDgsUri = null;

    /**
     * digest qop
     */
    public String valDgsQop = null;

    /**
     * digest algorithm
     */
    public String valDgsAlg = null;

    /**
     * digest entity body hash
     */
    public String valDgsEnt = null;

    /**
     * digest cnonce
     */
    public String valDgsCno = null;

    /**
     * digest nonce count
     */
    public String valDgsNoc = null;

    /**
     * digest username
     */
    public String valDgsUsr = null;

    /**
     * digest opaque
     */
    public String valDgsOpq = null;

    /**
     * digest auth param
     */
    public String valDgsPrm = null;

    /**
     * digest aka auts
     */
    public String valDgsAka = null;

    /**
     * digest domain
     */
    public String valDgsDom = null;

    /**
     * digest stale
     */
    public String valDgsStl = null;

    /**
     * digest ha1
     */
    public String valDgsHa1 = null;

    /**
     * sip aor
     */
    public String valSipAor = null;

    /**
     * delegated ipv6 prefix
     */
    public addrPrefix<addrIPv6> valDelPfx = null;

    /**
     * mip6 feature vector
     */
    public int valMipFtr = -1;

    /**
     * mip6 home link prefix
     */
    public String valMipHom = null;

    /**
     * operator name
     */
    public String valOprNam = null;

    /**
     * location information
     */
    public String valLocInf = null;

    /**
     * location data
     */
    public String valLocDat = null;

    /**
     * basic location policy rules
     */
    public String valLocBas = null;

    /**
     * extended location policy rules
     */
    public String valLocExt = null;

    /**
     * location capable
     */
    public int valLocCap = -1;

    /**
     * requested location info
     */
    public int valLocReq = -1;

    /**
     * framed management protocol
     */
    public int valFrmMgt = -1;

    /**
     * management transport protection
     */
    public int valMgtPrt = -1;

    /**
     * management policy id
     */
    public String valMgtPol = null;

    /**
     * management privilege level
     */
    public int valMgtPrv = -1;

    /**
     * pkm ss cert
     */
    public String valPkmSsc = null;

    /**
     * pkm ca cert
     */
    public String valPkmCac = null;

    /**
     * pkm config settings
     */
    public String valPkmCfg = null;

    /**
     * pkm cryptosuite list
     */
    public String valPkmCrp = null;

    /**
     * pkm said
     */
    public String valPkmSai = null;

    /**
     * pkm sa descriptor
     */
    public String valPkmSad = null;

    /**
     * pkm auth key
     */
    public String valPkmKey = null;

    /**
     * ds lite tunnel name
     */
    public String valDslTun = null;

    /**
     * mobile node identifier
     */
    public String valMobNod = null;

    /**
     * service selection
     */
    public String valSrvSel = null;

    /**
     * pmip6 home lma ipv6 address
     */
    public addrIPv6 valPmiHom6 = null;

    /**
     * pmip6 visited lma ipv6 address
     */
    public addrIPv6 valPmiVis6 = null;

    /**
     * pmip6 home lma ipv4 address
     */
    public addrIPv4 valPmiHom4 = null;

    /**
     * pmip6 visited lma ipv4 address
     */
    public addrIPv4 valPmiVis4 = null;

    /**
     * pmip6 home hn prefix
     */
    public addrPrefix<addrIPv6> valPmiHomH = null;

    /**
     * pmip6 visited hn prefix
     */
    public addrPrefix<addrIPv6> valPmiVisH = null;

    /**
     * pmip6 home interface id
     */
    public addrEui valPmiHomI = null;

    /**
     * pmip6 visited interface id
     */
    public addrEui valPmiVisI = null;

    /**
     * pmip6 home ipv4 hoa
     */
    public addrPrefix<addrIPv4> valPmiHomO = null;

    /**
     * pmip6 visited ipv4 hoa
     */
    public addrPrefix<addrIPv4> valPmiVisO = null;

    /**
     * pmip6 home dhcp4 server address
     */
    public addrIPv4 valPmiHomD4 = null;

    /**
     * pmip6 visited dhcp4 server address
     */
    public addrIPv4 valPmiVisD4 = null;

    /**
     * pmip6 home dhcp6 server address
     */
    public addrIPv6 valPmiHomD6 = null;

    /**
     * pmip6 visited dhcp6 server address
     */
    public addrIPv6 valPmiVisD6 = null;

    /**
     * pmip6 home ipv4 gateway
     */
    public addrIPv4 valPmiHomG = null;

    /**
     * pmip6 visited ipv4 gateway
     */
    public addrIPv4 valPmiVisG = null;

    /**
     * eap lower layer
     */
    public int valEapLay = -1;

    /**
     * gss acceptor service name
     */
    public String valGssNam = null;

    /**
     * gss acceptor host name
     */
    public String valGssHst = null;

    /**
     * gss acceptor service specifics
     */
    public String valGssSpc = null;

    /**
     * gss acceptor realm name
     */
    public String valGssRlm = null;

    /**
     * framed ipv6 address
     */
    public addrIPv6 valFrmAdr6 = null;

    /**
     * dns server ipv6 address
     */
    public addrIPv6 valDnsSrv6 = null;

    /**
     * route ipv6 information
     */
    public addrPrefix<addrIPv6> valRouInf6 = null;

    /**
     * delegated ipv6 prefix pool
     */
    public String valDelPol6 = null;

    /**
     * stateful ipv6 address pool
     */
    public String valStaPol6 = null;

    /**
     * ipv6 6rd configuration
     */
    public String val6rdCfg = null;

    /**
     * allowed called station id
     */
    public String valAlwSta = null;

    /**
     * eap peer id
     */
    public String valEapPer = null;

    /**
     * eap server id
     */
    public String valEapSrv = null;

    /**
     * mobility domain id
     */
    public int valMobDom = -1;

    /**
     * preauth timeout
     */
    public int valPreTim = -1;

    /**
     * network id name
     */
    public String valNetName = null;

    /**
     * eapol announcement
     */
    public String valEapAno = null;

    /**
     * wlan hessid
     */
    public String valWlaHes = null;

    /**
     * wlan venue info
     */
    public int valWlaVen = -1;

    /**
     * wlan venue language
     */
    public String valWlaLng = null;

    /**
     * wlan venue name
     */
    public String valWlaNam = null;

    /**
     * wlan reason code
     */
    public int valWlaRea = -1;

    /**
     * wlan pairwise cipher
     */
    public int valWlaPai = -1;

    /**
     * wlan group cipher
     */
    public int valWlaGrp = -1;

    /**
     * wlan akm suite
     */
    public int valWlaAkm = -1;

    /**
     * wlan group management cipher
     */
    public int valWlaMgt = -1;

    /**
     * wlan rf band
     */
    public int valWlaBnd = -1;

    private encTlv tlv = new encTlv(0, 8, 8, 8, 1, 2, 2, 1, 0, 512, true);

    /**
     * convert code to string
     *
     * @param i code to convert
     * @return string
     */
    public String code2string(int i) {
        switch (i) {
            case typeAccReq:
                return "access request";
            case typeAccAcc:
                return "access accept";
            case typeAccRej:
                return "access reject";
            case typeAcoReq:
                return "accounting request";
            case typeAcoRep:
                return "accounting response";
            case typeAccChl:
                return "access challenge";
            case typeSttSrv:
                return "status server";
            case typeSttCln:
                return "status client";
            default:
                return "unknown=" + i;
        }
    }

    private <T extends addrType> addrPrefix<T> getPref(T adr) {
        int len = tlv.valDat[1];
        adr.fromBuf(tlv.valDat, 2);
        return new addrPrefix<T>(adr, len);
    }

    private addrType getAddr(addrType adr) {
        if (adr.getSize() != tlv.valSiz) {
            return null;
        }
        adr.fromBuf(tlv.valDat, 0);
        return adr;
    }

    private int getInt() {
        return bits.msbGetD(tlv.valDat, 0);
    }

    private long getLint(long old) {
        old >>>= 32;
        if (old == 0xffffffff) {
            old = 0;
        }
        old <<= 32;
        old |= bits.msbGetD(tlv.valDat, 0);
        return old;
    }

    private long getHint(long old) {
        old &= 0xffffffff;
        if (old == 0xffffffff) {
            old = 0;
        }
        old |= (long) bits.msbGetD(tlv.valDat, 0) << 32;
        return old;
    }

    private String getStr() {
        return tlv.getStr();
    }

    /**
     * parse packet
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parsePacket(packHolder pck) {
        code = pck.getByte(0);
        idnt = pck.getByte(1);
        int i = pck.msbGetW(2);
        if (i > pck.dataSize()) {
            return true;
        }
        pck.setBytesLeft(i);
        auther = new byte[16];
        pck.getCopy(auther, 0, 4, auther.length);
        pck.getSkip(size);
        int siz = pck.dataSize();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case tlvUsrNam:
                    valUsrNam = tlv.getStr();
                    break;
                case tlvUsrPwd:
                    valUsrPwd = passwdDecrypt(secret, auther, tlv.copyBytes());
                    break;
                case tlvChpPwd:
                    valChpIdn = tlv.valDat[0];
                    valChpPwd = new byte[tlv.valSiz - 1];
                    bits.byteCopy(tlv.valDat, 1, valChpPwd, 0, valChpPwd.length);
                    break;
                case tlvNasAdr4:
                    valNasAdr4 = (addrIPv4) getAddr(new addrIPv4());
                    break;
                case tlvNasPrt:
                    valNasPrt = getInt();
                    break;
                case tlvSrvTyp:
                    valSrvTyp = getInt();
                    break;
                case tlvFrmPrt:
                    valFrmPrt = getInt();
                    break;
                case tlvFrmAdr4:
                    valFrmAdr4 = (addrIPv4) getAddr(new addrIPv4());
                    break;
                case tlvFrmMsk4:
                    valFrmMsk4 = (addrIPv4) getAddr(new addrIPv4());
                    break;
                case tlvFrmRtn:
                    valFrmRtn = getInt();
                    break;
                case tlvFilter:
                    valFilter = getStr();
                    break;
                case tlvFrmMtu:
                    valFrmMtu = getInt();
                    break;
                case tlvFrmCmp:
                    valFrmCmp = getInt();
                    break;
                case tlvLgnHst4:
                    valLgnHst4 = (addrIPv4) getAddr(new addrIPv4());
                    break;
                case tlvLgnSvc:
                    valLgnSvc = getInt();
                    break;
                case tlvLgnTcp:
                    valLgnTcp = getInt();
                    break;
                case tlvReply:
                    valReply = getStr();
                    break;
                case tlvCllNum:
                    valCllNum = getStr();
                    break;
                case tlvCllId:
                    valCllId = getStr();
                    break;
                case tlvFrmRou4:
                    valFrmRou4 = getStr();
                    break;
                case tlvFrmIpx:
                    valFrmIpx = getInt();
                    break;
                case tlvState:
                    valState = getStr();
                    break;
                case tlvClass:
                    valClass = getStr();
                    break;
                case tlvSesTim:
                    valSesTim = getInt();
                    break;
                case tlvIdlTim:
                    valIdlTim = getInt();
                    break;
                case tlvTrmAct:
                    valTrmAct = getInt();
                    break;
                case tlvCalled:
                    valCalled = getStr();
                    break;
                case tlvCalling:
                    valCalling = getStr();
                    break;
                case tlvNasId:
                    valNasId = getStr();
                    break;
                case tlvPrxSta:
                    valPrxSta = getStr();
                    break;
                case tlvLatSrv:
                    valLatSrv = getStr();
                    break;
                case tlvLatMod:
                    valLatMod = getStr();
                    break;
                case tlvLatGrp:
                    valLatGrp = getStr();
                    break;
                case tlvFrmLnk:
                    valFrmLnk = getInt();
                    break;
                case tlvFrmNet:
                    valFrmNet = getInt();
                    break;
                case tlvFrmZon:
                    valFrmZon = getStr();
                    break;
                case tlvActSta:
                    valActSta = getInt();
                    break;
                case tlvActDel:
                    valActDel = getInt();
                    break;
                case tlvActInB:
                    valActInB = getLint(valActInB);
                    break;
                case tlvActInG:
                    valActInB = getHint(valActInB);
                    break;
                case tlvActOtB:
                    valActOtB = getLint(valActOtB);
                    break;
                case tlvActOtG:
                    valActOtB = getHint(valActOtB);
                    break;
                case tlvActSes:
                    valActSes = getStr();
                    break;
                case tlvActAut:
                    valActAut = getInt();
                    break;
                case tlvActTim:
                    valActTim = getInt();
                    break;
                case tlvActInP:
                    valActInP = getInt();
                    break;
                case tlvActOtP:
                    valActOtP = getInt();
                    break;
                case tlvActTer:
                    valActTer = getInt();
                    break;
                case tlvActMul:
                    valActMul = getStr();
                    break;
                case tlvActLnk:
                    valActLnk = getInt();
                    break;
                case tlvChpChl:
                    valChpChl = tlv.copyBytes();
                    break;
                case tlvPrtTyp:
                    valPrtTyp = getInt();
                    break;
                case tlvPrtLim:
                    valPrtLim = getInt();
                    break;
                case tlvLatPrt:
                    valLatPrt = getStr();
                    break;
                case tlvTunTyp:
                    valTunTyp = getInt();
                    break;
                case tlvTunMed:
                    valTunMed = getInt();
                    break;
                case tlvTunCln:
                    valTunCln = getStr();
                    break;
                case tlvTunSrv:
                    valTunSrv = getStr();
                    break;
                case tlvTunAct:
                    valTunAct = getStr();
                    break;
                case tlvTunPwd:
                    valTunPwd = getStr();
                    break;
                case tlvArpPwd:
                    valArpPwd = getStr();
                    break;
                case tlvArpFtr:
                    valArpFtr = getStr();
                    break;
                case tlvArpZon:
                    valArpZon = getInt();
                    break;
                case tlvArpSec:
                    valArpSec = getInt();
                    break;
                case tlvArpDat:
                    valArpDat = getStr();
                    break;
                case tlvPwdTry:
                    valPwdTry = getInt();
                    break;
                case tlvPrmpt:
                    valPrmpt = getInt();
                    break;
                case tlvConInf:
                    valConInf = getStr();
                    break;
                case tlvCfgTkn:
                    valCfgTkn = getStr();
                    break;
                case tlvEapMsg:
                    valEapMsg = getStr();
                    break;
                case tlvMsgAut:
                    valMsgAut = getStr();
                    break;
                case tlvTunPrv:
                    valTunPrv = getStr();
                    break;
                case tlvTunAsg:
                    valTunAsg = getStr();
                    break;
                case tlvTunPrf:
                    valTunPrf = getInt();
                    break;
                case tlvArpChl:
                    valArpChl = getStr();
                    break;
                case tlvActInt:
                    valActInt = getInt();
                    break;
                case tlvTunLst:
                    valTunLst = getInt();
                    break;
                case tlvNasPoi:
                    valNasPoi = getStr();
                    break;
                case tlvFrmPol4:
                    valFrmPol4 = getStr();
                    break;
                case tlvCui:
                    valCui = getStr();
                    break;
                case tlvFrmCln:
                    valFrmCln = getStr();
                    break;
                case tlvFrmSrv:
                    valFrmSrv = getStr();
                    break;
                case tlvNasFlt:
                    valNasFlt = getStr();
                    break;
                case tlvOrgLin:
                    valOrgLin = getStr();
                    break;
                case tlvFrmIfi:
                    valFrmIfi = (addrEui) getAddr(new addrEui());
                    break;
                case tlvFrmPrf:
                    valFrmPrf = getPref(new addrIPv6());
                    break;
                case tlvLgnHst6:
                    valLgnHst6 = (addrIPv6) getAddr(new addrIPv6());
                    break;
                case tlvFrmRou6:
                    valFrmRou6 = getStr();
                    break;
                case tlvFrmPol6:
                    valFrmPol6 = getStr();
                    break;
                case tlvErrCau:
                    valErrCau = getInt();
                    break;
                case tlvEapKey:
                    valEapKey = getStr();
                    break;
                case tlvDgsRsp:
                    valDgsRsp = getStr();
                    break;
                case tlvDgsRlm:
                    valDgsRlm = getStr();
                    break;
                case tlvDgsNon:
                    valDgsNon = getStr();
                    break;
                case tlvDgsAut:
                    valDgsAut = getStr();
                    break;
                case tlvDgsNno:
                    valDgsNno = getStr();
                    break;
                case tlvDgsMet:
                    valDgsMet = getStr();
                    break;
                case tlvDgsUri:
                    valDgsUri = getStr();
                    break;
                case tlvDgsQop:
                    valDgsQop = getStr();
                    break;
                case tlvDgsAlg:
                    valDgsAlg = getStr();
                    break;
                case tlvDgsEnt:
                    valDgsEnt = getStr();
                    break;
                case tlvDgsCno:
                    valDgsCno = getStr();
                    break;
                case tlvDgsNoc:
                    valDgsNoc = getStr();
                    break;
                case tlvDgsUsr:
                    valDgsUsr = getStr();
                    break;
                case tlvDgsOpq:
                    valDgsOpq = getStr();
                    break;
                case tlvDgsPrm:
                    valDgsPrm = getStr();
                    break;
                case tlvDgsAka:
                    valDgsAka = getStr();
                    break;
                case tlvDgsDom:
                    valDgsDom = getStr();
                    break;
                case tlvDgsStl:
                    valDgsStl = getStr();
                    break;
                case tlvDgsHa1:
                    valDgsHa1 = getStr();
                    break;
                case tlvSipAor:
                    valSipAor = getStr();
                    break;
                case tlvDelPfx:
                    valDelPfx = getPref(new addrIPv6());
                    break;
                case tlvMipFtr:
                    valMipFtr = getInt();
                    break;
                case tlvMipHom:
                    valMipHom = getStr();
                    break;
                case tlvOprNam:
                    valOprNam = getStr();
                    break;
                case tlvLocInf:
                    valLocInf = getStr();
                    break;
                case tlvLocDat:
                    valLocDat = getStr();
                    break;
                case tlvLocBas:
                    valLocBas = getStr();
                    break;
                case tlvLocExt:
                    valLocExt = getStr();
                    break;
                case tlvLocCap:
                    valLocCap = getInt();
                    break;
                case tlvLocReq:
                    valLocReq = getInt();
                    break;
                case tlvFrmMgt:
                    valFrmMgt = getInt();
                    break;
                case tlvMgtPrt:
                    valMgtPrt = getInt();
                    break;
                case tlvMgtPol:
                    valMgtPol = getStr();
                    break;
                case tlvMgtPrv:
                    valMgtPrv = getInt();
                    break;
                case tlvPkmSsc:
                    valPkmSsc = getStr();
                    break;
                case tlvPkmCac:
                    valPkmCac = getStr();
                    break;
                case tlvPkmCfg:
                    valPkmCfg = getStr();
                    break;
                case tlvPkmCrp:
                    valPkmCrp = getStr();
                    break;
                case tlvPkmSai:
                    valPkmSai = getStr();
                    break;
                case tlvPkmSad:
                    valPkmSad = getStr();
                    break;
                case tlvPkmKey:
                    valPkmKey = getStr();
                    break;
                case tlvDslTun:
                    valDslTun = getStr();
                    break;
                case tlvMobNod:
                    valMobNod = getStr();
                    break;
                case tlvSrvSel:
                    valSrvSel = getStr();
                    break;
                case tlvPmiHom6:
                    valPmiHom6 = (addrIPv6) getAddr(new addrIPv6());
                    break;
                case tlvPmiVis6:
                    valPmiVis6 = (addrIPv6) getAddr(new addrIPv6());
                    break;
                case tlvPmiHom4:
                    valPmiHom4 = (addrIPv4) getAddr(new addrIPv4());
                    break;
                case tlvPmiVis4:
                    valPmiVis4 = (addrIPv4) getAddr(new addrIPv4());
                    break;
                case tlvPmiHomH:
                    valPmiHomH = getPref(new addrIPv6());
                    break;
                case tlvPmiVisH:
                    valPmiVisH = getPref(new addrIPv6());
                    break;
                case tlvPmiHomI:
                    valPmiHomI = (addrEui) getAddr(new addrEui());
                    break;
                case tlvPmiVisI:
                    valPmiVisI = (addrEui) getAddr(new addrEui());
                    break;
                case tlvPmiHomO:
                    valPmiHomO = getPref(new addrIPv4());
                    break;
                case tlvPmiVisO:
                    valPmiVisO = getPref(new addrIPv4());
                    break;
                case tlvPmiHomD4:
                    valPmiHomD4 = (addrIPv4) getAddr(new addrIPv4());
                    break;
                case tlvPmiVisD4:
                    valPmiVisD4 = (addrIPv4) getAddr(new addrIPv4());
                    break;
                case tlvPmiHomD6:
                    valPmiHomD6 = (addrIPv6) getAddr(new addrIPv6());
                    break;
                case tlvPmiVisD6:
                    valPmiVisD6 = (addrIPv6) getAddr(new addrIPv6());
                    break;
                case tlvPmiHomG:
                    valPmiHomG = (addrIPv4) getAddr(new addrIPv4());
                    break;
                case tlvPmiVisG:
                    valPmiVisG = (addrIPv4) getAddr(new addrIPv4());
                    break;
                case tlvEapLay:
                    valEapLay = getInt();
                    break;
                case tlvGssNam:
                    valGssNam = getStr();
                    break;
                case tlvGssHst:
                    valGssHst = getStr();
                    break;
                case tlvGssSpc:
                    valGssSpc = getStr();
                    break;
                case tlvGssRlm:
                    valGssRlm = getStr();
                    break;
                case tlvFrmAdr6:
                    valFrmAdr6 = (addrIPv6) getAddr(new addrIPv6());
                    break;
                case tlvDnsSrv6:
                    valDnsSrv6 = (addrIPv6) getAddr(new addrIPv6());
                    break;
                case tlvRouInf6:
                    valRouInf6 = getPref(new addrIPv6());
                    break;
                case tlvDelPol6:
                    valDelPol6 = getStr();
                    break;
                case tlvStaPol6:
                    valStaPol6 = getStr();
                    break;
                case tlv6rdCfg:
                    val6rdCfg = getStr();
                    break;
                case tlvAlwSta:
                    valAlwSta = getStr();
                    break;
                case tlvEapPer:
                    valEapPer = getStr();
                    break;
                case tlvEapSrv:
                    valEapSrv = getStr();
                    break;
                case tlvMobDom:
                    valMobDom = getInt();
                    break;
                case tlvPreTim:
                    valPreTim = getInt();
                    break;
                case tlvNetName:
                    valNetName = getStr();
                    break;
                case tlvEapAno:
                    valEapAno = getStr();
                    break;
                case tlvWlaHes:
                    valWlaHes = getStr();
                    break;
                case tlvWlaVen:
                    valWlaVen = getInt();
                    break;
                case tlvWlaLng:
                    valWlaLng = getStr();
                    break;
                case tlvWlaNam:
                    valWlaNam = getStr();
                    break;
                case tlvWlaRea:
                    valWlaRea = getInt();
                    break;
                case tlvWlaPai:
                    valWlaPai = getInt();
                    break;
                case tlvWlaGrp:
                    valWlaGrp = getInt();
                    break;
                case tlvWlaAkm:
                    valWlaAkm = getInt();
                    break;
                case tlvWlaMgt:
                    valWlaMgt = getInt();
                    break;
                case tlvWlaBnd:
                    valWlaBnd = getInt();
                    break;
            }
        }
        pck.getSkip(-siz);
        return false;
    }

    private void putStr(packHolder pck, int typ, String str) {
        if (str == null) {
            return;
        }
        tlv.putStr(pck, typ, str);
    }

    private void putAddr(packHolder pck, int typ, addrType adr) {
        if (adr == null) {
            return;
        }
        adr.toBuffer(tlv.valDat, 0);
        tlv.valSiz = adr.getSize();
        tlv.putBytes(pck, typ);
    }

    private <T extends addrType> void putPref(packHolder pck, int typ, addrPrefix<T> prf) {
        if (prf == null) {
            return;
        }
        tlv.valDat[0] = 0;
        tlv.valDat[1] = (byte) prf.maskLen;
        prf.network.toBuffer(tlv.valDat, 2);
        tlv.valSiz = prf.network.getSize() + 2;
        tlv.putBytes(pck, typ);
    }

    private void putInt(packHolder pck, int typ, int val) {
        if (val == -1) {
            return;
        }
        bits.msbPutD(tlv.valDat, 0, val);
        tlv.valSiz = 4;
        tlv.putBytes(pck, typ);
    }

    private void put2int(packHolder pck, int typL, int typH, long val) {
        if (val == -1) {
            return;
        }
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, (int) val);
        tlv.putBytes(pck, typL);
        bits.msbPutD(tlv.valDat, 0, (int) (val >>> 32));
        tlv.putBytes(pck, typH);
    }

    /**
     * parse packet
     *
     * @param pck packet to use
     * @param auth update auther
     * @param vend vendors to pass, null if nothing
     */
    public void createPacket(packHolder pck, boolean auth, tabGen<packRadiusOption> vend) {
        pck.putByte(0, code);
        pck.putByte(1, idnt);
        pck.msbPutW(2, 0);
        pck.putCopy(auther, 0, 4, auther.length);
        pck.putSkip(size);
        putStr(pck, tlvUsrNam, valUsrNam);
        if (valUsrPwd != null) {
            tlv.putBytes(pck, tlvUsrPwd, passwdEncrypt(secret, auther, valUsrPwd));
        }
        if (valChpPwd != null) {
            tlv.valDat[0] = (byte) valChpIdn;
            bits.byteCopy(valChpPwd, 0, tlv.valDat, 1, valChpPwd.length);
            tlv.valSiz = valChpPwd.length + 1;
            tlv.putBytes(pck, tlvChpPwd);
        }
        if (valChpChl != null) {
            tlv.putBytes(pck, tlvChpChl, valChpChl);
        }
        putAddr(pck, tlvNasAdr4, valNasAdr4);
        putInt(pck, tlvNasPrt, valNasPrt);
        putInt(pck, tlvSrvTyp, valSrvTyp);
        putInt(pck, tlvFrmPrt, valFrmPrt);
        putAddr(pck, tlvFrmAdr4, valFrmAdr4);
        putAddr(pck, tlvFrmMsk4, valFrmMsk4);
        putInt(pck, tlvFrmRtn, valFrmRtn);
        putStr(pck, tlvFilter, valFilter);
        putInt(pck, tlvFrmMtu, valFrmMtu);
        putInt(pck, tlvFrmCmp, valFrmCmp);
        putAddr(pck, tlvLgnHst4, valLgnHst4);
        putInt(pck, tlvLgnSvc, valLgnSvc);
        putInt(pck, tlvLgnTcp, valLgnTcp);
        putStr(pck, tlvReply, valReply);
        putStr(pck, tlvCllNum, valCllNum);
        putStr(pck, tlvCllId, valCllId);
        putStr(pck, tlvFrmRou4, valFrmRou4);
        putInt(pck, tlvFrmIpx, valFrmIpx);
        putStr(pck, tlvState, valState);
        putStr(pck, tlvClass, valClass);
        putInt(pck, tlvSesTim, valSesTim);
        putInt(pck, tlvIdlTim, valIdlTim);
        putInt(pck, tlvTrmAct, valTrmAct);
        putStr(pck, tlvCalled, valCalled);
        putStr(pck, tlvCalling, valCalling);
        putStr(pck, tlvNasId, valNasId);
        putStr(pck, tlvPrxSta, valPrxSta);
        putStr(pck, tlvLatSrv, valLatSrv);
        putStr(pck, tlvLatMod, valLatMod);
        putStr(pck, tlvLatGrp, valLatGrp);
        putInt(pck, tlvFrmLnk, valFrmLnk);
        putInt(pck, tlvFrmNet, valFrmNet);
        putStr(pck, tlvFrmZon, valFrmZon);
        putInt(pck, tlvActAut, valActAut);
        putInt(pck, tlvActDel, valActDel);
        put2int(pck, tlvActInB, tlvActInG, valActInB);
        put2int(pck, tlvActOtB, tlvActOtG, valActOtB);
        putInt(pck, tlvActInP, valActInP);
        putInt(pck, tlvActOtP, valActOtP);
        putInt(pck, tlvActInt, valActInt);
        putInt(pck, tlvActLnk, valActLnk);
        putInt(pck, tlvActSta, valActSta);
        putStr(pck, tlvActSes, valActSes);
        putInt(pck, tlvActTer, valActTer);
        putInt(pck, tlvActTim, valActTim);
        putStr(pck, tlvActMul, valActMul);
        putInt(pck, tlvPrtTyp, valPrtTyp);
        putInt(pck, tlvPrtLim, valPrtLim);
        putStr(pck, tlvLatPrt, valLatPrt);
        putInt(pck, tlvTunTyp, valTunTyp);
        putInt(pck, tlvTunMed, valTunMed);
        putStr(pck, tlvTunCln, valTunCln);
        putStr(pck, tlvTunSrv, valTunSrv);
        putStr(pck, tlvTunAct, valTunAct);
        putStr(pck, tlvTunPwd, valTunPwd);
        putStr(pck, tlvArpPwd, valArpPwd);
        putStr(pck, tlvArpFtr, valArpFtr);
        putInt(pck, tlvArpZon, valArpZon);
        putInt(pck, tlvArpSec, valArpSec);
        putStr(pck, tlvArpDat, valArpDat);
        putInt(pck, tlvPwdTry, valPwdTry);
        putInt(pck, tlvPrmpt, valPrmpt);
        putStr(pck, tlvConInf, valConInf);
        putStr(pck, tlvCfgTkn, valCfgTkn);
        putStr(pck, tlvEapMsg, valEapMsg);
        putStr(pck, tlvMsgAut, valMsgAut);
        putStr(pck, tlvTunPrv, valTunPrv);
        putStr(pck, tlvTunAsg, valTunAsg);
        putInt(pck, tlvTunPrf, valTunPrf);
        putStr(pck, tlvArpChl, valArpChl);
        putInt(pck, tlvActInt, valActInt);
        putInt(pck, tlvTunLst, valTunLst);
        putStr(pck, tlvNasPoi, valNasPoi);
        putStr(pck, tlvFrmPol4, valFrmPol4);
        putStr(pck, tlvCui, valCui);
        putStr(pck, tlvFrmCln, valFrmCln);
        putStr(pck, tlvFrmSrv, valFrmSrv);
        putStr(pck, tlvNasFlt, valNasFlt);
        putStr(pck, tlvOrgLin, valOrgLin);
        putAddr(pck, tlvFrmIfi, valFrmIfi);
        putPref(pck, tlvFrmPrf, valFrmPrf);
        putAddr(pck, tlvLgnHst6, valLgnHst6);
        putStr(pck, tlvFrmRou6, valFrmRou6);
        putStr(pck, tlvFrmPol6, valFrmPol6);
        putInt(pck, tlvErrCau, valErrCau);
        putStr(pck, tlvEapKey, valEapKey);
        putStr(pck, tlvDgsRsp, valDgsRsp);
        putStr(pck, tlvDgsRlm, valDgsRlm);
        putStr(pck, tlvDgsNon, valDgsNon);
        putStr(pck, tlvDgsAut, valDgsAut);
        putStr(pck, tlvDgsNno, valDgsNno);
        putStr(pck, tlvDgsMet, valDgsMet);
        putStr(pck, tlvDgsUri, valDgsUri);
        putStr(pck, tlvDgsQop, valDgsQop);
        putStr(pck, tlvDgsAlg, valDgsAlg);
        putStr(pck, tlvDgsEnt, valDgsEnt);
        putStr(pck, tlvDgsCno, valDgsCno);
        putStr(pck, tlvDgsNoc, valDgsNoc);
        putStr(pck, tlvDgsUsr, valDgsUsr);
        putStr(pck, tlvDgsOpq, valDgsOpq);
        putStr(pck, tlvDgsPrm, valDgsPrm);
        putStr(pck, tlvDgsAka, valDgsAka);
        putStr(pck, tlvDgsDom, valDgsDom);
        putStr(pck, tlvDgsStl, valDgsStl);
        putStr(pck, tlvDgsHa1, valDgsHa1);
        putStr(pck, tlvSipAor, valSipAor);
        putPref(pck, tlvDelPfx, valDelPfx);
        putInt(pck, tlvMipFtr, valMipFtr);
        putStr(pck, tlvMipHom, valMipHom);
        putStr(pck, tlvOprNam, valOprNam);
        putStr(pck, tlvLocInf, valLocInf);
        putStr(pck, tlvLocDat, valLocDat);
        putStr(pck, tlvLocBas, valLocBas);
        putStr(pck, tlvLocExt, valLocExt);
        putInt(pck, tlvLocCap, valLocCap);
        putInt(pck, tlvLocReq, valLocReq);
        putInt(pck, tlvFrmMgt, valFrmMgt);
        putInt(pck, tlvMgtPrt, valMgtPrt);
        putStr(pck, tlvMgtPol, valMgtPol);
        putInt(pck, tlvMgtPrv, valMgtPrv);
        putStr(pck, tlvPkmSsc, valPkmSsc);
        putStr(pck, tlvPkmCac, valPkmCac);
        putStr(pck, tlvPkmCfg, valPkmCfg);
        putStr(pck, tlvPkmCrp, valPkmCrp);
        putStr(pck, tlvPkmSai, valPkmSai);
        putStr(pck, tlvPkmSad, valPkmSad);
        putStr(pck, tlvPkmKey, valPkmKey);
        putStr(pck, tlvDslTun, valDslTun);
        putStr(pck, tlvMobNod, valMobNod);
        putStr(pck, tlvSrvSel, valSrvSel);
        putAddr(pck, tlvPmiHom6, valPmiHom6);
        putAddr(pck, tlvPmiVis6, valPmiVis6);
        putAddr(pck, tlvPmiHom4, valPmiHom4);
        putAddr(pck, tlvPmiVis4, valPmiVis4);
        putPref(pck, tlvPmiHomH, valPmiHomH);
        putPref(pck, tlvPmiVisH, valPmiVisH);
        putAddr(pck, tlvPmiHomI, valPmiHomI);
        putAddr(pck, tlvPmiVisI, valPmiVisI);
        putPref(pck, tlvPmiHomO, valPmiHomO);
        putPref(pck, tlvPmiVisO, valPmiVisO);
        putAddr(pck, tlvPmiHomD4, valPmiHomD4);
        putAddr(pck, tlvPmiVisD4, valPmiVisD4);
        putAddr(pck, tlvPmiHomD6, valPmiHomD6);
        putAddr(pck, tlvPmiVisD6, valPmiVisD6);
        putAddr(pck, tlvPmiHomG, valPmiHomG);
        putAddr(pck, tlvPmiVisG, valPmiVisG);
        putInt(pck, tlvEapLay, valEapLay);
        putStr(pck, tlvGssNam, valGssNam);
        putStr(pck, tlvGssHst, valGssHst);
        putStr(pck, tlvGssSpc, valGssSpc);
        putStr(pck, tlvGssRlm, valGssRlm);
        putAddr(pck, tlvFrmAdr6, valFrmAdr6);
        putAddr(pck, tlvDnsSrv6, valDnsSrv6);
        putPref(pck, tlvRouInf6, valRouInf6);
        putStr(pck, tlvDelPol6, valDelPol6);
        putStr(pck, tlvStaPol6, valStaPol6);
        putStr(pck, tlv6rdCfg, val6rdCfg);
        putStr(pck, tlvAlwSta, valAlwSta);
        putStr(pck, tlvEapPer, valEapPer);
        putStr(pck, tlvEapSrv, valEapSrv);
        putInt(pck, tlvMobDom, valMobDom);
        putInt(pck, tlvPreTim, valPreTim);
        putStr(pck, tlvNetName, valNetName);
        putStr(pck, tlvEapAno, valEapAno);
        putStr(pck, tlvWlaHes, valWlaHes);
        putInt(pck, tlvWlaVen, valWlaVen);
        putStr(pck, tlvWlaLng, valWlaLng);
        putStr(pck, tlvWlaNam, valWlaNam);
        putInt(pck, tlvWlaRea, valWlaRea);
        putInt(pck, tlvWlaPai, valWlaPai);
        putInt(pck, tlvWlaGrp, valWlaGrp);
        putInt(pck, tlvWlaAkm, valWlaAkm);
        putInt(pck, tlvWlaMgt, valWlaMgt);
        putInt(pck, tlvWlaBnd, valWlaBnd);
        if (vend != null) {
            for (int i = 0; i < vend.size(); i++) {
                packRadiusOption vnd = vend.get(i);
                tlv.putBytes(pck, tlvVendor, vnd.doEncode());
            }
        }
        pck.mergeHeader(0, size);
        pck.msbPutW(2 - size, pck.dataSize() + size);
        if (auth) {
            byte[] buf = calcReplyAuthen(code, idnt, secret, auther, pck.getCopy(), 0, pck.dataSize());
            pck.putCopy(buf, 0, 4 - size, buf.length);
        }
        pck.merge2beg();
    }

    /**
     * decrypt user password
     *
     * @param sec shared secret
     * @param auth authenticator
     * @param pwd ciphertext password
     * @return cleartext password
     */
    public static String passwdDecrypt(String sec, byte[] auth, byte[] pwd) {
        byte[] res = new byte[pwd.length];
        for (int o = 0; o < pwd.length;) {
            cryHashMd5 mdC = new cryHashMd5();
            mdC.init();
            mdC.update(sec.getBytes());
            mdC.update(auth);
            byte[] mdR = mdC.finish();
            auth = new byte[mdR.length];
            for (int i = 0; i < mdR.length; i++) {
                if ((o + i) >= pwd.length) {
                    break;
                }
                auth[i] = pwd[o + i];
                res[o + i] = (byte) (mdR[i] ^ pwd[o + i]);
            }
            o += mdR.length;
        }
        int o = res.length;
        for (int i = res.length - 1; i >= 0; i--) {
            if (res[i] == 0) {
                o = i;
            }
        }
        return new String(res, 0, o);
    }

    /**
     * encrypt user password
     *
     * @param sec shared secret
     * @param auth authenticator
     * @param pwd1 cleartext password
     * @return ciphertext password
     */
    public static byte[] passwdEncrypt(String sec, byte[] auth, String pwd1) {
        byte[] pwd2 = pwd1.getBytes();
        byte[] pwd3;
        if ((pwd2.length % 0xf) == 0) {
            pwd3 = pwd2;
        } else {
            pwd3 = new byte[pwd2.length + (0x10 - (pwd2.length & 0xf))];
            bits.byteFill(pwd3, 0, pwd3.length, 0);
            bits.byteCopy(pwd2, 0, pwd3, 0, pwd2.length);
        }
        byte[] res = new byte[pwd3.length];
        for (int p = 0; p < pwd3.length;) {
            cryHashMd5 mdC = new cryHashMd5();
            mdC.init();
            mdC.update(sec.getBytes());
            mdC.update(auth);
            auth = mdC.finish();
            for (int i = 0; i < auth.length; i++) {
                auth[i] ^= pwd3[p + i];
                bits.byteCopy(auth, 0, res, p, auth.length);
            }
            p += auth.length;
        }
        return res;
    }

    /**
     * calculate response authenticator
     *
     * @param code response code
     * @param id request id
     * @param sec secret
     * @param authen request authenticator
     * @param attrD response attributes
     * @param attrO offset of attributes
     * @param attrS size of attributes
     * @return calculated
     */
    public static byte[] calcReplyAuthen(int code, int id, String sec, byte[] authen, byte[] attrD, int attrO, int attrS) {
        byte[] buf = new byte[2];
        bits.msbPutW(buf, 0, attrS + size);
        cryHashMd5 mdC = new cryHashMd5();
        mdC.init();
        mdC.update(code);
        mdC.update(id);
        mdC.update(buf);
        mdC.update(authen);
        mdC.update(attrD, attrO, attrS);
        mdC.update(sec.getBytes());
        return mdC.finish();
    }

    /**
     * dump current packet
     *
     * @return packet dump
     */
    public String dump() {
        return code2string(code) + " id=" + idnt + " auth=" + bits.byteDump(auther, 0, -1) + " user=" + valUsrNam + " chap="
                + valChpIdn + " nasip=" + valNasAdr4 + " nasprt=" + valNasPrt + " service=" + valSrvTyp + " frnprt=" + valFrmPrt
                + " frmadr=" + valFrmAdr4 + " frnnsk=" + valFrmMsk4 + " frmrtr=" + valFrmRtn + " filter=" + valFilter + " mtu="
                + valFrmMtu + " comp=" + valFrmCmp + " logip=" + valLgnHst4 + " logserv=" + valLgnSvc + " logtcp=" + valLgnTcp
                + " reply=" + valReply + " cllbck=" + valCllNum + " callid=" + valCllId + " route=" + valFrmRou4 + " ipx="
                + valFrmIpx + " state=" + valState + " class=" + valClass + " sestim=" + valSesTim + " idltim=" + valIdlTim
                + " term=" + valTrmAct + " called=" + valCalled + " calling=" + valCalling + " nasid=" + valNasId + " latserv="
                + valLatSrv + " latmod=" + valLatMod + " latgrp=" + valLatGrp + " link=" + valFrmLnk + " net=" + valFrmNet
                + " zone=" + valFrmZon + " accmul=" + valActMul + " accaut=" + valActAut + " accdel=" + valActDel + " accinb="
                + valActInB + " accinp=" + valActInP + " accotb=" + valActOtB + " accotp=" + valActOtP + " accint=" + valActInt
                + " accses=" + valActSes + " accsta=" + valActSta + " accter=" + valActTer + " acctim=" + valActTim + " portyp="
                + valPrtTyp + " prtlim=" + valPrtLim + " latprt=" + valLatPrt + " tunTyp=" + valTunTyp + " tunMed=" + valTunMed
                + " tunCln=" + valTunCln + " tunSrv=" + valTunSrv + " tunAct=" + valTunAct + " tunPwd=" + valTunPwd + " arpPwd="
                + valArpPwd + " arpFtr=" + valArpFtr + " arpZon=" + valArpZon + " arpSec=" + valArpSec + " arpDat=" + valArpDat
                + "pwdTry=" + valPwdTry + " prmpt=" + valPrmpt + " conInf=" + valConInf + " cfgTkn=" + valCfgTkn + " eapMsg="
                + valEapMsg + " msgAut=" + valMsgAut + " tunPrv=" + valTunPrv + " tunAsg=" + valTunAsg + " tunPrf=" + valTunPrf
                + " arpChl=" + valArpChl + " actInt=" + valActInt + " tunLst=" + valTunLst + " nasPoi=" + valNasPoi + " frmPol4="
                + valFrmPol4 + " cui=" + valCui + " frmCln=" + valFrmCln + " frmSrv=" + valFrmSrv + " nasFlt=" + valNasFlt + " orgLin="
                + valOrgLin + " frmIfi=" + valFrmIfi + " frmPrf=" + valFrmPrf + " lgnHst6=" + valLgnHst6 + " frmRou6=" + valFrmRou6
                + " frmPol6=" + valFrmPol6 + " errCau=" + valErrCau + " eapKey=" + valEapKey + " dgsRsp=" + valDgsRsp + " dgsRlm="
                + valDgsRlm + " dgsNon=" + valDgsNon + " dgsAut=" + valDgsAut + " dgsNno=" + valDgsNno + " dgsMet=" + valDgsMet
                + " dgsUri=" + valDgsUri + "dgsQop=" + valDgsQop + " dgsAlg=" + valDgsAlg + " dgsEnt=" + valDgsEnt + " dgsCno="
                + valDgsCno + " dgsNoc=" + valDgsNoc + " dgsUsr=" + valDgsUsr + " dgsOpq=" + valDgsOpq + " dgsPrm=" + valDgsPrm
                + " dgsAka=" + valDgsAka + " dgsDom=" + valDgsDom + " dgsStl=" + valDgsStl + " dgsHa1=" + valDgsHa1 + " sipAor="
                + valSipAor + " delPfx=" + valDelPfx + " mipFtr=" + valMipFtr + " mipHom=" + valMipHom + " oprNam=" + valOprNam
                + " locInf=" + valLocInf + " locDat=" + valLocDat + " locBas=" + valLocBas + " locExt=" + valLocExt + " locCap="
                + valLocCap + " locReq=" + valLocReq + " frmMgt=" + valFrmMgt + " mgtPrt=" + valMgtPrt + " mgtPol=" + valMgtPol
                + " mgtPrv=" + valMgtPrv + " pkmSsc=" + valPkmSsc + " pkmCac=" + valPkmCac + " pkmCfg=" + valPkmCfg + " pkmCrp="
                + valPkmCrp + " pkmSai=" + valPkmSai + " pkmSad=" + valPkmSad + " pkmKey=" + valPkmKey + " dslTun=" + valDslTun
                + " mobNod=" + valMobNod + " srvSel=" + valSrvSel + " pmiHom6=" + valPmiHom6 + " pmiVis6=" + valPmiVis6 + " pmiHom4="
                + valPmiHom4 + " pmiVis4=" + valPmiVis4 + " pmiHomH=" + valPmiHomH + " pmiVisH=" + valPmiVisH + " pmiHomI="
                + valPmiHomI + " pmiVisI=" + valPmiVisI + " pmiHomO=" + valPmiHomO + " pmiVisO=" + valPmiVisO + " pmiHomD4="
                + valPmiHomD4 + " pmiVisD4=" + valPmiVisD4 + " pmiHomD6=" + valPmiHomD6 + " pmiVisD6=" + valPmiVisD6 + " pmiHomG="
                + valPmiHomG + " pmiVisG=" + valPmiVisG + " eapLay=" + valEapLay + " gssNam=" + valGssNam + " gssHst=" + valGssHst
                + " gssSpc=" + valGssSpc + " gssRlm=" + valGssRlm + " frmAdr6=" + valFrmAdr6 + " dnsSrv6=" + valDnsSrv6 + " rouInf6="
                + valRouInf6 + " delPol6=" + valDelPol6 + " staPol6=" + valStaPol6 + " 6rdCfg=" + val6rdCfg + " alwSta=" + valAlwSta
                + " eapPer=" + valEapPer + " eapSrv=" + valEapSrv + " mobDom=" + valMobDom + " preTim=" + valPreTim + " netName="
                + valNetName + " eapAno=" + valEapAno + " wlaHes=" + valWlaHes + " wlaVen=" + valWlaVen + " wlaLng=" + valWlaLng
                + " wlaNam=" + valWlaNam + " wlaRea=" + valWlaRea + " wlaPai=" + valWlaPai + " wlaGrp=" + valWlaGrp + " wlaAkm="
                + valWlaAkm + " wlaMgt=" + valWlaMgt + " wlaBnd=" + valWlaBnd;
    }

}

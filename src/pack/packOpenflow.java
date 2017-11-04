package pack;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrType;
import java.util.List;
import pipe.pipeSide;
import util.bits;
import util.typLenVal;

/**
 * openflow packet
 *
 * @author matecsaba
 */
public class packOpenflow {

    /**
     * port number
     */
    public static final int port = 6653;

    /**
     * header size
     */
    public static final int size = 8;

    /**
     * hello
     */
    public static final int typHello = 0;

    /**
     * error
     */
    public static final int typError = 1;

    /**
     * echo request
     */
    public static final int typEchoReq = 2;

    /**
     * echo reply
     */
    public static final int typEchoRep = 3;

    /**
     * feature request
     */
    public static final int typFeatReq = 5;

    /**
     * feature reply
     */
    public static final int typFeatRep = 6;

    /**
     * config request
     */
    public static final int typConfReq = 7;

    /**
     * config reply
     */
    public static final int typConfRep = 8;

    /**
     * config set
     */
    public static final int typConfSet = 9;

    /**
     * packet in
     */
    public static final int typPackIn = 10;

    /**
     * flow removed
     */
    public static final int typFlowDel = 11;

    /**
     * port status
     */
    public static final int typPortStat = 12;

    /**
     * packet out
     */
    public static final int typPackOut = 13;

    /**
     * flow mod
     */
    public static final int typFlowMod = 14;

    /**
     * group mod
     */
    public static final int typGrpMod = 15;

    /**
     * port mod
     */
    public static final int typPortMod = 16;

    /**
     * table mod
     */
    public static final int typTableMod = 17;

    /**
     * multipart request
     */
    public static final int typMultiReq = 18;

    /**
     * multipart reply
     */
    public static final int typMultiRep = 19;

    /**
     * barrier request
     */
    public static final int typBarrReq = 20;

    /**
     * barrier reply
     */
    public static final int typBarrRep = 21;

    /**
     * queue request
     */
    public static final int typQueReq = 22;

    /**
     * queue reply
     */
    public static final int typQueRep = 23;

    /**
     * role request
     */
    public static final int typRoleReq = 24;

    /**
     * role reply
     */
    public static final int typRoleRep = 25;

    /**
     * async request
     */
    public static final int typAsyncReq = 26;

    /**
     * async reply
     */
    public static final int typAsyncRep = 27;

    /**
     * async set
     */
    public static final int typAsyncSet = 28;

    /**
     * meter mod
     */
    public static final int typMeterMod = 29;

    /**
     * controller port
     */
    public static final int cntrlPort = 0xfffffffd;

    /**
     * type mask
     */
    public static final int matchMaskTyp = 0xfffffe;

    /**
     * have mask
     */
    public static final int matchMaskVal = 1;

    /**
     * logical port
     */
    public static final int matchPortLog = 0x800000 | 0 << 1;

    /**
     * physical port
     */
    public static final int matchPortPhy = 0x800000 | 1 << 1;
    /**
     * metadata
     */
    public static final int matchMeta = 0x800000 | 2 << 1;

    /**
     * ethernet destination
     */
    public static final int matchEthDst = 0x800000 | 3 << 1;

    /**
     * ethernet source
     */
    public static final int matchEthSrc = 0x800000 | 4 << 1;

    /**
     * ethernet type
     */
    public static final int matchEthTyp = 0x800000 | 5 << 1;

    /**
     * vlan id
     */
    public static final int matchVlanId = 0x800000 | 6 << 1;

    /**
     * vlan priority
     */
    public static final int matchVlanPri = 0x800000 | 7 << 1;

    /**
     * ip dscp
     */
    public static final int matchIpDscp = 0x800000 | 8 << 1;

    /**
     * ip ecn
     */
    public static final int matchIpEcn = 0x800000 | 9 << 1;

    /**
     * ip protocol
     */
    public static final int matchIpProto = 0x800000 | 10 << 1;

    /**
     * ipv4 source
     */
    public static final int matchIp4src = 0x800000 | 11 << 1;

    /**
     * ipv4 destination
     */
    public static final int matchIp4dst = 0x800000 | 12 << 1;

    /**
     * tcp source
     */
    public static final int matchTcpSrc = 0x800000 | 13 << 1;

    /**
     * tcp destination
     */
    public static final int matchTcpDst = 0x800000 | 14 << 1;

    /**
     * udp source
     */
    public static final int matchUdpSrc = 0x800000 | 15 << 1;

    /**
     * udp destination
     */
    public static final int matchUdpDst = 0x800000 | 16 << 1;

    /**
     * sctp source
     */
    public static final int matchSctpSrc = 0x800000 | 17 << 1;

    /**
     * sctp destination
     */
    public static final int matchSctpDst = 0x800000 | 18 << 1;

    /**
     * icmp4 type
     */
    public static final int matchIcmp4typ = 0x800000 | 19 << 1;

    /**
     * icmp4 code
     */
    public static final int matchIcmp4cod = 0x800000 | 20 << 1;

    /**
     * arp op
     */
    public static final int matchArpOp = 0x800000 | 21 << 1;

    /**
     * arp spa
     */
    public static final int matchArpSpa = 0x800000 | 22 << 1;

    /**
     * arp tpa
     */
    public static final int matchArpTpa = 0x800000 | 23 << 1;

    /**
     * arp sha
     */
    public static final int matchArpSha = 0x800000 | 24 << 1;

    /**
     * arp tha
     */
    public static final int matchArpTha = 0x800000 | 25 << 1;

    /**
     * ipv6 source
     */
    public static final int matchIp6src = 0x800000 | 26 << 1;

    /**
     * ipv6 destination
     */
    public static final int matchIp6dst = 0x800000 | 27 << 1;

    /**
     * ipv6 flow label
     */
    public static final int matchIp6flw = 0x800000 | 28 << 1;

    /**
     * icmp6 type
     */
    public static final int matchIcmp6typ = 0x800000 | 29 << 1;

    /**
     * icmp6 code
     */
    public static final int matchIcmp6cod = 0x800000 | 30 << 1;

    /**
     * icmp6 nd target
     */
    public static final int matchIcmp6trg = 0x800000 | 31 << 1;

    /**
     * icmp6 nd sll
     */
    public static final int matchIcmp6sll = 0x800000 | 32 << 1;

    /**
     * icmp6 nd tll
     */
    public static final int matchIcmp6tll = 0x800000 | 33 << 1;

    /**
     * mpls label
     */
    public static final int matchMplsLab = 0x800000 | 34 << 1;

    /**
     * mpls experimental
     */
    public static final int matchMplsExp = 0x800000 | 35 << 1;

    /**
     * mpls bottom of stack
     */
    public static final int matchMplsBos = 0x800000 | 36 << 1;

    /**
     * pbb s-sid
     */
    public static final int matchPbbIsid = 0x800000 | 37 << 1;

    /**
     * tunnel id
     */
    public static final int matchTunId = 0x800000 | 38 << 1;

    /**
     * ipv6 extension header
     */
    public static final int matchIp6ext = 0x800000 | 39 << 1;

    /**
     * pbb uca
     */
    public static final int matchPbbUca = 0x800000 | 41 << 1;

    /**
     * tcp flags
     */
    public static final int matchTcpFlg = 0x800000 | 42 << 1;

    /**
     * action set
     */
    public static final int matchActSet = 0x800000 | 43 << 1;

    /**
     * packet type
     */
    public static final int matchPckTyp = 0x800000 | 44 << 1;

    /**
     * output to port
     */
    public static final int actionOutput = 0;

    /**
     * copy ttl outwards
     */
    public static final int actionTtlOut = 11;

    /**
     * copy ttl inwards
     */
    public static final int actionTtlIn = 12;

    /**
     * set mpls ttl
     */
    public static final int actionMplsTtlSet = 15;

    /**
     * decrement mpls ttl
     */
    public static final int actionMplsTtlDec = 16;

    /**
     * push vlan
     */
    public static final int actionPushVlan = 17;

    /**
     * pop vlan
     */
    public static final int actionPopVlan = 18;

    /**
     * push mpls
     */
    public static final int actionMplsPush = 19;

    /**
     * pop mpls
     */
    public static final int actionMplsPop = 20;

    /**
     * set queue
     */
    public static final int actionQueue = 21;

    /**
     * group
     */
    public static final int actionGroup = 22;

    /**
     * set ip ttl
     */
    public static final int actionIpTtlSet = 23;

    /**
     * decrement ip ttl
     */
    public static final int actionIpTtlDec = 24;

    /**
     * set field
     */
    public static final int actionField = 25;

    /**
     * push pbb
     */
    public static final int actionPushPbb = 26;

    /**
     * pop pbb
     */
    public static final int actionPopPbb = 27;

    /**
     * copy field
     */
    public static final int actionCopy = 28;

    /**
     * meter
     */
    public static final int actionMeter = 29;

    /**
     * add
     */
    public static final int groupCmdAdd = 0;

    /**
     * modify
     */
    public static final int groupCmdMdf = 1;

    /**
     * delete
     */
    public static final int groupCmdDel = 2;

    /**
     * broadcast
     */
    public static final int groupTypAll = 0;

    /**
     * select
     */
    public static final int groupTypSel = 1;

    /**
     * indirect
     */
    public static final int groupTypInd = 2;

    /**
     * failover
     */
    public static final int groupTypFal = 3;

    /**
     * add
     */
    public static final int flowCmdAdd = 0;

    /**
     * modify
     */
    public static final int flowCmdMdf = 1;

    /**
     * modify strict
     */
    public static final int flowCmdMdfs = 2;

    /**
     * delete
     */
    public static final int flowCmdDel = 3;

    /**
     * delete strict
     */
    public static final int flowCmdDels = 4;

    /**
     * goto table
     */
    public static final int instGoto = 1;

    /**
     * write metadata
     */
    public static final int instMetWrt = 2;

    /**
     * write action
     */
    public static final int instActWrt = 3;

    /**
     * apply action
     */
    public static final int instActDo = 4;

    /**
     * clear action
     */
    public static final int instActClr = 5;

    /**
     * meter
     */
    public static final int instMeter = 6;

    /**
     * pipeline to use
     */
    public pipeSide pipe;

    /**
     * version
     */
    public int version;

    /**
     * type
     */
    public int type;

    /**
     * transaction id
     */
    public int xid;

    /**
     * receive one packet
     *
     * @param pck packet to receive
     * @return false on success, true on error
     */
    public boolean recvPack(packHolder pck) {
        pck.clear();
        if (pck.pipeRecv(pipe, 0, size, 144) != size) {
            return true;
        }
        version = pck.getByte(0);
        type = pck.getByte(1);
        int len = pck.msbGetW(2) - size;
        if (len < 0) {
            return true;
        }
        xid = pck.lsbGetD(4);
        pck.getSkip(size);
        if (len < 1) {
            return false;
        }
        if (pck.pipeRecv(pipe, 0, len, 144) != len) {
            return true;
        }
        return false;
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    public void sendPack(packHolder pck) {
        pck.putByte(0, version);
        pck.putByte(1, type);
        pck.msbPutW(2, pck.dataSize() + size);
        pck.msbPutD(4, xid);
        pck.putSkip(size);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 3);
    }

    /**
     * type to string
     *
     * @param i type
     * @return string
     */
    public static String type2str(int i) {
        switch (i) {
            case typHello:
                return "hello";
            case typError:
                return "error";
            case typEchoReq:
                return "echoReq";
            case typEchoRep:
                return "echoRep";
            case typFeatReq:
                return "featReq";
            case typFeatRep:
                return "featRep";
            case typConfReq:
                return "confReq";
            case typConfRep:
                return "confRep";
            case typConfSet:
                return "confSet";
            case typPackIn:
                return "packIn";
            case typFlowDel:
                return "flowDel";
            case typPortStat:
                return "portStat";
            case typPackOut:
                return "packOut";
            case typFlowMod:
                return "flowMod";
            case typGrpMod:
                return "grpMod";
            case typPortMod:
                return "portMod";
            case typTableMod:
                return "tableMod";
            case typMultiReq:
                return "multiReq";
            case typMultiRep:
                return "multiRep";
            case typBarrReq:
                return "barrReq";
            case typBarrRep:
                return "barrRep";
            case typQueReq:
                return "queReq";
            case typQueRep:
                return "queRep";
            case typRoleReq:
                return "roleReq";
            case typRoleRep:
                return "roleRep";
            case typAsyncReq:
                return "asyncReq";
            case typAsyncRep:
                return "asyncRep";
            case typAsyncSet:
                return "asyncSet";
            case typMeterMod:
                return "meterMod";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * dump packet
     *
     * @param pck packet
     * @return string
     */
    public String dump(packHolder pck) {
        return "ver=" + version + " typ=" + type2str(type) + " dat=" + pck.dump();
    }

    /**
     * create hello packet
     */
    public void createHello() {
        type = typHello;
    }

    /**
     * create echo request packet
     */
    public void createEchoReq() {
        type = typEchoReq;
    }

    /**
     * create echo reply packet
     */
    public void createEchoRep() {
        type = typEchoRep;
    }

    /**
     * create port mod packet
     *
     * @param pck packet
     * @param prt port
     * @param adr address
     * @param cfg config
     * @param msk mask
     */
    public void createPortMod(packHolder pck, int prt, addrType adr, int cfg, int msk) {
        type = typPortMod;
        pck.clear();
        pck.msbPutD(0, prt);
        pck.msbPutD(4, 0); // pad
        pck.putFill(8, 8, 0); // mac
        pck.putAddr(8, adr);
        pck.msbPutD(16, cfg);
        pck.msbPutD(20, msk);
        pck.msbPutD(24, 0); // adv
        pck.msbPutD(28, 0); // pad
        pck.putSkip(32);
        pck.merge2beg();
    }

    /**
     * get match buffer
     *
     * @param pck packet to read
     * @return matcher
     */
    public byte[] getMatchBuf(packHolder pck) {
        packHolder res = new packHolder(true, true);
        int typ = pck.msbGetW(0);
        int len = pck.msbGetW(2);
        pck.getSkip(4);
        byte[] buf = new byte[len - 4];
        pck.getCopy(buf, 0, 0, buf.length);
        pck.getSkip(buf.length);
        len = len & 7;
        if (len > 0) {
            pck.getSkip(8 - len);
        }
        if (typ != 1) {
            buf = new byte[0];
        }
        return buf;
    }

    /**
     * put match buffer
     *
     * @param pck packet to send
     * @param buf match buffer
     */
    public void putMatchBuf(packHolder pck, byte[] buf) {
        pck.msbPutW(0, 1); // type
        pck.msbPutW(2, buf.length + 4);
        pck.putSkip(4);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        int i = (buf.length + 4) & 7;
        if (i > 0) {
            i = 8 - i;
            pck.putFill(0, i, 0);
            pck.putSkip(i);
        }
        pck.merge2beg();
    }

    /**
     * get match tlv
     *
     * @return matcher
     */
    public typLenVal getMatchTlv() {
        return new typLenVal(0, 24, 24, 8, 1, 0, 4, 1, 0, 512, true);
    }

    /**
     * get match tlv
     *
     * @param pck packet to read
     * @return matcher
     */
    public typLenVal getMatchTlv(packHolder pck) {
        typLenVal tlv = getMatchTlv();
        if (tlv.getBytes(pck)) {
            return null;
        }
        return tlv;
    }

    /**
     * get action tlv
     *
     * @return action
     */
    public typLenVal getActionTlv() {
        return new typLenVal(0, 16, 16, 16, 1, 4, 4, 8, 4, 512, true);
    }

    /**
     * get output action
     *
     * @param prt port
     * @return action
     */
    public typLenVal getActionOutput(int prt) {
        typLenVal tlv = getActionTlv();
        tlv.valTyp = actionOutput;
        tlv.valSiz = 12;
        bits.msbPutD(tlv.valDat, 0, prt);
        bits.msbPutW(tlv.valDat, 4, 0xffff); // max length
        return tlv;
    }

    /**
     * get group action
     *
     * @param grp group
     * @return action
     */
    public typLenVal getActionGroup(int grp) {
        typLenVal tlv = getActionTlv();
        tlv.valTyp = actionGroup;
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, grp);
        return tlv;
    }

    /**
     * get push/pop mpls/vlan action
     *
     * @param cmd command
     * @param typ ethertype
     * @return action
     */
    public typLenVal getActionPush(int cmd, int typ) {
        typLenVal tlv = getActionTlv();
        tlv.valTyp = cmd;
        tlv.valSiz = 4;
        bits.msbPutW(tlv.valDat, 0, typ);
        return tlv;
    }

    /**
     * get set/decrement ip/mpls ttl action
     *
     * @param cmd command
     * @param val value
     * @return action
     */
    public typLenVal getActionTtl(int cmd, int val) {
        typLenVal tlv = getActionTlv();
        tlv.valTyp = cmd;
        tlv.valSiz = 4;
        tlv.valDat[0] = (byte) (val & 0xff);
        return tlv;
    }

    /**
     * get set field action
     *
     * @param pck sets
     * @return action
     */
    public typLenVal getActionSetField(packHolder pck) {
        pck.merge2beg();
        typLenVal tlv = getActionTlv();
        tlv.valTyp = actionField;
        tlv.valSiz = pck.dataSize();
        pck.getCopy(tlv.valDat, 0, 0, tlv.valSiz);
        int i = (tlv.valSiz + 4) & 7;
        if (i > 0) {
            tlv.valSiz += 8 - i;
        }
        return tlv;
    }

    /**
     * parse packet in
     *
     * @param pck packet to read
     * @return port number
     */
    public int parsePckIn(packHolder pck) {
        if (version < 4) {
            pck.getSkip(8); // header
        } else {
            pck.getSkip(16); // header
        }
        byte[] buf = getMatchBuf(pck);
        pck.getSkip(2); // pad
        packHolder mtch = new packHolder(true, true);
        mtch.putCopy(buf, 0, 0, buf.length);
        mtch.putSkip(buf.length);
        mtch.merge2beg();
        for (;;) {
            typLenVal tlv = getMatchTlv(mtch);
            if (tlv == null) {
                return -1;
            }
            if ((tlv.valTyp & matchMaskTyp) != matchPortLog) {
                continue;
            }
            return bits.msbGetD(tlv.valDat, 0);
        }
    }

    /**
     * create packet out
     *
     * @param pck packet to send
     * @param prt port id
     */
    public void createPckOut(packHolder pck, int prt) {
        type = typPackOut;
        typLenVal tlv = getActionOutput(prt);
        int i = pck.dataSize();
        tlv.putThis(pck);
        pck.merge2beg();
        i = pck.dataSize() - i;
        pck.msbPutD(0, 0xffffffff); // buffer id
        pck.msbPutD(4, cntrlPort); // in port is controller
        pck.msbPutW(8, i); // action length
        pck.putFill(10, 6, 0); // pad
        pck.putSkip(16);
        pck.merge2beg();
    }

    /**
     * create group modify
     *
     * @param pck packet to send
     * @param cmd command
     * @param typ group type
     * @param grp group id
     */
    public void createGroupMod(packHolder pck, int cmd, int typ, int grp) {
        type = typGrpMod;
        pck.msbPutW(0, cmd);
        pck.putByte(2, typ);
        pck.putByte(3, 0); // pad
        pck.msbPutD(4, grp);
        pck.putSkip(8);
        pck.merge2beg();
    }

    /**
     * create flow modify
     *
     * @param pck packet to send
     * @param cook cookie
     * @param tab table
     * @param cmd command
     * @param pri priority
     */
    public void createFlowMod(packHolder pck, long cook, int tab, int cmd, int pri) {
        type = typFlowMod;
        pck.msbPutQ(0, cook);
        pck.msbPutQ(8, 0); // mask
        pck.putByte(16, tab);
        pck.putByte(17, cmd);
        pck.msbPutW(18, 0); // idle
        pck.msbPutW(20, 0); // timeout
        pck.msbPutW(22, pri);
        pck.msbPutD(24, 0xffffffff); // buffer id
        pck.msbPutD(28, 0xffffffff); // port id
        pck.msbPutD(32, 0xffffffff); // group id
        pck.msbPutW(36, 0); // flags
        pck.msbPutW(38, 0); // pad
        pck.putSkip(40);
        pck.merge2beg();
    }

    /**
     * create feature message
     *
     * @param pck packet to send
     */
    public void createFeatures(packHolder pck) {
        type = typFeatReq;
        pck.merge2beg();
    }

    /**
     * create multipart message
     *
     * @param pck packet to send
     * @param typ type
     * @param flg flags
     */
    public void createMultipart(packHolder pck, int typ, int flg) {
        type = typMultiReq;
        pck.msbPutW(0, typ);
        pck.msbPutW(2, flg);
        pck.msbPutD(4, 0); // pad
        pck.putSkip(8);
        pck.merge2beg();
    }

    /**
     * create goto instruction
     *
     * @param pck packet to append
     * @param tab table id
     */
    public void createInstrGoto(packHolder pck, int tab) {
        pck.msbPutW(0, instGoto);
        pck.msbPutW(2, 8); // size
        pck.msbPutD(4, 0); // pad
        pck.putByte(4, tab);
        pck.putSkip(8);
        pck.merge2end();
    }

    /**
     * create action instruction
     *
     * @param pck packet to append
     * @param tlvs actions
     */
    public void createInstrAct(packHolder pck, List<typLenVal> tlvs) {
        int o = 8;
        for (int i = 0; i < tlvs.size(); i++) {
            o += tlvs.get(i).valSiz + 4;
        }
        pck.msbPutW(0, instActDo);
        pck.msbPutW(2, o);
        pck.msbPutD(4, 0); // pad
        pck.putSkip(8);
        for (int i = 0; i < tlvs.size(); i++) {
            tlvs.get(i).putThis(pck);
        }
    }

    /**
     * create action bucket
     *
     * @param pck packet to append
     * @param tlvs actions
     */
    public void createBucketAct(packHolder pck, List<typLenVal> tlvs) {
        int o = 16;
        for (int i = 0; i < tlvs.size(); i++) {
            o += tlvs.get(i).valSiz + 4;
        }
        pck.msbPutW(0, o);
        pck.msbPutW(2, 0); // weigth
        pck.msbPutD(4, 0xffffffff); // watch port
        pck.msbPutD(8, 0xffffffff); // watch group
        pck.msbPutD(12, 0); // pad
        pck.putSkip(16);
        for (int i = 0; i < tlvs.size(); i++) {
            tlvs.get(i).putThis(pck);
        }
    }

    /**
     * create port matcher
     *
     * @param pck packet to append
     * @param prt port number
     */
    public void createMatchPort(packHolder pck, int prt) {
        typLenVal tlv = getMatchTlv();
        tlv.valTyp = matchPortLog;
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, prt);
        tlv.putThis(pck);
    }

    /**
     * create ethertype matcher
     *
     * @param pck packet to append
     * @param typ ethertype
     */
    public void createMatchEthTyp(packHolder pck, int typ) {
        typLenVal tlv = getMatchTlv();
        tlv.valTyp = matchEthTyp;
        tlv.valSiz = 2;
        bits.msbPutW(tlv.valDat, 0, typ);
        tlv.putThis(pck);
    }

    /**
     * create vlan matcher
     *
     * @param pck packet to append
     * @param id vlan
     * @param msk mask, -1 if none
     */
    public void createMatchVlan(packHolder pck, int id, int msk) {
        typLenVal tlv = getMatchTlv();
        tlv.valTyp = matchVlanId;
        tlv.valSiz = 2;
        bits.msbPutW(tlv.valDat, 0, id);
        if (msk >= 0) {
            tlv.valSiz += 2;
            bits.msbPutW(tlv.valDat, 2, msk);
        }
        tlv.putThis(pck);
    }

    /**
     * create mac matcher
     *
     * @param pck packet to append
     * @param src true=source, false=destination
     * @param adr address to match
     * @param msk mask mac, null if none
     */
    public void createMatchMac(packHolder pck, boolean src, addrMac adr, addrMac msk) {
        typLenVal tlv = getMatchTlv();
        if (src) {
            tlv.valTyp = matchEthSrc;
        } else {
            tlv.valTyp = matchEthDst;
        }
        tlv.valSiz = addrMac.size;
        adr.toBuffer(tlv.valDat, 0);
        if (msk != null) {
            msk.toBuffer(tlv.valDat, addrMac.size);
            tlv.valSiz += addrMac.size;
            tlv.valTyp |= matchMaskVal;
        }
        tlv.putThis(pck);
    }

    /**
     * create ipv4 matcher
     *
     * @param pck packet to append
     * @param src true=source, false=destination
     * @param adr address to match
     * @param msk mask mac, null if none
     */
    public void createMatchIpv4(packHolder pck, boolean src, addrIP adr, addrIP msk) {
        typLenVal tlv = getMatchTlv();
        if (src) {
            tlv.valTyp = matchIp4src;
        } else {
            tlv.valTyp = matchIp4dst;
        }
        tlv.valSiz = addrIPv4.size;
        adr.toIPv4().toBuffer(tlv.valDat, 0);
        if (msk != null) {
            msk.toIPv4().toBuffer(tlv.valDat, addrIPv4.size);
            tlv.valSiz += addrIPv4.size;
            tlv.valTyp |= matchMaskVal;
        }
        tlv.putThis(pck);
    }

    /**
     * create ipv6 matcher
     *
     * @param pck packet to append
     * @param src true=source, false=destination
     * @param adr address to match
     * @param msk mask mac, null if none
     */
    public void createMatchIpv6(packHolder pck, boolean src, addrIP adr, addrIP msk) {
        typLenVal tlv = getMatchTlv();
        if (src) {
            tlv.valTyp = matchIp6src;
        } else {
            tlv.valTyp = matchIp6dst;
        }
        tlv.valSiz = addrIPv6.size;
        adr.toIPv6().toBuffer(tlv.valDat, 0);
        if (msk != null) {
            msk.toIPv6().toBuffer(tlv.valDat, addrIPv6.size);
            tlv.valSiz += addrIPv6.size;
            tlv.valTyp |= matchMaskVal;
        }
        tlv.putThis(pck);
    }

    /**
     * create mpls label matcher
     *
     * @param pck packet to append
     * @param lab label
     */
    public void createMatchMplsLab(packHolder pck, int lab) {
        typLenVal tlv = getMatchTlv();
        tlv.valTyp = matchMplsLab;
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, lab);
        tlv.putThis(pck);
    }

    /**
     * create mpls bottom matcher
     *
     * @param pck packet to append
     * @param bottom true if bottom, false if not
     */
    public void createMatchMplsBos(packHolder pck, boolean bottom) {
        typLenVal tlv = getMatchTlv();
        tlv.valTyp = matchMplsBos;
        tlv.valSiz = 1;
        if (bottom) {
            tlv.valDat[0] = 1;
        } else {
            tlv.valDat[0] = 0;
        }
        tlv.putThis(pck);
    }

    /**
     * create protocol matcher
     *
     * @param pck packet to append
     * @param prt protocol
     */
    public void createMatchProto(packHolder pck, int prt) {
        typLenVal tlv = getMatchTlv();
        tlv.valTyp = matchIpProto;
        tlv.valSiz = 1;
        tlv.valDat[0] = (byte) prt;
        tlv.putThis(pck);
    }

    /**
     * create dscp matcher
     *
     * @param pck packet to append
     * @param dscp dscp
     */
    public void createMatchDscp(packHolder pck, int dscp) {
        typLenVal tlv = getMatchTlv();
        tlv.valTyp = matchIpDscp;
        tlv.valSiz = 1;
        tlv.valDat[0] = (byte) dscp;
        tlv.putThis(pck);
    }

    /**
     * create ecn matcher
     *
     * @param pck packet to append
     * @param ecn ecn
     */
    public void createMatchEcn(packHolder pck, int ecn) {
        typLenVal tlv = getMatchTlv();
        tlv.valTyp = matchIpEcn;
        tlv.valSiz = 1;
        tlv.valDat[0] = (byte) ecn;
        tlv.putThis(pck);
    }

    /**
     * create tcp matcher
     *
     * @param pck packet to append
     * @param src true=source, false=destination
     * @param prt port to match
     */
    public void createMatchTcp(packHolder pck, boolean src, int prt) {
        typLenVal tlv = getMatchTlv();
        if (src) {
            tlv.valTyp = matchTcpSrc;
        } else {
            tlv.valTyp = matchTcpDst;
        }
        tlv.valSiz = 2;
        bits.msbPutW(tlv.valDat, 0, prt);
        tlv.putThis(pck);
    }

    /**
     * create udp matcher
     *
     * @param pck packet to append
     * @param src true=source, false=destination
     * @param prt port to match
     */
    public void createMatchUdp(packHolder pck, boolean src, int prt) {
        typLenVal tlv = getMatchTlv();
        if (src) {
            tlv.valTyp = matchUdpSrc;
        } else {
            tlv.valTyp = matchUdpDst;
        }
        tlv.valSiz = 2;
        bits.msbPutW(tlv.valDat, 0, prt);
        tlv.putThis(pck);
    }

    /**
     * create udp matcher
     *
     * @param pck packet to append
     * @param src true=source, false=destination
     * @param prt port to match
     */
    public void createMatchSctp(packHolder pck, boolean src, int prt) {
        typLenVal tlv = getMatchTlv();
        if (src) {
            tlv.valTyp = matchSctpSrc;
        } else {
            tlv.valTyp = matchSctpDst;
        }
        tlv.valSiz = 2;
        bits.msbPutW(tlv.valDat, 0, prt);
        tlv.putThis(pck);
    }

}

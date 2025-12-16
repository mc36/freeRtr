package org.freertr.pack;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

/**
 * openflow packet
 *
 * @author matecsaba
 */
public class packOpenflow {

    /**
     * create instance
     */
    public packOpenflow() {
    }

    /**
     * port number
     */
    public final static int port = 6653;

    /**
     * header size
     */
    public final static int size = 8;

    /**
     * hello
     */
    public final static int typHello = 0;

    /**
     * error
     */
    public final static int typError = 1;

    /**
     * echo request
     */
    public final static int typEchoReq = 2;

    /**
     * echo reply
     */
    public final static int typEchoRep = 3;

    /**
     * feature request
     */
    public final static int typFeatReq = 5;

    /**
     * feature reply
     */
    public final static int typFeatRep = 6;

    /**
     * config request
     */
    public final static int typConfReq = 7;

    /**
     * config reply
     */
    public final static int typConfRep = 8;

    /**
     * config set
     */
    public final static int typConfSet = 9;

    /**
     * packet in
     */
    public final static int typPackIn = 10;

    /**
     * flow removed
     */
    public final static int typFlowDel = 11;

    /**
     * port status
     */
    public final static int typPortStat = 12;

    /**
     * packet out
     */
    public final static int typPackOut = 13;

    /**
     * flow mod
     */
    public final static int typFlowMod = 14;

    /**
     * group mod
     */
    public final static int typGrpMod = 15;

    /**
     * port mod
     */
    public final static int typPortMod = 16;

    /**
     * table mod
     */
    public final static int typTableMod = 17;

    /**
     * multipart request
     */
    public final static int typMultiReq = 18;

    /**
     * multipart reply
     */
    public final static int typMultiRep = 19;

    /**
     * barrier request
     */
    public final static int typBarrReq = 20;

    /**
     * barrier reply
     */
    public final static int typBarrRep = 21;

    /**
     * queue request
     */
    public final static int typQueReq = 22;

    /**
     * queue reply
     */
    public final static int typQueRep = 23;

    /**
     * role request
     */
    public final static int typRoleReq = 24;

    /**
     * role reply
     */
    public final static int typRoleRep = 25;

    /**
     * async request
     */
    public final static int typAsyncReq = 26;

    /**
     * async reply
     */
    public final static int typAsyncRep = 27;

    /**
     * async set
     */
    public final static int typAsyncSet = 28;

    /**
     * meter mod
     */
    public final static int typMeterMod = 29;

    /**
     * controller port
     */
    public final static int cntrlPort = 0xfffffffd;

    /**
     * type mask
     */
    public final static int matchMaskTyp = 0xfffffe;

    /**
     * have mask
     */
    public final static int matchMaskVal = 1;

    /**
     * logical port
     */
    public final static int matchPortLog = 0x800000 | 0 << 1;

    /**
     * physical port
     */
    public final static int matchPortPhy = 0x800000 | 1 << 1;
    /**
     * metadata
     */
    public final static int matchMeta = 0x800000 | 2 << 1;

    /**
     * ethernet destination
     */
    public final static int matchEthDst = 0x800000 | 3 << 1;

    /**
     * ethernet source
     */
    public final static int matchEthSrc = 0x800000 | 4 << 1;

    /**
     * ethernet type
     */
    public final static int matchEthTyp = 0x800000 | 5 << 1;

    /**
     * vlan id
     */
    public final static int matchVlanId = 0x800000 | 6 << 1;

    /**
     * vlan priority
     */
    public final static int matchVlanPri = 0x800000 | 7 << 1;

    /**
     * ip dscp
     */
    public final static int matchIpDscp = 0x800000 | 8 << 1;

    /**
     * ip ecn
     */
    public final static int matchIpEcn = 0x800000 | 9 << 1;

    /**
     * ip protocol
     */
    public final static int matchIpProto = 0x800000 | 10 << 1;

    /**
     * ipv4 source
     */
    public final static int matchIp4src = 0x800000 | 11 << 1;

    /**
     * ipv4 destination
     */
    public final static int matchIp4dst = 0x800000 | 12 << 1;

    /**
     * tcp source
     */
    public final static int matchTcpSrc = 0x800000 | 13 << 1;

    /**
     * tcp destination
     */
    public final static int matchTcpDst = 0x800000 | 14 << 1;

    /**
     * udp source
     */
    public final static int matchUdpSrc = 0x800000 | 15 << 1;

    /**
     * udp destination
     */
    public final static int matchUdpDst = 0x800000 | 16 << 1;

    /**
     * sctp source
     */
    public final static int matchSctpSrc = 0x800000 | 17 << 1;

    /**
     * sctp destination
     */
    public final static int matchSctpDst = 0x800000 | 18 << 1;

    /**
     * icmp4 type
     */
    public final static int matchIcmp4typ = 0x800000 | 19 << 1;

    /**
     * icmp4 code
     */
    public final static int matchIcmp4cod = 0x800000 | 20 << 1;

    /**
     * arp op
     */
    public final static int matchArpOp = 0x800000 | 21 << 1;

    /**
     * arp spa
     */
    public final static int matchArpSpa = 0x800000 | 22 << 1;

    /**
     * arp tpa
     */
    public final static int matchArpTpa = 0x800000 | 23 << 1;

    /**
     * arp sha
     */
    public final static int matchArpSha = 0x800000 | 24 << 1;

    /**
     * arp tha
     */
    public final static int matchArpTha = 0x800000 | 25 << 1;

    /**
     * ipv6 source
     */
    public final static int matchIp6src = 0x800000 | 26 << 1;

    /**
     * ipv6 destination
     */
    public final static int matchIp6dst = 0x800000 | 27 << 1;

    /**
     * ipv6 flow label
     */
    public final static int matchIp6flw = 0x800000 | 28 << 1;

    /**
     * icmp6 type
     */
    public final static int matchIcmp6typ = 0x800000 | 29 << 1;

    /**
     * icmp6 code
     */
    public final static int matchIcmp6cod = 0x800000 | 30 << 1;

    /**
     * icmp6 nd target
     */
    public final static int matchIcmp6trg = 0x800000 | 31 << 1;

    /**
     * icmp6 nd sll
     */
    public final static int matchIcmp6sll = 0x800000 | 32 << 1;

    /**
     * icmp6 nd tll
     */
    public final static int matchIcmp6tll = 0x800000 | 33 << 1;

    /**
     * mpls label
     */
    public final static int matchMplsLab = 0x800000 | 34 << 1;

    /**
     * mpls experimental
     */
    public final static int matchMplsExp = 0x800000 | 35 << 1;

    /**
     * mpls bottom of stack
     */
    public final static int matchMplsBos = 0x800000 | 36 << 1;

    /**
     * pbb s-sid
     */
    public final static int matchPbbIsid = 0x800000 | 37 << 1;

    /**
     * tunnel id
     */
    public final static int matchTunId = 0x800000 | 38 << 1;

    /**
     * ipv6 extension header
     */
    public final static int matchIp6ext = 0x800000 | 39 << 1;

    /**
     * pbb uca
     */
    public final static int matchPbbUca = 0x800000 | 41 << 1;

    /**
     * tcp flags
     */
    public final static int matchTcpFlg = 0x800000 | 42 << 1;

    /**
     * action set
     */
    public final static int matchActSet = 0x800000 | 43 << 1;

    /**
     * packet type
     */
    public final static int matchPckTyp = 0x800000 | 44 << 1;

    /**
     * output to port
     */
    public final static int actionOutput = 0;

    /**
     * copy ttl outwards
     */
    public final static int actionTtlOut = 11;

    /**
     * copy ttl inwards
     */
    public final static int actionTtlIn = 12;

    /**
     * set mpls ttl
     */
    public final static int actionMplsTtlSet = 15;

    /**
     * decrement mpls ttl
     */
    public final static int actionMplsTtlDec = 16;

    /**
     * push vlan
     */
    public final static int actionPushVlan = 17;

    /**
     * pop vlan
     */
    public final static int actionPopVlan = 18;

    /**
     * push mpls
     */
    public final static int actionMplsPush = 19;

    /**
     * pop mpls
     */
    public final static int actionMplsPop = 20;

    /**
     * set queue
     */
    public final static int actionQueue = 21;

    /**
     * group
     */
    public final static int actionGroup = 22;

    /**
     * set ip ttl
     */
    public final static int actionIpTtlSet = 23;

    /**
     * decrement ip ttl
     */
    public final static int actionIpTtlDec = 24;

    /**
     * set field
     */
    public final static int actionField = 25;

    /**
     * push pbb
     */
    public final static int actionPushPbb = 26;

    /**
     * pop pbb
     */
    public final static int actionPopPbb = 27;

    /**
     * copy field
     */
    public final static int actionCopy = 28;

    /**
     * meter
     */
    public final static int actionMeter = 29;

    /**
     * add
     */
    public final static int groupCmdAdd = 0;

    /**
     * modify
     */
    public final static int groupCmdMdf = 1;

    /**
     * delete
     */
    public final static int groupCmdDel = 2;

    /**
     * broadcast
     */
    public final static int groupTypAll = 0;

    /**
     * select
     */
    public final static int groupTypSel = 1;

    /**
     * indirect
     */
    public final static int groupTypInd = 2;

    /**
     * failover
     */
    public final static int groupTypFal = 3;

    /**
     * add
     */
    public final static int flowCmdAdd = 0;

    /**
     * modify
     */
    public final static int flowCmdMdf = 1;

    /**
     * modify strict
     */
    public final static int flowCmdMdfs = 2;

    /**
     * delete
     */
    public final static int flowCmdDel = 3;

    /**
     * delete strict
     */
    public final static int flowCmdDels = 4;

    /**
     * goto table
     */
    public final static int instGoto = 1;

    /**
     * write metadata
     */
    public final static int instMetWrt = 2;

    /**
     * write action
     */
    public final static int instActWrt = 3;

    /**
     * apply action
     */
    public final static int instActDo = 4;

    /**
     * clear action
     */
    public final static int instActClr = 5;

    /**
     * meter
     */
    public final static int instMeter = 6;

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
    public encTlv getMatchTlv() {
        return new encTlv(0, 24, 24, 8, 1, 0, 4, 1, 0, 512, true);
    }

    /**
     * get match tlv
     *
     * @param pck packet to read
     * @return matcher
     */
    public encTlv getMatchTlv(packHolder pck) {
        encTlv tlv = getMatchTlv();
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
    public encTlv getActionTlv() {
        return new encTlv(0, 16, 16, 16, 1, 4, 4, 8, 4, 512, true);
    }

    /**
     * get output action
     *
     * @param prt port
     * @return action
     */
    public encTlv getActionOutput(int prt) {
        encTlv tlv = getActionTlv();
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
    public encTlv getActionGroup(int grp) {
        encTlv tlv = getActionTlv();
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
    public encTlv getActionPush(int cmd, int typ) {
        encTlv tlv = getActionTlv();
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
    public encTlv getActionTtl(int cmd, int val) {
        encTlv tlv = getActionTlv();
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
    public encTlv getActionSetField(packHolder pck) {
        pck.merge2beg();
        encTlv tlv = getActionTlv();
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
            encTlv tlv = getMatchTlv(mtch);
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
        encTlv tlv = getActionOutput(prt);
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
    public void createInstrAct(packHolder pck, List<encTlv> tlvs) {
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
    public void createBucketAct(packHolder pck, List<encTlv> tlvs) {
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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
        encTlv tlv = getMatchTlv();
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

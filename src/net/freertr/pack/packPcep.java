package net.freertr.pack;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabHop;
import net.freertr.util.bits;
import net.freertr.util.typLenVal;

/**
 * path computation element protocol (rfc5440) packet
 *
 * @author matecsaba
 */
public class packPcep {

    /**
     * create instance
     */
    public packPcep() {
    }

    /**
     * port number
     */
    public static final int port = 4189;

    /**
     * header size
     */
    public static final int size1 = 4;

    /**
     * object size
     */
    public static final int size2 = 4;

    /**
     * pipe to use
     */
    public pipeSide pipe;

    /**
     * message type
     */
    public int msgTyp;

    /**
     * rp loose
     */
    public boolean loose;

    /**
     * rp setup type, 0=te, 1=sr
     */
    public int setupType;

    /**
     * rp request id
     */
    public int reqId;

    /**
     * true=ipv4, false=ipv6
     */
    public boolean isIP4;

    /**
     * source address
     */
    public addrIP srcAddr = new addrIP();

    /**
     * target address
     */
    public addrIP trgAddr = new addrIP();

    /**
     * plsp id
     */
    public int plspId;

    /**
     * exclude any
     */
    public int exclAny;

    /**
     * include any
     */
    public int inclAny;

    /**
     * include all
     */
    public int inclAll;

    /**
     * setup priority
     */
    public int priSet;

    /**
     * hold priority
     */
    public int priHld;

    /**
     * bandwidth
     */
    public float bandwidth;

    /**
     * metric type, 1=igp, 2=te, 3=hops
     */
    public int metTyp;

    /**
     * metric value
     */
    public int metVal;

    /**
     * explicit route
     */
    public List<tabHop> ero;

    /**
     * record route
     */
    public List<tabHop> rro;

    /**
     * open
     */
    public static final int typOpen = 1;

    /**
     * keepalive
     */
    public static final int typKeep = 2;

    /**
     * request
     */
    public static final int typReq = 3;

    /**
     * reply
     */
    public static final int typRep = 4;

    /**
     * notify
     */
    public static final int typNot = 5;

    /**
     * error
     */
    public static final int typErr = 6;

    /**
     * close
     */
    public static final int typCls = 7;

    /**
     * monitor request
     */
    public static final int typMonReq = 8;

    /**
     * monitor reply
     */
    public static final int typMonRep = 9;

    /**
     * report
     */
    public static final int typRpt = 10;

    /**
     * update
     */
    public static final int typUpd = 11;

    /**
     * initiate request
     */
    public static final int typInit = 12;

    /**
     * start tls
     */
    public static final int typTls = 13;

    /**
     * version
     */
    public static final int version = 1;

    /**
     * rp
     */
    public static final int otpRP = 0x0210;

    /**
     * ipv4 endpoint
     */
    public static final int otpEndpt4 = 0x0410;

    /**
     * ipv6 endpoint
     */
    public static final int otpEndpt6 = 0x0420;

    /**
     * lsp
     */
    public static final int otpLsp = 0x2010;

    /**
     * lsp attributes
     */
    public static final int otpLspa = 0x0910;

    /**
     * bandwidth
     */
    public static final int otpBwdt = 0x0510;

    /**
     * metric
     */
    public static final int otpMtrc = 0x0610;

    /**
     * no path
     */
    public static final int otpNopt = 0x0310;

    /**
     * ero
     */
    public static final int otpEro = 0x0710;

    /**
     * rro
     */
    public static final int otpRro = 0x0810;

    private packHolder pck1 = new packHolder(true, true);

    private packHolder pck2 = new packHolder(true, true);

    private typLenVal tlv = new typLenVal(0, 16, 16, 16, 1, 0, 4, 4, 0, 512, true);

    public String toString() {
        String a;
        switch (msgTyp) {
            case typOpen:
                a = "open";
                break;
            case typKeep:
                a = "keepalive";
                break;
            case typReq:
                a = "request";
                break;
            case typRep:
                a = "reply";
                break;
            case typNot:
                a = "notification";
                break;
            case typErr:
                a = "error";
                break;
            case typCls:
                a = "close";
                break;
            case typMonReq:
                a = "monitor-request";
                break;
            case typMonRep:
                a = "monitor-reply";
                break;
            case typRpt:
                a = "report";
                break;
            case typUpd:
                a = "update";
                break;
            case typInit:
                a = "initiate";
                break;
            case typTls:
                a = "start-tls";
                break;
            default:
                a = "unknown=" + msgTyp;
                break;
        }
        return "type=" + a + " reqid=" + reqId + " loose=" + loose + " type=" + setupType + " ipv4=" + isIP4 + " source=" + srcAddr + " target=" + trgAddr + " plsp=" + plspId + " affinity=" + exclAny + "/" + inclAny + "/" + inclAll + " priority=" + priSet + "/" + priHld + " bandwidth=" + bandwidth + " metric=" + metTyp + "/" + metVal + " ero=" + tabHop.dumpList(ero) + " rro=" + tabHop.dumpList(rro);
    }

    private void putObj(int obj) {
        pck2.merge2beg();
        byte[] buf = pck2.getCopy();
        pck1.msbPutW(0, obj); // type
        pck1.msbPutW(2, buf.length + size2); // length
        pck1.putSkip(size2);
        pck1.putCopy(buf, 0, 0, buf.length);
        pck1.putSkip(buf.length);
        pck1.merge2end();
    }

    private int getObj() {
        if (pck1.dataSize() < size2) {
            return -1;
        }
        int obj = pck1.msbGetW(0); // type
        int len = pck1.msbGetW(2) - size2; // length
        pck1.getSkip(size2);
        byte[] buf = new byte[len];
        pck1.getCopy(buf, 0, 0, buf.length);
        pck1.getSkip(buf.length);
        pck2.clear();
        pck2.putCopy(buf, 0, 0, buf.length);
        pck2.putSkip(buf.length);
        pck2.merge2end();
        return obj;
    }

    /**
     * receive a packet
     *
     * @return false on success, true on error
     */
    public boolean recvPack() {
        pck1.clear();
        if (pck1.pipeRecv(pipe, 0, size1, 144) != size1) {
            return true;
        }
        msgTyp = pck1.getByte(0); // vers flags
        if ((msgTyp >>> 5) != version) {
            return true;
        }
        msgTyp = pck1.getByte(1); // type
        int len = pck1.msbGetW(2) - size1; // length
        if (len < 0) {
            return true;
        }
        pck1.clear();
        if (len > 0) {
            if (pck1.pipeRecv(pipe, 0, len, 144) != len) {
                return true;
            }
        }
        for (;;) {
            int obj = getObj();
            if (obj < 0) {
                break;
            }
            switch (obj & 0xfff0) {
                case otpRP:
                    reqId = pck2.msbGetD(0);
                    loose = (reqId & 0x20) != 0;
                    reqId = pck2.msbGetD(4);
                    pck2.getSkip(8);
                    for (;;) {
                        if (tlv.getBytes(pck2)) {
                            break;
                        }
                        switch (tlv.valTyp) {
                            case 28:
                                setupType = tlv.valDat[3];
                                break;
                        }
                    }
                    break;
                case otpLsp:
                    plspId = pck2.msbGetD(0);
                    break;
                case otpLspa:
                    exclAny = pck2.msbGetD(0);
                    inclAny = pck2.msbGetD(4);
                    inclAll = pck2.msbGetD(8);
                    priSet = pck2.getByte(12);
                    priHld = pck2.getByte(13);
                    break;
                case otpBwdt:
                    bandwidth = Float.intBitsToFloat(pck2.msbGetD(0));
                    break;
                case otpMtrc:
                    metTyp = pck2.msbGetW(2);
                    metVal = pck2.msbGetD(4);
                    break;
                case otpEro:
                    ero = tabHop.parseHops(pck2);
                    break;
                case otpRro:
                    rro = tabHop.parseHops(pck2);
                    break;
                case otpEndpt4:
                    isIP4 = true;
                    addrIPv4 adr4 = new addrIPv4();
                    pck2.getAddr(adr4, 0);
                    srcAddr.fromIPv4addr(adr4);
                    pck2.getAddr(adr4, addrIPv4.size);
                    trgAddr.fromIPv4addr(adr4);
                    break;
                case otpEndpt6:
                    isIP4 = false;
                    addrIPv6 adr6 = new addrIPv6();
                    pck2.getAddr(adr6, 0);
                    srcAddr.fromIPv6addr(adr6);
                    pck2.getAddr(adr6, addrIPv6.size);
                    trgAddr.fromIPv6addr(adr6);
                    break;
                default:
                    break;
            }
        }
        return false;
    }

    /**
     * send packet
     */
    public void sendPack() {
        pck1.putByte(0, version << 5); // vers flags
        pck1.putByte(1, msgTyp); // type
        pck1.msbPutW(2, pck1.dataSize() + size1); // length
        pck1.putSkip(size1);
        pck1.merge2beg();
        pck1.pipeSend(pipe, 0, pck1.dataSize(), 2);
    }

    /**
     * create open
     */
    public void createOpen() {
        msgTyp = typOpen;
        pck1.clear();
        pck2.clear();
        pck2.putByte(0, version << 5); // vers
        pck2.putByte(1, 40); // keepalive
        pck2.putByte(2, 120); // dead
        pck2.putByte(3, bits.randomB()); // sid
        pck2.putSkip(4);
        tlv.valSiz = 4;
        tlv.valTyp = 16; // stateful
        bits.msbPutD(tlv.valDat, 0, 1); // flags
        tlv.putThis(pck2);
        tlv.valSiz = 4;
        tlv.valTyp = 26; // segrout
        bits.msbPutD(tlv.valDat, 0, 16); // msd
        tlv.putThis(pck2);
        putObj(0x0110);
    }

    /**
     * create keepalive
     */
    public void createKeep() {
        msgTyp = typKeep;
        pck1.clear();
        pck2.clear();
    }

    private void createRP() {
        pck2.clear();
        int i = 0;
        if (loose) {
            i |= 0x20;
        }
        pck2.msbPutD(0, i); // flags
        pck2.msbPutD(4, reqId); // req id
        pck2.putSkip(8);
        bits.msbPutD(tlv.valDat, 0, setupType);
        tlv.valTyp = 28; // setup type
        tlv.valSiz = 4;
        tlv.putThis(pck2);
        putObj(otpRP | 2);
    }

    private void createEndpt() {
        pck2.clear();
        if (isIP4) {
            pck2.putAddr(0, srcAddr.toIPv4());
            pck2.putAddr(addrIPv4.size, trgAddr.toIPv4());
            pck2.putSkip(addrIPv4.size * 2);
            putObj(otpEndpt4);
        } else {
            pck2.putAddr(0, srcAddr.toIPv6());
            pck2.putAddr(addrIPv6.size, trgAddr.toIPv6());
            pck2.putSkip(addrIPv6.size * 2);
            putObj(otpEndpt6);
        }
    }

    private void createLsp() {
        pck2.clear();
        pck2.msbPutD(0, plspId);
        pck2.putSkip(4);
        putObj(otpLsp);
    }

    private void createLspa() {
        pck2.clear();
        pck2.msbPutD(0, exclAny);
        pck2.msbPutD(4, inclAny);
        pck2.msbPutD(8, inclAll);
        pck2.putByte(12, priSet);
        pck2.putByte(13, priHld);
        pck2.msbPutW(14, 0); // flags
        pck2.putSkip(16);
        putObj(otpLspa);
    }

    private void createBwdt() {
        pck2.clear();
        pck2.msbPutD(0, Float.floatToIntBits(bandwidth));
        pck2.putSkip(4);
        putObj(otpBwdt);
    }

    private void createMtrc() {
        pck2.clear();
        pck2.msbPutD(4, metTyp);
        pck2.msbPutD(4, metVal);
        pck2.putSkip(8);
        putObj(otpMtrc);
    }

    private void createEro() {
        if (ero == null) {
            return;
        }
        pck2.clear();
        tabHop.createHops(pck2, ero);
        putObj(otpEro);
    }

    private void createRro() {
        if (rro == null) {
            return;
        }
        pck2.clear();
        tabHop.createHops(pck2, rro);
        putObj(otpRro);
    }

    private void createNoPath() {
        pck2.clear();
        pck2.msbPutD(0, 0); // flags
        pck2.putSkip(4);
        putObj(otpNopt);
    }

    /**
     * create reply
     */
    public void createReply() {
        msgTyp = typRep;
        pck1.clear();
        pck2.clear();
        createRP();
        if (ero == null) {
            createNoPath();
            return;
        }
        createEro();
        createRro();
    }

    /**
     * create request
     */
    public void createRequest() {
        msgTyp = typReq;
        pck1.clear();
        pck2.clear();
        createRP();
        createEndpt();
        createLsp();
        createLspa();
        createBwdt();
        createMtrc();
    }

}

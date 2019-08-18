package ip;

import addr.addrIP;
import ifc.ifcDn;
import ifc.ifcEthTyp;
import ifc.ifcEther;
import ifc.ifcNshFwd;
import ifc.ifcNull;
import ifc.ifcUp;
import java.math.BigInteger;
import java.util.List;
import pack.packHolder;
import tab.tabLabel;
import tab.tabLabelBier;
import tab.tabLabelNtry;
import tab.tabNshNtry;
import tab.tabSession;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * multiprotocol label switching (rfc3032) packet
 *
 * @author matecsaba
 */
public class ipMpls implements ifcUp {

    /**
     * size of mpls header
     */
    public final static int sizeL = 4;

    /**
     * size of bier header
     */
    public final static int sizeB = 8;

    /**
     * size of bier version
     */
    public final static int bierV = 0x50;

    /**
     * mpls unicast ethertype
     */
    public final static int typeU = 0x8847;

    /**
     * mpls multicast ethertype
     */
    public final static int typeM = 0x8848;

    /**
     * ipv4 explicit null
     */
    public final static int labelExp4 = 0;

    /**
     * router alert
     */
    public final static int labelAlert = 1;

    /**
     * ipv6 explicit null
     */
    public final static int labelExp6 = 2;

    /**
     * implicit null
     */
    public final static int labelImp = 3;

    /**
     * entropy label
     */
    public final static int labelEntropy = 7;

    /**
     * generic associated label
     */
    public final static int labelGal = 13;

    /**
     * operation and maintenance
     */
    public final static int labelOam = 14;

    /**
     * extension label
     */
    public final static int labelExt = 15;

    /**
     * bier downstream label
     */
    public final static int bierLabD = 1;

    /**
     * bier upstream label
     */
    public final static int bierLabU = 2;

    /**
     * bier ethernet
     */
    public final static int bierEth = 3;

    /**
     * bier ipv4
     */
    public final static int bierIp4 = 4;

    /**
     * bier oam
     */
    public final static int bierOam = 5;

    /**
     * bier ipv6
     */
    public final static int bierIp6 = 6;

    /**
     * label security
     */
    public boolean security;

    /**
     * redirect packets
     */
    public ipMpls redirect;

    /**
     * inspector
     */
    public tabSession inspect;

    /**
     * forwarder
     */
    protected ipFwd fwd4;

    /**
     * forwarder
     */
    protected ipFwd fwd6;

    /**
     * forwarder
     */
    protected ifcEthTyp fwdE;

    private ifcDn lower = new ifcNull();

    private counter cntr = new counter();

    private ipCor core4 = new ipCor4();

    private ipCor core6 = new ipCor6();

    /**
     * create mpls handler
     *
     * @param fwdr4 ip4 forwarder
     * @param fwdr6 ip6 forwarder
     * @param ether ethertype handler
     */
    public ipMpls(ipFwd fwdr4, ipFwd fwdr6, ifcEthTyp ether) {
        fwd4 = fwdr4;
        fwd6 = fwdr6;
        fwdE = ether;
    }

    public String toString() {
        return "" + lower;
    }

    /**
     * register to ethtyp
     */
    public void register2eth() {
        fwdE.addET(typeM, "mplsMulti", this);
        fwdE.updateET(typeM, this);
        fwdE.addET(typeU, "mplsUni", this);
        fwdE.updateET(typeU, this);
    }

    /**
     * unregister from ethtyp
     */
    public void unregister2eth() {
        fwdE.delET(typeM);
        fwdE.delET(typeU);
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    public void send2eth(packHolder pck) {
        if (redirect != null) {
            redirect.cntr.tx(pck);
            redirect.lower.sendPack(pck);
            return;
        }
        if (inspect != null) {
            pck.getSkip(2);
            inspectPacket(pck, true);
            pck.getSkip(-2);
        }
        cntr.tx(pck);
        lower.sendPack(pck);
    }

    private void inspectPacket(packHolder pck, boolean dir) {
        int i = pck.dataSize();
        for (;;) {
            if (parseMPLSheader(pck)) {
                break;
            }
            if (!pck.MPLSbottom) {
                continue;
            }
            boolean b;
            switch (ifcEther.guessEtherType(pck)) {
                case ipIfc4.type:
                    b = core4.parseIPheader(pck, false);
                    break;
                case ipIfc6.type:
                    b = core6.parseIPheader(pck, false);
                    break;
                default:
                    b = true;
                    break;
            }
            if (!b) {
                inspect.doPack(pck, dir);
            }
            break;
        }
        int o = pck.dataSize();
        pck.getSkip(o - i);
    }

    /**
     * parse mpls header
     *
     * @param pck packet to parse
     * @return false if successful, true if error happened
     */
    public static boolean parseMPLSheader(packHolder pck) {
        if (pck.dataSize() < sizeL) {
            return true;
        }
        pck.MPLSlabel = pck.msbGetD(0) >>> 12;
        pck.MPLSttl = pck.getByte(3);
        int i = pck.getByte(2);
        pck.MPLSbottom = (i & 1) != 0;
        pck.MPLSexp = (i >>> 1) & 7;
        pck.getSkip(sizeL);
        if (debugger.ipMPLStrafL) {
            logger.debug("rx label=" + pck.MPLSlabel + " exp=" + pck.MPLSexp + " ttl=" + pck.MPLSttl + " bottom=" + pck.MPLSbottom);
        }
        return false;
    }

    /**
     * create mpls header
     *
     * @param pck packet to update
     */
    public static void createMPLSheader(packHolder pck) {
        pck.merge2beg();
        if (debugger.ipMPLStrafL) {
            logger.debug("tx label=" + pck.MPLSlabel + " exp=" + pck.MPLSexp + " ttl=" + pck.MPLSttl + " bottom=" + pck.MPLSbottom);
        }
        if (pck.MPLSlabel == labelImp) {
            return;
        }
        int i = pck.MPLSlabel << 12;
        i |= pck.MPLSttl & 0xff;
        i |= (pck.MPLSexp & 7) << 9;
        if (pck.MPLSbottom) {
            i |= 0x100;
        }
        pck.msbPutD(0, i);
        pck.putSkip(sizeL);
        pck.merge2beg();
        pck.MPLSbottom = false;
    }

    /**
     * create mpls labels
     *
     * @param pck packet to update
     * @param labs labels to add
     */
    public static void createMPLSlabels(packHolder pck, List<Integer> labs) {
        for (int i = labs.size() - 1; i >= 0; i--) {
            pck.MPLSlabel = labs.get(i);
            createMPLSheader(pck);
        }
    }

    /**
     * parse bier header
     *
     * @param pck packet to parse
     * @return false if successful, true if error happened
     */
    public static boolean parseBIERheader(packHolder pck) {
        if (pck.dataSize() < sizeB) {
            return true;
        }
        int i = pck.msbGetD(0);
        if ((i >>> 24) != bierV) { // nibble, version
            return true;
        }
        if (((i >>> 20) & 0x7) != pck.BIERbsl) { // bsl
            return true;
        }
        pck.MPLSrnd = i & 0xfffff; // entropy
        i = pck.msbGetD(4);
        pck.BIERid = i & 0xffff; // bfir id
        pck.IPprt = (i >>> 16) & 0x3f; // proto
        pck.IPtos = (i >>> 22) & 0x3f; // dscp
        pck.BIERoam = i >>> 28; // oam, rsv
        pck.getSkip(sizeB);
        i = tabLabelBier.bsl2num(pck.BIERbsl);
        byte[] buf = new byte[(i / 8) + 1];
        if (pck.dataSize() < buf.length) {
            return true;
        }
        buf[0] = 0;
        pck.getCopy(buf, 1, 0, buf.length - 1); // bitstring
        pck.getSkip(buf.length - 1);
        pck.BIERbs = new BigInteger(buf);
        if (debugger.ipMPLStrafB) {
            logger.debug("rx bfir=" + pck.BIERid + " si=" + pck.BIERsi + " prt=" + pck.IPprt + " bs=" + pck.BIERbs.toString(16));
        }
        return false;
    }

    /**
     * create bier header
     *
     * @param pck packet to update
     */
    public static void createBIERheader(packHolder pck) {
        pck.merge2beg();
        if (debugger.ipMPLStrafB) {
            logger.debug("tx bfir=" + pck.BIERid + " si=" + pck.BIERsi + " prt=" + pck.IPprt + " bs=" + pck.BIERbs.toString(16));
        }
        int i = pck.MPLSrnd & 0xfffff; // entropy
        i |= (pck.BIERbsl & 0x7) << 20; // bsl
        i |= bierV << 24; // nibble, version
        pck.msbPutD(0, i);
        i = pck.BIERid & 0xffff; // bfir id
        i |= (pck.IPprt & 0x3f) << 16; // proto
        i |= (pck.IPtos & 0x3f) << 22; // dscp
        i |= pck.BIERoam << 28; // oam
        pck.msbPutD(4, i);
        pck.putSkip(sizeB);
        byte[] buf = pck.BIERbs.toByteArray(); // bitstring
        i = tabLabelBier.bsl2num(pck.BIERbsl) / 8;
        if (buf.length >= i) {
            pck.putCopy(buf, buf.length - i, 0, i);
        } else {
            int o = i - buf.length;
            pck.putFill(0, o, 0);
            pck.putCopy(buf, 0, o, buf.length);
        }
        pck.putSkip(i);
        pck.merge2beg();
        pck.MPLSbottom = true;
    }

    /**
     * create error report
     *
     * @param pck packet to report
     * @param lab label to report
     * @param res reason code
     * @return false if succeed, true if error
     */
    public static boolean createError(packHolder pck, tabLabelNtry lab, counter.reasons res) {
        if (!pck.MPLSbottom) {
            return true;
        }
        if (lab.forwarder == null) {
            return true;
        }
        if (lab.duplicate != null) {
            return true;
        }
        if (lab.bier != null) {
            return true;
        }
        if (lab.pweIfc != null) {
            return true;
        }
        ipFwdIface ifc = lab.iface;
        if (ifc == null) {
            ifc = ipFwdTab.findStableIface(lab.forwarder);
            if (ifc == null) {
                return true;
            }
        }
        addrIP src = ifc.getUnreachAddr();
        if (src == null) {
            return true;
        }
        if (lab.forwarder.ipCore.parseIPheader(pck, true)) {
            return true;
        }
        if (debugger.ipFwdTraf) {
            logger.debug("drop " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " reason=" + counter.reason2string(res));
        }
        if (lab.forwarder.icmpCore.createError(pck, res, src.copyBytes())) {
            return true;
        }
        lab.forwarder.ipCore.createIPheader(pck);
        pck.INTupper = -1;
        beginMPLSfields(pck, lab.forwarder.mplsPropTtl);
        return false;
    }

    /**
     * start mpls header
     *
     * @param pck packet to update
     * @param prop propagate ip ttl
     */
    public static void beginMPLSfields(packHolder pck, boolean prop) {
        pck.MPLSbottom = true;
        pck.MPLSexp = pck.IPtos >>> 5;
        if (prop) {
            pck.MPLSttl = pck.IPttl;
        } else {
            pck.MPLSttl = 255;
        }
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
    }

    /**
     * close interface
     */
    public void closeUp() {
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        int ethTyp = pck.msbGetW(0); // ethertype
        switch (ethTyp) {
            case typeU:
            case typeM:
                break;
            default:
                cntr.drop(pck, counter.reasons.badEthTyp);
                return;
        }
        pck.getSkip(2);
        if (inspect != null) {
            inspectPacket(pck, false);
        }
        gotMplsPack(fwd4, fwd6, fwdE, security, pck);
    }

    /**
     * do one nsh packet
     *
     * @param fwdE ethernet forwarder
     * @param pck packet to read
     */
    public static void gotNshPack(ifcEthTyp fwdE, packHolder pck) {
        if (debugger.ifcNshEvnt) {
            logger.debug("fwd sp=" + pck.NSHsp + " si=" + pck.NSHsi + " prt=" + pck.IPprt + " ttl=" + pck.NSHttl + " meta=" + pck.NSHmdt + "," + pck.NSHmdv.length);
        }
        tabNshNtry ntry = new tabNshNtry(pck.NSHsp, pck.NSHsi);
        ntry = tabNshNtry.services.find(ntry);
        if (ntry == null) {
            logger.info("received invalid service " + pck.NSHsp + " " + pck.NSHsi + " on " + fwdE);
            return;
        }
        ntry.cntr.rx(pck);
        pck.NSHttl--;
        if (pck.NSHttl < 1) {
            ntry.cntr.drop(pck, counter.reasons.ttlExceed);
            return;
        }
        pck.NSHsp = ntry.trgSp;
        pck.NSHsi = ntry.trgSi;
        if (ntry.iface != null) {
            ifcNshFwd fwd = ntry.iface.nshFwd;
            if (fwd == null) {
                ntry.cntr.drop(pck, counter.reasons.notUp);
                return;
            }
            if (!ntry.rawPack) {
                fwd.doTxNsh(pck, ntry.target);
                return;
            }
            if (ifcNshFwd.convert2ethtyp(pck) < 0) {
                ntry.cntr.drop(pck, counter.reasons.badProto);
                return;
            }
            if (ntry.keepHdr) {
                fwd.doTxRaw(pck, null);
            } else {
                fwd.doTxRaw(pck, ntry.target);
            }
            return;
        }
        if ((ntry.route4 == null) || (ntry.route6 == null)) {
            ntry.cntr.drop(pck, counter.reasons.denied);
            return;
        }
        int i = ifcNshFwd.convert2ethtyp(pck);
        if (i < 0) {
            ntry.cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        pck.getSkip(2);
        ipFwd fwd = null;
        switch (i) {
            case ipIfc4.type:
                fwd = ntry.route4;
                break;
            case ipIfc6.type:
                fwd = ntry.route6;
                break;
            case ipMpls.typeU:
                gotMplsPack(ntry.route4, ntry.route6, null, false, pck);
                return;
            case ifcNshFwd.type:
                if (ifcNshFwd.parseNSHheader(pck)) {
                    ntry.cntr.drop(pck, counter.reasons.badHdr);
                    return;
                }
                if (debugger.ifcNshEvnt) {
                    logger.debug("rx sp=" + pck.NSHsp + " si=" + pck.NSHsi + " prt=" + pck.IPprt + " ttl=" + pck.NSHttl + " meta=" + pck.NSHmdt + "," + pck.NSHmdv.length);
                }
                ntry = new tabNshNtry(pck.NSHsp, pck.NSHsi);
                ntry = tabNshNtry.services.find(ntry);
                if (ntry == null) {
                    ntry.cntr.drop(pck, counter.reasons.notInTab);
                    return;
                }
                gotNshPack(fwdE, pck);
                return;
            default:
                ntry.cntr.drop(pck, counter.reasons.badProto);
                return;
        }
        beginMPLSfields(pck, false);
        fwd.mplsRxPack(ntry.route4, ntry.route6, null, fwd.commonLabel, pck);
    }

    /**
     * do one mpls packet
     *
     * @param pck packet to read
     * @param fwd4 ipv4 forwarder
     * @param fwd6 ipv6 forwarder
     * @param fwdE ethernet forwarder
     * @param secure secure configuration
     */
    public static void gotMplsPack(ipFwd fwd4, ipFwd fwd6, ifcEthTyp fwdE, boolean secure, packHolder pck) {
        for (;;) {
            if (parseMPLSheader(pck)) {
                logger.info("received invalid header on " + fwdE);
                return;
            }
            switch (pck.MPLSlabel) {
                case labelImp:
                case labelExp4:
                    if (!pck.MPLSbottom) {
                        continue;
                    }
                    if (fwd4 == null) {
                        return;
                    }
                    fwd4.mplsRxPack(fwd4, fwd6, fwdE, fwd4.commonLabel, pck);
                    return;
                case labelExp6:
                    if (!pck.MPLSbottom) {
                        continue;
                    }
                    if (fwd6 == null) {
                        return;
                    }
                    fwd6.mplsRxPack(fwd4, fwd6, fwdE, fwd6.commonLabel, pck);
                    return;
                case labelEntropy:
                    if (parseMPLSheader(pck)) {
                        return;
                    }
                    pck.MPLSrnd = pck.MPLSlabel;
                    if (!pck.MPLSbottom) {
                        continue;
                    }
                    if (fwd4 == null) {
                        return;
                    }
                    fwd4.mplsRxPack(fwd4, fwd6, fwdE, fwd4.commonLabel, pck);
                    return;
                case labelAlert:
                    continue;
                default:
                    break;
            }
            tabLabelNtry ntry = tabLabel.find(pck.MPLSlabel);
            if (ntry == null) {
                logger.info("received invalid label " + pck.MPLSlabel + " on " + fwdE);
                return;
            }
            ntry.cntr.rx(pck);
            if (ntry.forwarder == null) {
                ntry.cntr.drop(pck, counter.reasons.noRoute);
                return;
            }
            if (secure) {
                if ((ntry.forwarder != fwd4) && (ntry.forwarder != fwd6)) {
                    logger.info("received violating label " + pck.MPLSlabel + " on " + fwdE);
                    ntry.cntr.drop(pck, counter.reasons.denied);
                    return;
                }
            }
            if (ntry.nextHop != null) {
                ntry.forwarder.mplsRxPack(fwd4, fwd6, fwdE, ntry, pck);
                return;
            }
            if (pck.MPLSbottom) {
                ntry.forwarder.mplsRxPack(fwd4, fwd6, fwdE, ntry, pck);
                return;
            }
        }
    }

    /**
     * do one bier packet
     *
     * @param pck packet to read
     * @param fwd4 ipv4 forwarder
     * @param fwd6 ipv6 forwarder
     * @param fwdE ethernet forwarder
     * @return false on success, true on error
     */
    public static boolean gotBierPck(ipFwd fwd4, ipFwd fwd6, ifcEthTyp fwdE, packHolder pck) {
        switch (pck.IPprt) {
            case bierLabD:
            case bierLabU:
                gotMplsPack(fwd4, fwd6, fwdE, false, pck);
                return false;
            case bierIp4:
                if (fwd4 == null) {
                    return true;
                }
                fwd4.mplsRxPack(fwd4, fwd6, fwdE, fwd4.commonLabel, pck);
                return false;
            case bierIp6:
                if (fwd6 == null) {
                    return true;
                }
                fwd6.mplsRxPack(fwd4, fwd6, fwdE, fwd6.commonLabel, pck);
                return false;
            case bierEth:
                if (fwdE == null) {
                    return true;
                }
                ifcEther.parseETHheader(pck, false);
                fwdE.recvPack(pck);
                return false;
            default:
                return true;
        }
    }

}

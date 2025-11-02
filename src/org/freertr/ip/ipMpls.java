package org.freertr.ip;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.clnt.clntNetflow;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcNshFwd;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcPolka;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrNshIface;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabNshEntry;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabSession;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

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
     * bier ethertype
     */
    public final static int typeB = 0xab37;

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
     * metadata label indicator
     */
    public final static int labelMetaInd = 16;

    /**
     * metadata present indicator
     */
    public final static int labelMetaPrs = 17;

    /**
     * alternate marking label indicator
     */
    public final static int labelAltMark = 18;

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
     * send with this ethertype
     */
    public int ethtyp = ipMpls.typeU;

    /**
     * entropy marking
     */
    public int flow;

    /**
     * alternate marking
     */
    public int mark;

    /**
     * ingress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> filterIn;

    /**
     * egress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> filterOut;

    /**
     * common ingress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> cfilterIn;

    /**
     * common egress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> cfilterOut;

    /**
     * redirect packets
     */
    public ipMpls redirect;

    /**
     * inspector
     */
    public tabSession inspect;

    /**
     * process netflow on receiving
     */
    public boolean netflowRx = false;

    /**
     * process netflow on sending
     */
    public boolean netflowTx = false;

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
        fwdE.addET(typeB, "mplsBer", this);
        fwdE.updateET(typeB, this);
        fwdE.addET(typeM, "mplsMlt", this);
        fwdE.updateET(typeM, this);
        fwdE.addET(typeU, "mplsUni", this);
        fwdE.updateET(typeU, this);
    }

    /**
     * unregister from ethtyp
     */
    public void unregister2eth() {
        fwdE.delET(typeB);
        fwdE.delET(typeM);
        fwdE.delET(typeU);
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    public void send2eth(packHolder pck) {
        if ((flow > 0) || (mark > 0)) {
            int typ = pck.msbGetW(0);
            pck.getSkip(2);
            pck.MPLSntr = flow;
            pck.MPLSmrkV = mark;
            int old = pck.MPLSlabel;
            pck.MPLSlabel = labelImp;
            createMPLSheader(pck);
            pck.MPLSlabel = old;
            pck.msbPutW(0, typ);
            pck.putSkip(2);
            pck.merge2beg();
        }
        if (cfilterOut != null) {
            pck.getSkip(2);
            if (filterPacket(pck, cfilterOut)) {
                return;
            }
            pck.getSkip(-2);
        }
        if (filterOut != null) {
            pck.getSkip(2);
            if (filterPacket(pck, filterOut)) {
                return;
            }
            pck.getSkip(-2);
        }
        if (inspect != null) {
            pck.getSkip(2);
            if (inspectPacket(pck, true)) {
                return;
            }
            pck.getSkip(-2);
        }
        if (netflowTx) {
            pck.getSkip(2);
            if (netflowPacket(pck, true)) {
                return;
            }
            pck.getSkip(-2);
        }
        if (redirect != null) {
            redirect.cntr.tx(pck);
            redirect.lower.sendPack(pck);
            return;
        }
        cntr.tx(pck);
        lower.sendPack(pck);
    }

    private ipFwd parseIPhead(packHolder pck) {
        for (;;) {
            if (parseMPLSheader(pck)) {
                return null;
            }
            if (pck.MPLSbottom) {
                break;
            }
        }
        switch (ifcEther.guessEtherType(pck)) {
            case ipIfc4.type:
                if (core4.parseIPheader(pck, false)) {
                    return null;
                }
                return fwd4;
            case ipIfc6.type:
                if (core6.parseIPheader(pck, false)) {
                    return null;
                }
                return fwd6;
            default:
                return null;
        }
    }

    private boolean filterPacket(packHolder pck, tabListing<tabAceslstN<addrIP>, addrIP> acl) {
        int i = pck.dataSize();
        if (parseIPhead(pck) == null) {
            return true;
        }
        boolean b = !acl.matches(false, true, pck);
        int o = pck.dataSize();
        pck.getSkip(o - i);
        return b;
    }

    private boolean inspectPacket(packHolder pck, boolean dir) {
        int i = pck.dataSize();
        if (parseIPhead(pck) == null) {
            return true;
        }
        boolean b = inspect.doPack(pck, dir);
        int o = pck.dataSize();
        pck.getSkip(o - i);
        return b;
    }

    private boolean netflowPacket(packHolder pck, boolean dir) {
        int i = pck.dataSize();
        ipFwd fw = parseIPhead(pck);
        if (fw != null) {
            clntNetflow nf = fw.netflow;
            if (nf != null) {
                nf.session.doPack(pck, dir);
            }
        }
        int o = pck.dataSize();
        pck.getSkip(o - i);
        return false;
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

    private static void createMPLSheader(packHolder pck, int label, boolean bottom) {
        int i = label << 12;
        i |= pck.MPLSttl & 0xff;
        i |= (pck.MPLSexp & 7) << 9;
        if (bottom) {
            i |= 0x100;
        }
        pck.msbPutD(0, i);
        pck.putSkip(sizeL);
        pck.merge2beg();
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
        if (pck.MPLSmrkV > 0) {
            int old = pck.MPLSexp;
            pck.MPLSexp = (((int) bits.getTime() / 60000) & 1) << 2;
            pck.MPLSmrkC = pck.MPLSexp;
            createMPLSheader(pck, pck.MPLSmrkV, pck.MPLSbottom);
            createMPLSheader(pck, labelAltMark, false);
            pck.MPLSexp = old;
            pck.MPLSmrkV = 0;
            pck.MPLSbottom = false;
        }
        if (pck.MPLSntr > 0) {
            createMPLSheader(pck, pck.MPLSntr, pck.MPLSbottom);
            createMPLSheader(pck, labelEntropy, false);
            pck.MPLSntr = 0;
            pck.MPLSbottom = false;
        }
        if (pck.MPLSlabel == labelImp) {
            return;
        }
        createMPLSheader(pck, pck.MPLSlabel, pck.MPLSbottom);
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
        pck.MPLSntr = i & 0xfffff; // entropy
        i = pck.msbGetD(4);
        pck.BIERid = i & 0xffff; // bfir id
        pck.IPprt = (i >>> 16) & 0x3f; // proto
        pck.IPtos = (i >>> 22) & 0x3f; // dscp
        pck.BIERoam = i >>> 28; // oam, rsv
        pck.getSkip(sizeB);
        i = tabLabelBier.bsl2num(pck.BIERbsl);
        byte[] buf = new byte[(i / 8)];
        if (pck.dataSize() <= buf.length) {
            return true;
        }
        pck.getCopy(buf, 0, 0, buf.length); // bitstring
        pck.getSkip(buf.length);
        pck.BIERbs = buf;
        if (debugger.ipMPLStrafB) {
            logger.debug("rx bfir=" + pck.BIERid + " si=" + pck.BIERsi + " prt=" + pck.IPprt + " bs=" + bits.byteDump(pck.BIERbs, 0, -1));
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
            logger.debug("tx bfir=" + pck.BIERid + " si=" + pck.BIERsi + " prt=" + pck.IPprt + " bs=" + bits.byteDump(pck.BIERbs, 0, -1));
        }
        int i = pck.MPLSntr & 0xfffff; // entropy
        i |= (pck.BIERbsl & 0x7) << 20; // bsl
        i |= bierV << 24; // nibble, version
        pck.msbPutD(0, i);
        i = pck.BIERid & 0xffff; // bfir id
        i |= (pck.IPprt & 0x3f) << 16; // proto
        i |= (pck.IPtos & 0x3f) << 22; // dscp
        i |= pck.BIERoam << 28; // oam
        pck.msbPutD(4, i);
        pck.putSkip(sizeB);
        byte[] buf = pck.BIERbs; // bitstring
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
        pck.MPLSntr = 0;
        pck.MPLSbottom = true;
    }

    /**
     * create error report
     *
     * @param pck packet to report
     * @param lab label to report
     * @param res reason code
     * @param dat reason data
     * @return false if succeed, true if error
     */
    public static boolean createError(packHolder pck, tabLabelEntry lab, counter.reasons res, int dat) {
        ipFwd fwd = lab.forwarder;
        if (fwd == null) {
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
        if (fwd.unreach != null) {
            if (fwd.unreach.check(1)) {
                return true;
            }
        }
        ipFwdIface ifc = lab.iface;
        if (ifc == null) {
            ifc = ipFwdTab.findStableIface(fwd);
            if (ifc == null) {
                return true;
            }
        }
        List<Integer> labs = null;
        if (!pck.MPLSbottom) {
            int old = pck.MPLSlabel;
            labs = new ArrayList<Integer>();
            for (;;) {
                if (parseMPLSheader(pck)) {
                    return true;
                }
                labs.add(pck.MPLSlabel);
                if (pck.MPLSbottom) {
                    break;
                }
            }
            pck.MPLSlabel = old;
        }
        if (fwd.ipCore.parseIPheader(pck, true)) {
            return true;
        }
        if (debugger.ipFwdTraf) {
            logger.debug("drop " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " reason=" + counter.reason2string(res));
        }
        addrIP src = ifc.getUnreachAddr();
        if (src == null) {
            return true;
        }
        if (fwd.icmpCore.createError(pck, res, dat, src, fwd.mplsExtRep)) {
            return true;
        }
        fwd.ipCore.createIPheader(pck);
        pck.INTupper = -1;
        beginMPLSfields(pck, (fwd.mplsPropTtl | ifc.mplsPropTtlAlways) & ifc.mplsPropTtlAllow);
        if (labs != null) {
            createMPLSlabels(pck, labs);
        }
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
        pck.MPLSntr = 0;
        pck.MPLSmrkV = 0;
        pck.MPLSmrkC = 0;
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
            case typeB:
                break;
            default:
                cntr.drop(pck, counter.reasons.badEthTyp);
                return;
        }
        pck.getSkip(2);
        if (cfilterIn != null) {
            if (filterPacket(pck, cfilterIn)) {
                return;
            }
        }
        if (filterIn != null) {
            if (filterPacket(pck, filterIn)) {
                return;
            }
        }
        if (inspect != null) {
            if (inspectPacket(pck, false)) {
                return;
            }
        }
        if (netflowRx) {
            if (netflowPacket(pck, false)) {
                return;
            }
        }
        gotMplsPack(fwd4, fwd6, fwdE, security, pck);
    }

    /**
     * do one polka packet
     *
     * @param fwdP polka forwarder
     * @param fwd4 ipv4 forwarder
     * @param fwd6 ipv6 forwarder
     * @param pck packet to read
     */
    public static void gotPolkaPack(ifcPolka fwdP, ipFwd fwd4, ipFwd fwd6, packHolder pck) {
        int id = fwdP.decodeRouteId(pck);
        if (debugger.ifcPolkaEvnt) {
            logger.debug("fwd to=" + id + " at=" + fwdP.localId + " route=" + bits.byteDump(pck.NSHmdv, 0, -1));
        }
        pck.NSHttl--;
        if (pck.NSHttl < 1) {
            return;
        }
        if (pck.NSHmdt == 0) {
            if (id == 0) {
                pck.ETHtype = pck.IPprt;
                ipFwd fwd;
                switch (pck.IPprt) {
                    case ipIfc4.type:
                        fwd = fwd4;
                        break;
                    case ipIfc6.type:
                        fwd = fwd6;
                        break;
                    case ipMpls.typeU:
                        gotMplsPack(fwd4, fwd6, null, false, pck);
                        return;
                    default:
                        return;
                }
                beginMPLSfields(pck, false);
                if (fwd.mplsPropTtl) {
                    pck.MPLSttl = pck.NSHttl;
                }
                fwd.mplsRxPack(fwd4, fwd6, null, fwd.commonLabel, pck);
                return;
            }
            tabIndex<addrIP> idx = new tabIndex<addrIP>(id, null);
            tabIndex<addrIP> res = null;
            ipFwd fwd = null;
            if (fwd4 != null) {
                res = fwd4.actualIU.find(idx);
                fwd = fwd4;
            }
            if ((res == null) && (fwd6 != null)) {
                res = fwd6.actualIU.find(idx);
                fwd = fwd6;
            }
            if (res == null) {
                logger.info("received invalid index " + id);
                return;
            }
            res.cntr.rx(pck);
            tabRouteEntry<addrIP> ntry = fwd.actualU.find(res.prefix);
            if (ntry == null) {
                logger.info("no route for index " + id);
                return;
            }
            if (ntry.best.nextHop == null) {
                logger.info("no nexthop for index " + id);
                return;
            }
            ipFwdIface ifc = (ipFwdIface) ntry.best.iface;
            if (ifc == null) {
                logger.info("no iface for index " + id);
                return;
            }
            ifc.lower.sendPolka(pck, ntry.best.nextHop);
            return;
        }
        ipFwd fwd = null;
        if (fwd4.actualIC.size() > fwd6.actualIC.size()) {
            fwd = fwd4;
        } else {
            fwd = fwd6;
        }
        for (int i = 1; i <= fwd.actualIC.size(); i++) {
            if (i >= bits.bitVals.length) {
                continue;
            }
            if ((id & bits.bitVals[i]) == 0) {
                continue;
            }
            tabIndex<addrIP> idx = fwd.actualIC.get(i - 1);
            if (idx == null) {
                logger.info("received invalid index " + i);
                continue;
            }
            idx.cntr.rx(pck);
            tabRouteEntry<addrIP> ntry = fwd.actualU.find(idx.prefix);
            if (ntry == null) {
                logger.info("no route for index " + i);
                continue;
            }
            if (ntry.best.nextHop == null) {
                logger.info("no nexthop for index " + id);
                continue;
            }
            ipFwdIface ifc = (ipFwdIface) ntry.best.iface;
            if (ifc == null) {
                logger.info("no iface for index " + id);
                continue;
            }
            ifc.lower.sendPolka(pck.copyBytes(true, true), ntry.best.nextHop);
        }
        if ((id & 1) == 0) {
            return;
        }
        pck.ETHtype = pck.IPprt;
        switch (pck.IPprt) {
            case ipIfc4.type:
                fwd = fwd4;
                break;
            case ipIfc6.type:
                fwd = fwd6;
                break;
            case ipMpls.typeU:
                gotMplsPack(fwd4, fwd6, null, false, pck);
                return;
            default:
                return;
        }
        beginMPLSfields(pck, false);
        if (fwd.mplsPropTtl) {
            pck.MPLSttl = pck.NSHttl;
        }
        fwd.mplsRxPack(fwd4, fwd6, null, fwd.commonLabel, pck);
    }

    /**
     * do one nsh packet
     *
     * @param pck packet to read
     */
    public static void gotNshPack(packHolder pck) {
        if (debugger.ifcNshEvnt) {
            logger.debug("fwd sp=" + pck.NSHsp + " si=" + pck.NSHsi + " prt=" + pck.IPprt + " ttl=" + pck.NSHttl + " meta=" + pck.NSHmdt + "," + pck.NSHmdv.length);
        }
        tabNshEntry ntry = new tabNshEntry(pck.NSHsp, pck.NSHsi);
        ntry = tabNshEntry.services.find(ntry);
        if (ntry == null) {
            logger.info("received invalid service " + pck.NSHsp + " " + pck.NSHsi);
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
        if ((ntry.tunnelV != null) && (ntry.tunnelI != null) && (ntry.tunnelA != null)) {
            ifcNshFwd.createNSHheader(pck);
            pck.getSkip(2);
            pck.IPprt = rtrNshIface.protoNum;
            pck.IPtrg.setAddr(ntry.tunnelA);
            pck.IPsrc.setAddr(ntry.tunnelI.addr);
            pck.merge2beg();
            pck.putDefaults();
            ntry.tunnelV.protoPack(ntry.tunnelI, null, pck);
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
                tabNshEntry trg = new tabNshEntry(pck.NSHsp, pck.NSHsi);
                trg = tabNshEntry.services.find(trg);
                if (trg == null) {
                    ntry.cntr.drop(pck, counter.reasons.notInTab);
                    return;
                }
                gotNshPack(pck);
                return;
            default:
                ntry.cntr.drop(pck, counter.reasons.badProto);
                return;
        }
        beginMPLSfields(pck, false);
        if (fwd.mplsPropTtl) {
            pck.MPLSttl = pck.NSHttl;
        }
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
        int ttl = -1;
        for (;;) {
            if (parseMPLSheader(pck)) {
                logger.info("received invalid header on " + fwdE);
                return;
            }
            if (ttl >= 0) {
                pck.MPLSttl = ttl;
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
                    pck.MPLSntr = pck.MPLSlabel;
                    if (!pck.MPLSbottom) {
                        continue;
                    }
                    if (fwd4 == null) {
                        return;
                    }
                    fwd4.mplsRxPack(fwd4, fwd6, fwdE, fwd4.commonLabel, pck);
                    return;
                case labelAltMark:
                    if (parseMPLSheader(pck)) {
                        return;
                    }
                    pck.MPLSmrkC = pck.MPLSexp;
                    pck.MPLSmrkV = pck.MPLSlabel;
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
            tabLabelEntry ntry = tabLabel.find(pck.MPLSlabel);
            if (ntry == null) {
                logger.info("received invalid label " + pck.MPLSlabel + " on " + fwdE);
                return;
            }
            ntry.cntr.rx(pck);
            ipFwd fwd = ntry.forwarder;
            if (fwd == null) {
                ntry.cntr.drop(pck, counter.reasons.noRoute);
                return;
            }
            if (fwd.mplsPropTtl) {
                ttl = pck.MPLSttl;
            }
            if (secure) {
                if ((fwd != fwd4) && (fwd != fwd6)) {
                    logger.info("received violating label " + pck.MPLSlabel + " on " + fwdE);
                    ntry.cntr.drop(pck, counter.reasons.denied);
                    return;
                }
            }
            if (ntry.nextHop != null) {
                fwd.mplsRxPack(fwd4, fwd6, fwdE, ntry, pck);
                return;
            }
            if (pck.MPLSbottom) {
                fwd.mplsRxPack(fwd4, fwd6, fwdE, ntry, pck);
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

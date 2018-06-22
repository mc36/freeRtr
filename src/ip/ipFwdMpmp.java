package ip;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pack.packLdp;
import pack.packLdpMp;
import rtr.rtrLdpNeigh;
import tab.tabGen;
import tab.tabLabel;
import tab.tabLabelNtry;
import tab.tabRouteEntry;
import util.bits;

/**
 * stores one multipoint lsp
 *
 * @author matecsaba
 */
public class ipFwdMpmp implements Comparator<ipFwdMpmp> {

    /**
     * multipoint to multipoint
     */
    public boolean mp2mp;

    /**
     * local endpoint
     */
    public boolean local;

    /**
     * root node address
     */
    public addrIP root;

    /**
     * uplink neighbor
     */
    public addrIP uplnk;

    /**
     * opaque value
     */
    public byte[] opaque;

    /**
     * self root
     */
    public boolean selfRoot;

    /**
     * vrf of uplink
     */
    public ipFwd vrfUpl;

    /**
     * receiver vrf
     */
    public ipFwd vrfRx;

    /**
     * neighbors
     */
    public final tabGen<ipFwdMpmpNeigh> neighs = new tabGen<ipFwdMpmpNeigh>();

    /**
     * create new instance
     *
     * @param mp true=multipoint2multipoint, false=point2multipoint
     * @param rot root address
     * @param opq opaque value
     */
    public ipFwdMpmp(boolean mp, addrIP rot, byte[] opq) {
        mp2mp = mp;
        root = rot.copyBytes();
        opaque = new byte[opq.length];
        bits.byteCopy(opq, 0, opaque, 0, opaque.length);
    }

    public String toString() {
        return mp2mp + " " + root + " " + bits.byteDump(opaque, 0, -1);
    }

    /**
     * create for tunnel
     *
     * @param mp true=multipoint2multipoint, false=point2multipoint
     * @param rot root address
     * @param id tunnel id
     * @return instance
     */
    public static ipFwdMpmp create4tunnel(boolean mp, addrIP rot, int id) {
        byte[] op = new byte[7];
        op[0] = 1; // generic lsp id
        bits.msbPutW(op, 1, 4);
        bits.msbPutD(op, 3, id);
        return new ipFwdMpmp(mp, rot, op);
    }

    /**
     * create for multicast group
     *
     * @param mp true=multipoint2multipoint, false=point2multipoint
     * @param rot root address
     * @param mcst multicast group
     * @return instance
     */
    public static ipFwdMpmp create4multicast(boolean mp, addrIP rot, ipFwdMcast mcst) {
        byte[] op;
        if (mcst.group.isIPv4()) {
            op = new byte[11];
            op[0] = 3; // ipv4 transit
            bits.msbPutW(op, 1, 8);
            mcst.source.toIPv4().toBuffer(op, 3);
            mcst.group.toIPv4().toBuffer(op, 7);
        } else {
            op = new byte[35];
            op[0] = 4; // ipv6 transit
            bits.msbPutW(op, 1, 32);
            mcst.source.toIPv6().toBuffer(op, 3);
            mcst.group.toIPv6().toBuffer(op, 19);
        }
        return new ipFwdMpmp(mp, rot, op);
    }

    /**
     * create for multicast vpn group
     *
     * @param mp true=multipoint2multipoint, false=point2multipoint
     * @param rot root address
     * @param rd route distinguisher
     * @param mcst multicast group
     * @return instance
     */
    public static ipFwdMpmp create4vpnMcast(boolean mp, addrIP rot, long rd, ipFwdMcast mcst) {
        byte[] op;
        if (mcst.group.isIPv4()) {
            op = new byte[19];
            op[0] = (byte) 250; // vpnv4 transit
            bits.msbPutW(op, 1, 16);
            mcst.source.toIPv4().toBuffer(op, 3);
            mcst.group.toIPv4().toBuffer(op, 7);
            bits.msbPutQ(op, 11, rd);
        } else {
            op = new byte[43];
            op[0] = (byte) 251; // vpnv6 transit
            bits.msbPutW(op, 1, 40);
            mcst.source.toIPv6().toBuffer(op, 3);
            mcst.group.toIPv6().toBuffer(op, 19);
            bits.msbPutQ(op, 35, rd);
        }
        return new ipFwdMpmp(mp, rot, op);
    }

    /**
     * create for recursive
     *
     * @param mp true=multipoint2multipoint, false=point2multipoint
     * @param rot root address
     * @param orig original lsp
     * @return instance
     */
    public static ipFwdMpmp create4recursive(boolean mp, addrIP rot, packLdpMp orig) {
        packHolder pck = new packHolder(true, true);
        orig.createFEC(pck);
        pck.merge2end();
        pck.putByte(0, 7); // recursive
        pck.msbPutW(1, pck.dataSize()); // size
        pck.putSkip(3);
        pck.merge2beg();
        return new ipFwdMpmp(mp, rot, pck.getCopy());
    }

    /**
     * create for vpn recursive
     *
     * @param mp true=multipoint2multipoint, false=point2multipoint
     * @param rot root address
     * @param rd route distinguisher
     * @param orig original lsp
     * @return instance
     */
    public static ipFwdMpmp create4vpnRecursive(boolean mp, addrIP rot, long rd, packLdpMp orig) {
        packHolder pck = new packHolder(true, true);
        orig.createFEC(pck);
        pck.merge2end();
        pck.putByte(0, 8); // vpn recursive
        pck.msbPutW(1, pck.dataSize() + 8); // size
        pck.msbPutQ(3, rd);
        pck.putSkip(11);
        pck.merge2beg();
        return new ipFwdMpmp(mp, rot, pck.getCopy());
    }

    /**
     * decode for multicast group
     *
     * @param ntry entry to decode
     * @return multicast group, null if error happened
     */
    public static ipFwdMcast decode4multicast(ipFwdMpmp ntry) {
        if (ntry.mp2mp) {
            return null;
        }
        if (ntry.opaque.length < 3) {
            return null;
        }
        int siz = bits.msbGetW(ntry.opaque, 1); // size
        if (siz > (ntry.opaque.length - 3)) {
            return null;
        }
        addrIP src = new addrIP();
        addrIP grp = new addrIP();
        long rd = 0;
        switch (ntry.opaque[0] & 0xff) {
            case 3: // ipv4 transit
                if (siz < 8) {
                    return null;
                }
                addrIPv4 a4 = new addrIPv4();
                a4.fromBuf(ntry.opaque, 3);
                src.fromIPv4addr(a4);
                a4.fromBuf(ntry.opaque, 7);
                grp.fromIPv4addr(a4);
                break;
            case 4: // ipv6 transit
                if (siz < 32) {
                    return null;
                }
                addrIPv6 a6 = new addrIPv6();
                a6.fromBuf(ntry.opaque, 3);
                src.fromIPv6addr(a6);
                a6.fromBuf(ntry.opaque, 19);
                grp.fromIPv6addr(a6);
                break;
            case 250: // vpnv4 transit
                if (siz < 16) {
                    return null;
                }
                a4 = new addrIPv4();
                a4.fromBuf(ntry.opaque, 3);
                src.fromIPv4addr(a4);
                a4.fromBuf(ntry.opaque, 7);
                grp.fromIPv4addr(a4);
                rd = bits.msbGetQ(ntry.opaque, 11);
                break;
            case 251: // vpnv6 transit
                if (siz < 40) {
                    return null;
                }
                a6 = new addrIPv6();
                a6.fromBuf(ntry.opaque, 3);
                src.fromIPv6addr(a6);
                a6.fromBuf(ntry.opaque, 19);
                grp.fromIPv6addr(a6);
                rd = bits.msbGetQ(ntry.opaque, 35);
                break;
            default:
                return null;
        }
        ipFwdMcast d = new ipFwdMcast(grp, src);
        d.rd = rd;
        return d;
    }

    /**
     * dump this entry
     *
     * @return string
     */
    public String dump() {
        String a;
        if (local) {
            a = "local";
        } else {
            a = "";
        }
        for (int i = 0; i < neighs.size(); i++) {
            ipFwdMpmpNeigh ntry = neighs.get(i);
            a += " " + ntry.labelL + "/" + ntry.addr + "/" + ntry.labelR;
        }
        return (mp2mp ? "mp2mp" : "p2mp") + "|" + root + "|" + bits.byteDump(opaque, 0, -1) + "|" + uplnk + "|" + a.trim();
    }

    public int compare(ipFwdMpmp o1, ipFwdMpmp o2) {
        if (o1.mp2mp != o2.mp2mp) {
            if (o1.mp2mp) {
                return -1;
            } else {
                return -2;
            }
        }
        int i = o1.root.compare(o1.root, o2.root);
        if (i != 0) {
            return i;
        }
        if (o1.opaque.length < o2.opaque.length) {
            return -1;
        }
        if (o1.opaque.length > o2.opaque.length) {
            return +1;
        }
        return bits.byteComp(o1.opaque, 0, o2.opaque, 0, o1.opaque.length);
    }

    /**
     * stop labels
     */
    public void stopLabels() {
        for (int i = 0; i < neighs.size(); i++) {
            ipFwdMpmpNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.labelL == null) {
                continue;
            }
            tabLabel.release(ntry.labelL, 5);
        }
    }

    /**
     * add one peer
     *
     * @param adr address
     * @param ifc interface
     * @param label remote label
     * @param alloc allocate local label
     */
    public void addPeer(addrIP adr, ipFwdIface ifc, int label, boolean alloc) {
        ipFwdMpmpNeigh ntry = new ipFwdMpmpNeigh();
        ntry.addr = adr.copyBytes();
        ntry.iface = ifc;
        ipFwdMpmpNeigh old = neighs.add(ntry);
        if (old != null) {
            ntry = old;
        }
        if (alloc && (ntry.labelL == null)) {
            ntry.labelL = tabLabel.allocate(5);
        }
        ntry.labelR = label;
    }

    /**
     * delete one peer
     *
     * @param adr address
     */
    public void delPeer(addrIP adr) {
        ipFwdMpmpNeigh ntry = new ipFwdMpmpNeigh();
        ntry.addr = adr.copyBytes();
        ntry = neighs.del(ntry);
        if (ntry == null) {
            return;
        }
        if (ntry.labelL == null) {
            return;
        }
        tabLabel.release(ntry.labelL, 5);
    }

    private int getTyp() {
        if (mp2mp) {
            return packLdp.fecTmp2mpDn;
        } else {
            return packLdp.fecTp2mp;
        }
    }

    /**
     * get fec
     *
     * @return fec
     */
    public packLdpMp getFec() {
        packLdpMp res = new packLdpMp();
        res.typ = getTyp();
        res.root = root.copyBytes();
        res.opaque = new byte[opaque.length];
        bits.byteCopy(opaque, 0, res.opaque, 0, opaque.length);
        return res;
    }

    /**
     * get reverse fec
     *
     * @param adr address to use
     * @param typ type to reverse
     * @return reverse fec
     */
    public packLdpMp getReverse(addrIP adr, int typ) {
        ipFwdMpmpNeigh ntry = new ipFwdMpmpNeigh();
        ntry.addr = adr.copyBytes();
        ntry = neighs.find(ntry);
        if (ntry == null) {
            return null;
        }
        if (ntry.labelL == null) {
            return null;
        }
        packLdpMp res = getFec();
        res.typ = packLdpMp.getReverse(typ);
        res.label = ntry.labelL.getValue();
        return res;
    }

    private addrIP getRootNeigh(ipFwd fwd) {
        tabRouteEntry<addrIP> rou = fwd.actualU.route(root);
        if (rou == null) {
            return null;
        }
        if (rou.iface == null) {
            return null;
        }
        ipFwdIface ifc = (ipFwdIface) rou.iface;
        selfRoot = false;
        switch (rou.rouTyp) {
            case local:
                selfRoot = true;
                return null;
            case conn:
                if (ifc.lower.checkMyAddress(root)) {
                    selfRoot = true;
                    return null;
                }
                return root;
            case remote:
            case defpref:
            case automesh:
                return root;
            default:
                return rou.nextHop;
        }
    }

    private void doDel(ipFwd fwd, addrIP adr) {
        rtrLdpNeigh nei = fwd.ldpNeighFind(null, adr, false);
        delPeer(adr);
        if (nei == null) {
            return;
        }
        packLdpMp fec = getFec();
        nei.sendLabelWdrw(fec);
        nei.pmpAdvert.del(fec);
    }

    private void doRootNeigh(ipFwd fwd) {
        addrIP adr = getRootNeigh(fwd);
        if (adr == null) {
            if (uplnk == null) {
                return;
            }
            doDel(fwd, uplnk);
            uplnk = null;
            return;
        }
        if (uplnk != null) {
            if (uplnk.compare(uplnk, adr) == 0) {
                return;
            }
            doDel(fwd, uplnk);
            uplnk = null;
        }
        rtrLdpNeigh nei = fwd.ldpNeighFind(null, adr, false);
        if (nei == null) {
            return;
        }
        if (nei.conn == null) {
            return;
        }
        addPeer(adr, nei.ifc, -1, true);
        packLdpMp fec = getReverse(adr, 0);
        if (fec == null) {
            return;
        }
        uplnk = adr;
        fec.typ = getTyp();
        nei.pmpAdvert.add(fec);
        nei.sendLabelMap(fec);
    }

    /**
     * update forwarding state
     *
     * @param fwd forwarder
     */
    public void updateState(ipFwd fwd) {
        for (int i = neighs.size() - 1; i >= 0; i--) {
            ipFwdMpmpNeigh ntry = neighs.get(i);
            if (fwd.ldpNeighFind(null, ntry.addr, false) != null) {
                continue;
            }
            neighs.del(ntry);
        }
        if (local) {
            doRootNeigh(fwd);
        } else {
            if (neighs.size() < 1) {
                return;
            }
            doRootNeigh(fwd);
            int i = neighs.size();
            if (uplnk != null) {
                i--;
            }
            if (selfRoot) {
                i++;
            }
            if (i < 1) {
                doDel(fwd, uplnk);
                uplnk = null;
                return;
            }
        }
        boolean nedLoc = false;
        if (mp2mp) {
            nedLoc = local;
        } else {
            nedLoc = local && (!selfRoot);
        }
        boolean ned = false;
        for (int o = 0; o < neighs.size(); o++) {
            ipFwdMpmpNeigh curr = neighs.get(o);
            if (curr.labelL == null) {
                continue;
            }
            if (nedLoc) {
                if (vrfRx == null) {
                    curr.labelL.setFwdCommon(5, fwd);
                } else {
                    curr.labelL.setFwdCommon(5, vrfRx);
                }
                ned = true;
            }
            for (int i = 0; i < neighs.size(); i++) {
                if (i == o) {
                    continue;
                }
                ipFwdMpmpNeigh ntry = neighs.get(i);
                if (ntry.labelR < 0) {
                    continue;
                }
                List<Integer> labs = tabLabel.int2labels(ntry.labelR);
                curr.labelL.setDupMpls(5, fwd, ntry.iface, ntry.addr, labs);
                ned = true;
            }
            if (ned) {
                continue;
            }
            curr.labelL.setFwdDrop(5);
        }
    }

    /**
     * send local packet
     *
     * @param fwdCor forwarder
     * @param orig packet to send
     */
    public void sendPack(ipFwd fwdCor, packHolder orig) {
        if ((!mp2mp) && (!selfRoot)) {
            return;
        }
        if (vrfUpl != null) {
            fwdCor = vrfUpl;
        }
        for (int i = 0; i < neighs.size(); i++) {
            ipFwdMpmpNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.labelR < 0) {
                continue;
            }
            packHolder pck = orig.copyBytes(true, true);
            pck.MPLSlabel = ntry.labelR;
            ipMpls.createMPLSheader(pck);
            fwdCor.mplsTxPack(ntry.addr, pck, false);
        }
    }

}

class ipFwdMpmpNeigh implements Comparator<ipFwdMpmpNeigh> {

    public addrIP addr;

    public ipFwdIface iface;

    public int labelR;

    public tabLabelNtry labelL;

    public int compare(ipFwdMpmpNeigh o1, ipFwdMpmpNeigh o2) {
        return o1.addr.compare(o1.addr, o2.addr);
    }

}

package org.freertr.ip;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.ifc.ifcBridge;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabLabelBierN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;

/**
 * stores one bier lsp
 *
 * @author matecsaba
 */
public class ipFwdBier {

    /**
     * bfr id
     */
    public final int srcId;

    /**
     * forwarder
     */
    public final ipFwd fwd;

    /**
     * list of peer
     */
    public tabGen<ipFwdBierPeer> peers = new tabGen<ipFwdBierPeer>();

    /**
     * current forwarders
     */
    public tabGen<tabLabelBierN> fwds = new tabGen<tabLabelBierN>();

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * create new instance
     *
     * @param f forwarder
     * @param id source id
     */
    public ipFwdBier(ipFwd f, int id) {
        fwd = f;
        srcId = id;
    }

    /**
     * copy bytes
     *
     * @return copy of record
     */
    public ipFwdBier copyBytes() {
        ipFwdBier n = new ipFwdBier(fwd, srcId);
        for (int i = 0; i < peers.size(); i++) {
            ipFwdBierPeer ntry = peers.get(i);
            if (ntry == null) {
                continue;
            }
            n.peers.add(new ipFwdBierPeer(ntry.addr));
        }
        for (int i = 0; i < fwds.size(); i++) {
            tabLabelBierN ntry = fwds.get(i);
            if (ntry == null) {
                continue;
            }
            n.fwds.add(ntry.copyBytes());
        }
        return n;
    }

    /**
     * compare this entry
     *
     * @param o other
     * @return false if equals, true if differs
     */
    public boolean differs(ipFwdBier o) {
        if (o == null) {
            return true;
        }
        if (peers.size() != o.peers.size()) {
            return true;
        }
        for (int i = 0; i < peers.size(); i++) {
            if (o.peers.find(peers.get(i)) == null) {
                return true;
            }
        }
        if (fwds.size() != o.fwds.size()) {
            return true;
        }
        for (int i = 0; i < fwds.size(); i++) {
            if (fwds.get(i).differs(o.fwds.get(i))) {
                return true;
            }
        }
        return false;
    }

    /**
     * send packet
     *
     * @param orig packet
     */
    public void sendPack(packHolder orig) {
        cntr.tx(orig);
        int prt;
        switch (orig.ETHtype) {
            case ipMpls.typeU:
            case ipMpls.typeM:
            case ipMpls.typeB:
                prt = ipMpls.bierLabD;
                break;
            case ipIfc4.type:
                prt = ipMpls.bierIp4;
                break;
            case ipIfc6.type:
                prt = ipMpls.bierIp6;
                break;
            case ifcBridge.serialType:
                prt = ipMpls.bierEth;
                break;
            default:
                return;
        }
        orig.IPprt = prt;
        orig.BIERid = srcId;
        orig.BIERoam = 0;
        for (int i = 0; i < fwds.size(); i++) {
            tabLabelBierN trg = fwds.get(i);
            if (trg == null) {
                continue;
            }
            int sft = tabLabelBier.bsl2num(trg.bsl);
            for (int o = 0;; o++) {
                byte[] ned = trg.getAndShr(tabLabelBier.bsl2msk(trg.bsl), sft * o);
                if (ned == null) {
                    break;
                }
                if (ned.length < 1) {
                    continue;
                }
                packHolder pck = orig.copyBytes(true, true);
                pck.BIERsi = o;
                pck.BIERbsl = trg.bsl;
                pck.BIERbs = ned;
                ipMpls.createBIERheader(pck);
                pck.MPLSlabel = trg.label + o;
                ipMpls.createMPLSheader(pck);
                fwd.mplsTxPack(trg.hop, pck, false);
            }
        }
    }

    /**
     * list peers
     *
     * @return list
     */
    public String listPeers() {
        List<tabLabelBierN> f = new ArrayList<tabLabelBierN>();
        for (int i = 0; i < fwds.size(); i++) {
            f.add(fwds.get(i));
        }
        String a = "";
        for (int i = 0; i < peers.size(); i++) {
            ipFwdBierPeer ntry = peers.get(i);
            a += " " + ntry;
            if (ntry.via == null) {
                a += ",-1";
            } else {
                a += "," + f.indexOf(ntry.via);
            }
        }
        return a.trim();
    }

    /**
     * list forwarders
     *
     * @return list
     */
    public String listFwds() {
        String a = "";
        for (int i = 0; i < fwds.size(); i++) {
            a += " " + fwds.get(i);
        }
        return a.trim();
    }

    /**
     * get details
     *
     * @param res result
     */
    public void getDump(userFormat res) {
        for (int i = 0; i < peers.size(); i++) {
            res.add("bier peer|" + peers.get(i));
        }
        for (int i = 0; i < fwds.size(); i++) {
            res.add("bier fwd|" + fwds.get(i));
        }
    }

    /**
     * add one peer
     *
     * @param adr address
     * @param exp expiration time, negative if not expires
     */
    public void addPeer(addrIP adr, long exp) {
        if (exp > 0) {
            exp += bits.getTime();
        }
        ipFwdBierPeer ntry = new ipFwdBierPeer(adr);
        ipFwdBierPeer old = peers.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.expires = exp;
    }

    /**
     * delete one peer
     *
     * @param adr address
     */
    public void delPeer(addrIP adr) {
        ipFwdBierPeer ntry = new ipFwdBierPeer(adr);
        ntry = peers.del(ntry);
        if (ntry == null) {
            return;
        }
    }

    /**
     * update peer list
     */
    public synchronized void updatePeers() {
        tabGen<tabLabelBierN> trgs = new tabGen<tabLabelBierN>();
        for (int i = 0; i < peers.size(); i++) {
            ipFwdBierPeer trg = peers.get(i);
            if (trg == null) {
                continue;
            }
            trg.bit = 0;
            trg.via = null;
            tabRouteEntry<addrIP> rou = fwd.actualU.route(trg.addr);
            if (rou == null) {
                continue;
            }
            rou = rou.copyBytes(tabRoute.addType.lnkAlters);
            if (rou.best.oldHop != null) {
                rou = fwd.actualU.route(rou.best.oldHop);
                if (rou == null) {
                    continue;
                }
                rou = rou.copyBytes(tabRoute.addType.lnkAlters);
            }
            if (rou.best.bierIdx < 1) {
                continue;
            }
            if (rou.best.bierBeg < 1) {
                continue;
            }
            tabLabelBierN ntry = new tabLabelBierN(rou.best.iface, rou.best.nextHop, rou.best.bierBeg);
            tabLabelBierN old = trgs.add(ntry);
            if (old != null) {
                ntry = old;
            }
            ntry.bsl = rou.best.bierHdr;
            ntry.setBit(rou.best.bierIdx - 1);
            trg.bit = rou.best.bierIdx;
            trg.via = ntry;
        }
        fwds = trgs;
    }

    /**
     * purge peers
     *
     * @param tim current time
     * @return number of peers remained
     */
    public int purgePeers(long tim) {
        for (int i = peers.size(); i >= 0; i--) {
            ipFwdBierPeer ntry = peers.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.expires < 0) {
                continue;
            }
            if (ntry.expires > tim) {
                continue;
            }
            peers.del(ntry);
        }
        return peers.size();
    }

}

class ipFwdBierPeer implements Comparator<ipFwdBierPeer> {

    public addrIP addr;

    public long expires;

    public int bit;

    public tabLabelBierN via;

    public int compare(ipFwdBierPeer o1, ipFwdBierPeer o2) {
        return o1.addr.compare(o1.addr, o2.addr);
    }

    public ipFwdBierPeer(addrIP adr) {
        addr = adr.copyBytes();
    }

    public String toString() {
        return addr + "," + bit + "," + via;
    }

}

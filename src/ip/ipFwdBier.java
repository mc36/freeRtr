package ip;

import addr.addrIP;
import ifc.ifcBridge;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import tab.tabGen;
import tab.tabLabelBier;
import tab.tabLabelBierN;
import tab.tabRouteEntry;
import util.bits;

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

    private tabGen<ipFwdBierPeer> peers = new tabGen<ipFwdBierPeer>();

    private tabGen<tabLabelBierN> fwdDups = new tabGen<tabLabelBierN>();

    private List<byte[]> fwdMsks = new ArrayList<byte[]>();

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
     * send packet
     *
     * @param orig packet
     */
    public void sendPack(packHolder orig) {
        int prt;
        switch (orig.ETHtype) {
            case ipMpls.typeU:
            case ipMpls.typeM:
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
        tabGen<tabLabelBierN> trgs = fwdDups;
        List<byte[]> msks = fwdMsks;
        for (int i = 0; i < trgs.size(); i++) {
            tabLabelBierN trg = trgs.get(i);
            int sft = tabLabelBier.bsl2num(trg.len);
            for (int o = 0;; o++) {
                byte[] ned = trg.getAndShr(msks.get(i), sft * o);
                if (ned == null) {
                    break;
                }
                if (ned.length < 1) {
                    continue;
                }
                packHolder pck = orig.copyBytes(true, true);
                pck.BIERsi = o;
                pck.BIERbsl = trg.len;
                pck.BIERbs = ned;
                ipMpls.createBIERheader(pck);
                pck.MPLSlabel = trg.lab + o;
                ipMpls.createMPLSheader(pck);
                fwd.mplsTxPack(trg.hop, pck, false);
            }
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
        ipFwdBierPeer ntry = new ipFwdBierPeer();
        ntry.addr = adr.copyBytes();
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
        ipFwdBierPeer ntry = new ipFwdBierPeer();
        ntry.addr = adr;
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
            addrIP trg = peers.get(i).addr;
            if (trg == null) {
                continue;
            }
            tabRouteEntry<addrIP> rou = fwd.actualU.route(trg);
            if (rou == null) {
                continue;
            }
            if (rou.best.oldHop != null) {
                rou = fwd.actualU.route(rou.best.oldHop);
                if (rou == null) {
                    continue;
                }
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
            ntry.len = rou.best.bierHdr;
            ntry.setBit(rou.best.bierIdx - 1);
        }
        List<byte[]> msks = new ArrayList<byte[]>();
        for (int i = 0; i < trgs.size(); i++) {
            tabLabelBierN ntry = trgs.get(i);
            msks.add(tabLabelBier.bsl2msk(ntry.len));
        }
        fwdDups = trgs;
        fwdMsks = msks;
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

    public int compare(ipFwdBierPeer o1, ipFwdBierPeer o2) {
        return o1.addr.compare(o1.addr, o2.addr);
    }

}

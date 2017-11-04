package ip;

import addr.addrIP;
import clnt.clntMplsBier;
import java.util.Comparator;
import pack.packHolder;
import tab.tabGen;
import util.bits;

/**
 * stores one bier lsp
 *
 * @author matecsaba
 */
public class ipFwdBier {

    /**
     * forwarder
     */
    public ipFwd fwd;

    /**
     * bfr id
     */
    public int id;

    /**
     * ethertype
     */
    public int typ;

    private tabGen<ipFwdBierPeer> peers = new tabGen<ipFwdBierPeer>();

    private clntMplsBier clnt = new clntMplsBier();

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    public void sendPack(packHolder pck) {
        pck.msbPutW(0, typ);
        pck.putSkip(2);
        pck.merge2beg();
        clnt.sendPack(pck);
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
        } else {
            clnt.addTarget(ntry.addr);
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
        clnt.delTarget(adr);
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
            clnt.delTarget(ntry.addr);
        }
        return peers.size();
    }

    /**
     * start work
     */
    public void workStart() {
        clnt.fwdCor = fwd;
        clnt.srcId = id;
        clnt.workStart();
    }

    /**
     * stop work
     */
    public void workStop() {
        clnt.workStop();
    }

}

class ipFwdBierPeer implements Comparator<ipFwdBierPeer> {

    public addrIP addr;

    public long expires;

    public int compare(ipFwdBierPeer o1, ipFwdBierPeer o2) {
        return o1.addr.compare(o1.addr, o2.addr);
    }

}

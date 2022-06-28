package net.freertr.serv;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packAnyconn;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * http anyconnect connection
 *
 * @author matecsaba
 */
public class servHttpAnyconn implements Runnable, ifcDn {

    /**
     * cloned interface
     */
    protected cfgIfc ifc = null;

    private counter cntr = new counter();

    private pipeSide pipe;

    private ifcUp upper = new ifcNull();

    private servHttpConn lower;

    /**
     * create instance
     *
     * @param conn connection
     * @param parent lower layer
     */
    public servHttpAnyconn(pipeSide conn, servHttpConn parent) {
        lower = parent;
        pipe = conn;
    }

    /**
     * start work
     */
    public void doStart() {
        new Thread(this).start();
    }

    public void run() {
        if (ifc.ip4polA != null) {
            ifc.addr4changed(ifc.addr4, ifc.mask4, ifc.ip4polA);
        }
        if (ifc.ip6polA != null) {
            ifc.addr6changed(ifc.addr6, ifc.mask6, ifc.ip6polA);
        }
        try {
            for (;;) {
                if (doRound()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        ifc.cloneStop();
    }

    private boolean doRound() {
        packAnyconn pckS = new packAnyconn(pipe);
        packHolder pckB = new packHolder(true, true);
        if (pckS.recvPack(pckB)) {
            return true;
        }
        switch (pckS.msgTyp) {
            case packAnyconn.typData:
                int i = ifcEther.guessEtherType(pckB);
                if (i < 0) {
                    logger.info("got bad protocol from " + lower.peer);
                    break;
                }
                pckB.msbPutW(0, i); // ethertype
                pckB.putSkip(2);
                pckB.merge2beg();
                upper.recvPack(pckB);
                break;
            case packAnyconn.typDpdReq:
                pckS.msgTyp = packAnyconn.typDpdRep;
                pckS.sendPack(pckB);
                break;
            case packAnyconn.typDpdRep:
                break;
            case packAnyconn.typKeep:
                break;
            case packAnyconn.typTerm:
                return true;
            case packAnyconn.typDisc:
                return true;
            default:
                logger.info("unknown type: " + pckS.msgTyp);
                break;
        }
        return false;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (ifcEther.stripEtherType(pck)) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        packAnyconn ps = new packAnyconn(pipe);
        ps.msgTyp = packAnyconn.typData;
        pck.putDefaults();
        ps.sendPack(pck);
    }

    public counter getCounter() {
        return cntr;
    }

    public void closeDn() {
        pipe.setClose();
    }

    public void flapped() {
        pipe.setClose();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public int getMTUsize() {
        return 1504;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public String toString() {
        return "anyconn";
    }

}

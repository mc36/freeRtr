package org.freertr.prt;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * handle mgre (rfc6037) packets
 *
 * @author matecsaba
 */
public class prtMgre implements ipPrt, ifcDn {

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * sending df value, -1 means maps out
     */
    public int sendingDFN = -1;

    /**
     * sending flow value, -1 means maps out
     */
    public int sendingFLW = -1;

    /**
     * forwarder
     */
    public ipFwd fwdCor;

    /**
     * source interface
     */
    public ipFwdIface fwdIfc = null;

    /**
     * target
     */
    public addrIP target;

    private addrIP[] targets = new addrIP[0];

    private ifcUp upper = new ifcNull();

    private counter cntr = new counter();

    /**
     * initialize context s
     */
    public prtMgre() {
    }

    /**
     * set targets
     *
     * @param s targets
     */
    public void setTargets(String s) {
        if (s == null) {
            s = "";
        }
        List<addrIP> trgs = new ArrayList<addrIP>();
        cmds c = new cmds("adrs", s);
        for (;;) {
            s = c.word();
            if (s.length() < 1) {
                break;
            }
            addrIP a = new addrIP();
            if (a.fromString(s)) {
                continue;
            }
            trgs.add(a);
        }
        setTargets(trgs);
    }

    /**
     * get targets
     *
     * @return targets
     */
    public String getTargets() {
        if (targets.length < 1) {
            return null;
        }
        String s = "";
        for (int i = 0; i < targets.length; i++) {
            s += " " + targets[i];
        }
        return s.trim();
    }

    /**
     * set targets
     *
     * @param trg targets
     */
    public void setTargets(List<addrIP> trg) {
        clearState();
        addrIP[] ts = new addrIP[trg.size()];
        for (int i = 0; i < ts.length; i++) {
            ts[i] = trg.get(i).copyBytes();
        }
        targets = ts;
        for (int i = 0; i < targets.length; i++) {
            fwdCor.mcastAddFloodIfc(target, targets[i], null, -1);
            fwdCor.protoAdd(this, null, targets[i]);
        }
    }

    private void clearState() {
        for (int i = 0; i < targets.length; i++) {
            fwdCor.mcastDelFloodIfc(target, targets[i], null);
            fwdCor.protoDel(this, null, targets[i]);
        }
        targets = new addrIP[0];
    }

    public counter getCounter() {
        return cntr;
    }

    public int getProtoNum() {
        return prtGre.protoNum;
    }

    public void closeUp(ipFwdIface iface) {
        upper.closeUp();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void setState(ipFwdIface iface, state.states stat) {
        if (iface.ifwNum != fwdIfc.ifwNum) {
            return;
        }
        upper.setState(stat);
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public void closeDn() {
        clearState();
        fwdCor.protoDel(this, fwdIfc, target);
    }

    public void flapped() {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void recvPack(ipFwdIface rxIface, packHolder pck) {
        cntr.rx(pck);
        if (debugger.prtGreTraf) {
            logger.debug("rx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt);
        }
        if (pck.IPprt != prtGre.protoNum) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (pck.IPtrg.compareTo(target) != 0) {
            cntr.drop(pck, counter.reasons.badTrgAddr);
            return;
        }
        int hdr = pck.msbGetW(0); // header
        int typ = pck.msbGetW(2); // ethertype
        pck.getSkip(prtGre.size);
        if (hdr != 0) {
            logger.info("got bad version from " + pck.IPsrc);
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        pck.putStart();
        pck.msbPutW(0, typ);
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
    }

    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        int typ = pck.msbGetW(0);
        pck.getSkip(2);
        if (debugger.prtGreTraf) {
            logger.debug("tx typ=" + typ);
        }
        pck.msbPutW(0, 0); // header
        pck.msbPutW(2, typ); // ethertype
        pck.putSkip(prtGre.size);
        pck.merge2beg();
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        if (sendingDFN >= 0) {
            pck.IPdf = sendingDFN == 1;
        }
        if (sendingFLW >= 0) {
            pck.IPid = sendingFLW;
        }
        pck.IPprt = prtGre.protoNum;
        pck.IPsrc.setAddr(fwdIfc.addr);
        pck.IPtrg.setAddr(target);
        fwdCor.protoPack(fwdIfc, null, pck);
    }

    public String toString() {
        return "mgre to " + target;
    }

    public int getMTUsize() {
        return fwdIfc.mtu - prtGre.size;
    }

    public long getBandwidth() {
        return fwdIfc.bandwidth;
    }

    /**
     * get remote address
     *
     * @return peer address, null if no session
     */
    public addrIP getAddrRem() {
        if (targets.length < 1) {
            return null;
        }
        return targets[0];
    }

    /**
     * get group address
     *
     * @return peer address, null if no session
     */
    public addrIP getAddrGrp() {
        return target;
    }

    /**
     * get local address
     *
     * @return peer address, null if no session
     */
    public addrIP getAddrLoc() {
        return fwdIfc.addr;
    }

    /**
     * get local address
     *
     * @return peer address, null if no session
     */
    public ipFwd getFwd() {
        return fwdCor;
    }

}

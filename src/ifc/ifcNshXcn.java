package ifc;

import ip.ipMpls;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * network service header (rfc8300) xconnect handler
 *
 * @author matecsaba
 */
public class ifcNshXcn implements ifcUp {

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    /**
     * service path id
     */
    public int sp;

    /**
     * service index
     */
    public int si;

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public counter getCounter() {
        return cntr;
    }

    public void closeUp() {
    }

    public void setState(state.states stat) {
    }

    public String toString() {
        return "nshx on " + lower;
    }

    public void recvPack(packHolder pck) {
        pck.IPprt = ifcNshFwd.protEth;
        pck.NSHttl = 63;
        pck.NSHmdt = 2;
        pck.NSHmdv = new byte[0];
        pck.NSHsp = sp;
        pck.NSHsi = si;
        ifcEther.createETHheader(pck, false);
        ipMpls.gotNshPack(null, pck);
    }

    /**
     * set filter
     *
     * @param prom pormiscous
     */
    public void setPromiscous(boolean prom) {
        lower.setFilter(prom);
    }

    /**
     * get config
     *
     * @param ntry handler
     * @return config
     */
    public static String getCfg(ifcNshXcn ntry) {
        if (ntry == null) {
            return "-1 -1";
        } else {
            return ntry.sp + " " + ntry.si;
        }
    }

}

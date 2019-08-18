package rtr;

import addr.addrIP;
import addr.addrPrefix;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipRtr;
import java.util.ArrayList;
import java.util.List;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * flowspec rewriter
 *
 * @author matecsaba
 */
public class rtrFlowspec extends ipRtr {

    /**
     * the forwarder protocol
     */
    public final ipFwd fwdCore;

    /**
     * route type
     */
    protected final tabRouteEntry.routeType rouTyp;

    /**
     * router number
     */
    protected final int rtrNum;

    /**
     * distance to give
     */
    protected int distance;

    /**
     * bytes to add
     */
    protected byte[] add;

    /**
     * create flowspec rewriter process
     *
     * @param forwarder forwarder to update
     * @param id process id
     */
    public rtrFlowspec(ipFwd forwarder, int id) {
        fwdCore = forwarder;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteEntry.routeType.flwspc4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteEntry.routeType.flwspc6;
                break;
            default:
                rouTyp = null;
                break;
        }
        distance = 254;
        add = new byte[0];
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "flowspec on " + fwdCore;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        tabRoute<addrIP> res = new tabRoute<addrIP>("computed");
        byte[] buf = new byte[128];
        byte[] adr = new byte[addrIP.size];
        for (int i = 0; i < routerRedistedF.size(); i++) {
            tabRouteEntry<addrIP> ntry = routerRedistedF.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes();
            ntry.rouTyp = rouTyp;
            ntry.protoNum = rtrNum;
            ntry.distance = distance;
            bits.byteCopy(ntry.prefix.network.getBytes(), 1, buf, 0, 15);
            bits.byteCopy(ntry.prefix.broadcast.getBytes(), 0, buf, 15, 16);
            bits.byteCopy(ntry.prefix.wildcard.getBytes(), 0, buf, 31, 16);
            bits.byteCopy(ntry.prefix.mask.getBytes(), 0, buf, 47, 16);
            int o = ntry.prefix.network.getBytes()[0];
            bits.byteCopy(add, 0, buf, o, add.length);
            o += add.length;
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
            adr[0] = (byte) o;
            bits.byteCopy(buf, 0, adr, 1, 15);
            ntry.prefix.network.fromBuf(adr, 0);
            bits.byteCopy(buf, 15, adr, 0, 16);
            ntry.prefix.broadcast.fromBuf(adr, 0);
            bits.byteCopy(buf, 31, adr, 0, 16);
            ntry.prefix.wildcard.fromBuf(adr, 0);
            bits.byteCopy(buf, 47, adr, 0, 16);
            ntry.prefix.mask.fromBuf(adr, 0);
            res.add(tabRoute.addType.better, ntry, false, false);
        }
        routerComputedF = res;
        fwdCore.routerChg(this);
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        routerCreateComputed();
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelping l) {
        l.add("1 2   distance                    specify default distance");
        l.add("2 .     <num>                     distance");
        l.add("1 2   add                         specify bytes to add");
        l.add("2 2,.   <num>                     byte");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, boolean filter) {
        l.add(beg + "distance " + distance);
        String a = "";
        for (int i = 0; i < add.length; i++) {
            a += " " + (add[i] & 0xff);
        }
        l.add(beg + "add" + a);
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        boolean negated = false;
        if (s.equals("no")) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("distance")) {
            distance = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("add")) {
            if (negated) {
                add = new byte[0];
                return false;
            }
            List<Integer> lst = new ArrayList<Integer>();
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                lst.add(bits.str2num(s));
            }
            byte[] res = new byte[lst.size()];
            for (int i = 0; i < res.length; i++) {
                res[i] = (byte) ((int) lst.get(i));
            }
            add = res;
            return false;
        }
        return true;
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
    }

    /**
     * get neighbor count
     *
     * @return count
     */
    public int routerNeighCount() {
        return 0;
    }

    /**
     * get neighbor list
     *
     * @param tab list
     */
    public void routerNeighList(tabRoute<addrIP> tab) {
    }

    /**
     * get interface count
     *
     * @return count
     */
    public int routerIfaceCount() {
        return 0;
    }

}

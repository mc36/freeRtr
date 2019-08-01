package cfg;

import ifc.ifcBridgeIfc;
import ifc.ifcEther;
import ifc.ifcUp;
import java.util.Comparator;
import pack.packLdpPwe;
import util.cmds;

/**
 * one ethernet virtual circuit configuration
 *
 * @author matecsaba
 */
public class cfgIfcEvc implements Comparator<cfgIfcEvc> {

    /**
     * number of this evc
     */
    public final int num;

    /**
     * parent interface
     */
    public final cfgIfc parent;

    private cfgXconnSide xconn;

    private ifcEther ether;

    private cfgBrdg bridgeHed;

    private ifcBridgeIfc bridgeIfc;

    /**
     * create one instance
     *
     * @param n evc id
     * @param p parent
     */
    public cfgIfcEvc(int n, cfgIfc p) {
        num = n;
        parent = p;
    }

    public String toString() {
        return parent.name + ".evc" + num;
    }

    public int compare(cfgIfcEvc o1, cfgIfcEvc o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

    /**
     * get config
     *
     * @return config
     */
    public String getCfg() {
        String s = "shutdown";
        if (bridgeHed != null) {
            s = "bridge-group " + bridgeHed.name;
        }
        if (xconn != null) {
            s = "xconnect " + xconn.getCfg();
        }
        return num + " " + s;
    }

    /**
     * parse config
     *
     * @param cmd commands
     */
    public void doCfg(cmds cmd) {
        String a = cmd.word();
        if (a.equals("shutdown")) {
            return;
        }
        if (a.equals("bridge-group")) {
            cfgBrdg brdg = cfgAll.brdgFind(cmd.word(), false);
            if (brdg == null) {
                cmd.error("invalid bridge number");
                return;
            }
            bridgeHed = brdg;
            return;
        }
        if (a.equals("xconnect")) {
            xconn = new cfgXconnSide();
            xconn.name = (parent.description.length() > 0 ? parent.description : parent.name) + ".evc" + num;
            xconn.pwtype = packLdpPwe.pwtEthPort;
            xconn.pwmtu = parent.ethtyp.getMTUsize();
            xconn.doCfg(cmd);
            if (!xconn.ready2run()) {
                xconn = null;
            }
            return;
        }
        cmd.badCmd();
    }

    /**
     * start working
     */
    public synchronized void startWork() {
        ifcUp res = null;
        if (bridgeHed != null) {
            bridgeIfc = bridgeHed.bridgeHed.newIface(true, parent.ifaceNeedMacs(), parent.ifaceNeedMacs());
            res = bridgeIfc;
        }
        if (xconn != null) {
            ether = new ifcEther(parent.ifaceNeedMacs());
            xconn.upper = ether.getSideEth();
            xconn.start2run();
            res = ether.getSideTyp();
        }
        if (res == null) {
            return;
        }
        parent.vlanHed.addVlan(num, res);
        parent.vlanHed.updateVlan(num, res);
        if (xconn != null) {
            ether.setPromiscous(true);
        }
    }

    /**
     * stop working
     */
    public synchronized void stopWork() {
        parent.vlanHed.delVlan(num);
        if (bridgeHed != null) {
            bridgeHed.bridgeHed.delIface(bridgeIfc.ifcNum);
            bridgeHed = null;
            bridgeIfc = null;
        }
        if (xconn != null) {
            xconn.stop2run();
            xconn = null;
            ether = null;
        }
    }

}

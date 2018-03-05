package ifc;

import addr.addrMac;
import addr.addrType;
import cfg.cfgInit;
import java.util.List;
import pack.packHolder;
import user.userHelping;
import util.cmds;
import util.counter;
import util.state;

/**
 * connect two interfaces
 *
 * @author matecsaba
 */
public class ifcHairpin {

    /**
     * interface is not ethernet
     */
    public boolean notEther;

    private ifcHairpinWorker s1;

    private ifcHairpinWorker s2;

    /**
     * create new instance
     */
    public ifcHairpin() {
        s1 = new ifcHairpinWorker();
        s2 = new ifcHairpinWorker();
        s1.other = s2;
        s2.other = s1;
    }

    /**
     * get side one
     *
     * @return interface handler
     */
    public ifcDn getSide1() {
        return s1;
    }

    /**
     * get side two
     *
     * @return interface handler
     */
    public ifcDn getSide2() {
        return s2;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelping l) {
        l.add("1 .  ethernet                       specify type of hairpin");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        cmds.cfgLine(l, notEther, beg, "ethernet", "");
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String s = cmd.word();
        if (s.equals("ethernet")) {
            notEther = false;
            return;
        }
        if (!s.equals("no")) {
            cmd.badCmd();
            return;
        }
        s = cmd.word();
        if (s.equals("ethernet")) {
            notEther = true;
            return;
        }
        cmd.badCmd();
    }

}

class ifcHairpinWorker implements ifcDn {

    public ifcHairpinWorker other;

    private counter cntr = new counter();

    public addrType hwaddr = addrMac.getRandom();

    private ifcUp upper = new ifcNull();

    public counter getCounter() {
        return cntr;
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return hwaddr;
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void closeDn() {
        upper = null;
    }

    public void flapped() {
    }

    public void sendPack(packHolder pck) {
        if (cfgInit.booting) {
            return;
        }
        other.upper.recvPack(pck);
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 100000000;
    }

}

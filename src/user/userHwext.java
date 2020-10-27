package user;

import cfg.cfgIfc;
import cfg.cfgInit;
import java.util.ArrayList;
import java.util.List;
import pack.packOpenflow;
import serv.servP4lang;
import util.bits;
import util.cmds;

/**
 * process hw externalization
 *
 * @author matecsaba
 */
public class userHwext {

    private enum dpTyp {
        opnflw, p4emu, p4dpdk, p4sw
    }

    private String pref = "./rtr-";

    private dpTyp dpt = null;

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public List<String> doer(cmds cmd) {
        cmd.error("externalizing hardware");
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            s = s.toLowerCase();
            if (s.equals("dataplane")) {
                s = cmd.word();
                if (s.equals("none")) {
                    dpt = null;
                    continue;
                }
                if (s.equals("openflow")) {
                    dpt = dpTyp.opnflw;
                    continue;
                }
                if (s.equals("p4emu")) {
                    dpt = dpTyp.p4emu;
                    continue;
                }
                if (s.equals("p4dpdk")) {
                    dpt = dpTyp.p4dpdk;
                    continue;
                }
                if (s.equals("p4sw")) {
                    dpt = dpTyp.p4sw;
                    continue;
                }
            }
            if (s.equals("path")) {
                pref = cmd.word();
                continue;
            }
        }
        if (dpt == null) {
            cmd.error("no dataplane selected");
            return null;
        }
        String path = pref;
        int i = path.lastIndexOf("/");
        if (i >= 0) {
            path = path.substring(0, i + 1);
        } else {
            path = "";
        }
        List<String> hwc = bits.txt2buf(pref + cfgInit.hwCfgEnd);
        List<String> swc = bits.txt2buf(pref + cfgInit.swCfgEnd);
        if (hwc == null) {
            cmd.error("error reading hw config");
            return null;
        }
        if (swc == null) {
            cmd.error("error reading sw config");
            return null;
        }
        List<String> ifp = new ArrayList<String>();
        List<String> ifl = new ArrayList<String>();
        for (i = hwc.size() - 1; i >= 0; i--) {
            cmd = new cmds("ln", hwc.get(i));
            String a = cmd.word();
            if (!a.equals("proc")) {
                continue;
            }
            cmd.word();
            a = cmd.word();
            if (!a.endsWith("pcapInt.bin") && !a.endsWith("rawInt.bin") && !a.endsWith("mapInt.bin") && !a.endsWith("tapInt.bin")) {
                continue;
            }
            String s = cmd.word();
            cmd = new cmds("ln", hwc.get(i + 1));
            a = cmd.word();
            if (!a.equals("int")) {
                continue;
            }
            a = cmd.word();
            a = cfgIfc.normName(a);
            if (a.length() < 1) {
                continue;
            }
            hwc.remove(i);
            hwc.remove(i);
            ifp.add(0, s);
            ifl.add(0, a);
        }
        cmd.error("found " + ifp.size() + " interfaces");
        if (ifp.size() < 1) {
            cmd.error("no interfaces found");
            return null;
        }
        List<String> ifr = new ArrayList<String>();
        for (i = 0; i < ifl.size(); i++) {
            ifr.add("sdn" + (i + 1));
        }
        List<String> vrf = new ArrayList<String>();
        for (int o = 0; o < swc.size(); o++) {
            String a = swc.get(o);
            if (a.startsWith("vrf definition ")) {
                vrf.add(a.substring(15, a.length()));
                continue;
            }
            for (i = ifl.size() - 1; i >= 0; i--) {
                a = a.replaceAll(ifl.get(i), ifr.get(i));
            }
            swc.set(o, a);
        }
        hwc.add("tcp2vrf 2323 dataplane 23");
        swc.add(cmds.comment);
        swc.add("vrf definition dataplane");
        swc.add(cmds.tabulator + cmds.finish);
        swc.add(cmds.comment);
        swc.add("server telnet dataplane");
        swc.add(cmds.tabulator + "security protocol telnet");
        swc.add(cmds.tabulator + "vrf dataplane");
        swc.add(cmds.tabulator + cmds.finish);
        swc.add(cmds.comment);
        List<String> hwd = new ArrayList<String>();
        switch (dpt) {
            case opnflw:
                hwd.add("ovs-vsctl init");
                hwd.add("ovs-vsctl del-br sw");
                hwd.add("ovs-vsctl add-br sw");
                hwd.add("ovs-vsctl set-controller sw tcp:127.0.0.1:" + packOpenflow.port);
                for (i = 0; i < ifp.size(); i++) {
                    hwd.add("ovs-vsctl add-port sw " + ifp.get(i));
                }
                hwc.add("tcp2vrf " + packOpenflow.port + " dataplane " + packOpenflow.port);
                swc.add("server openflow of");
                for (i = 0; i < ifr.size(); i++) {
                    swc.add(cmds.tabulator + "export-port " + ifr.get(i) + " " + (i + 1));
                }
                swc.add(cmds.tabulator + "vrf dataplane");
                swc.add(cmds.tabulator + cmds.finish);
                break;
            case p4emu:
            case p4dpdk:
            case p4sw:
                hwd.add("ip link add veth0a type veth peer name veth0b");
                userHwdet.setupIface(hwd, "veth0a");
                userHwdet.setupIface(hwd, "veth0b");
                hwd.add("ip link add veth1a type veth peer name veth1b");
                userHwdet.setupIface(hwd, "veth1a");
                userHwdet.setupIface(hwd, "veth1b");
                hwc.add("tcp2vrf " + servP4lang.port + " dataplane " + servP4lang.port);
                swc.add("server p4lang p4");
                for (i = 0; i < vrf.size(); i++) {
                    swc.add(cmds.tabulator + "export-vrf " + vrf.get(i) + " " + (i + 1));
                }
                for (i = 0; i < ifr.size(); i++) {
                    swc.add(cmds.tabulator + "export-port " + ifr.get(i) + " " + (i + 1));
                }
                swc.add(cmds.tabulator + "vrf dataplane");
                swc.add(cmds.tabulator + cmds.finish);
                break;
            default:
                return null;
        }
        List<String> res = new ArrayList<String>();
        res.add("# here are my suggested changes:");
        res.add("");
        res.add("cat >> " + path + "hwdet-all.sh << EOF");
        res.addAll(hwd);
        res.add("EOF");
        res.add("");
        res.add("cat > " + pref + cfgInit.hwCfgEnd + " << EOF");
        res.addAll(hwc);
        res.add("EOF");
        res.add("");
        res.add("cat > " + pref + cfgInit.swCfgEnd + " << EOF");
        res.addAll(swc);
        res.add("EOF");
        return res;
    }

}

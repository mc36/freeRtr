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

    private String hwdn = "hwdet-all.sh";

    private dpTyp dpt = null;

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public List<String> doer(cmds cmd) {
        cmds orig = cmd;
        orig.error("externalizing forwarding");
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
            orig.error("no dataplane selected");
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
        if (hwc == null) {
            orig.error("error reading hw config");
            return null;
        }
        List<String> swc = bits.txt2buf(pref + cfgInit.swCfgEnd);
        if (swc == null) {
            orig.error("error reading sw config");
            return null;
        }
        List<String> hwd = bits.txt2buf(path + hwdn);
        if (hwd == null) {
            orig.error("error reading " + hwdn);
            return null;
        }
        List<String> hw1 = new ArrayList<String>();
        List<String> hw2 = new ArrayList<String>();
        int o = hwd.indexOf("### main ###");
        if (o < 0) {
            orig.error("error splitting " + hwdn);
            return null;
        }
        for (i = 0; i < o; i++) {
            hw1.add(hwd.get(i));
        }
        hw1.add("");
        hw2.add("");
        for (i = o; i < hwd.size(); i++) {
            hw2.add(hwd.get(i));
        }
        hwd = new ArrayList<String>();
        hwd.add("### dataplane ###");
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
            boolean tap = a.endsWith("tapInt.bin");
            if (!a.endsWith("pcapInt.bin") && !a.endsWith("rawInt.bin") && !a.endsWith("mapInt.bin") && !tap) {
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
            if (tap) {
                s = "veth1b";
            }
            ifp.add(0, s);
            ifl.add(0, a);
        }
        orig.error("found " + ifp.size() + " interfaces");
        if (ifp.size() < 1) {
            orig.error("no interfaces found");
            return null;
        }
        List<String> ifr = new ArrayList<String>();
        for (i = 0; i < ifl.size(); i++) {
            ifr.add("sdn" + (i + 1));
        }
        List<String> vrf = new ArrayList<String>();
        for (o = 0; o < swc.size(); o++) {
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
        String dpv = null;
        switch (dpt) {
            case opnflw:
                dpv = "of";
                break;
            case p4dpdk:
            case p4emu:
            case p4sw:
                dpv = "p4";
                break;
            default:
                return null;
        }
        hwc.add("tcp2vrf 2323 " + dpv + " 23");
        swc.add(cmds.comment);
        swc.add("vrf definition " + dpv);
        swc.add(cmds.tabulator + cmds.finish);
        swc.add(cmds.comment);
        swc.add("server telnet " + dpv);
        swc.add(cmds.tabulator + "security protocol telnet");
        swc.add(cmds.tabulator + "vrf " + dpv);
        swc.add(cmds.tabulator + cmds.finish);
        swc.add(cmds.comment);
        switch (dpt) {
            case opnflw:
                hwd.add("ovs-vsctl init");
                hwd.add("ovs-vsctl del-br sw");
                hwd.add("ovs-vsctl add-br sw");
                hwd.add("ovs-vsctl set-controller sw tcp:127.0.0.1:" + packOpenflow.port);
                for (i = 0; i < ifp.size(); i++) {
                    hwd.add("ovs-vsctl add-port sw " + ifp.get(i));
                }
                hwc.add("tcp2vrf " + packOpenflow.port + " " + dpv + " " + packOpenflow.port);
                swc.add("server openflow " + dpv);
                for (i = 0; i < ifr.size(); i++) {
                    swc.add(cmds.tabulator + "export-port " + ifr.get(i) + " " + (i + 1));
                }
                swc.add(cmds.tabulator + "vrf " + dpv);
                swc.add(cmds.tabulator + cmds.finish);
                break;
            case p4emu:
            case p4dpdk:
            case p4sw:
                hwd.add("ip link add veth1a type veth peer name veth1b");
                userHwdet.setupIface(hwd, "veth1a", 8192);
                userHwdet.setupIface(hwd, "veth1b", 1500);
                hwd.add("ip link set veth1b up address 00:00:11:11:22:22 mtu 1500");
                hwd.add("ip addr add 10.255.255.1/24 dev veth1b");
                hwd.add("ip route add 0.0.0.0/0 via 10.255.255.254 dev veth1b");
                hwd.add("echo 0 > /proc/sys/net/ipv6/conf/veth1b/disable_ipv6");
                hwc.add("tcp2vrf " + servP4lang.port + " " + dpv + " " + servP4lang.port);
                swc.add("interface ethernet0");
                swc.add(cmds.tabulator + "description p4 cpu port");
                swc.add(cmds.tabulator + "no shutdown");
                swc.add(cmds.tabulator + cmds.finish);
                swc.add(cmds.comment);
                swc.add("server p4lang " + dpv);
                for (i = 0; i < vrf.size(); i++) {
                    swc.add(cmds.tabulator + "export-vrf " + vrf.get(i) + " " + (i + 1));
                }
                for (i = 0; i < ifr.size(); i++) {
                    swc.add(cmds.tabulator + "export-port " + ifr.get(i) + " " + i);
                }
                swc.add(cmds.tabulator + "interconnect ethernet0");
                swc.add(cmds.tabulator + "vrf " + dpv);
                swc.add(cmds.tabulator + cmds.finish);
                String ifn = null;
                switch (dpt) {
                    case p4dpdk:
                        ifn = "veth0a";
                        hwd.add("ip link add veth0a type veth peer name veth0b");
                        userHwdet.setupIface(hwd, "veth0a", 8192);
                        userHwdet.setupIface(hwd, "veth0b", 8192);
                        hwc.add("proc p4emu " + path + "p4dpdk.bin --vdev=net_af_packet0,iface=veth1b --vdev=net_af_packet1,iface=veth0b 127.0.0.1 " + servP4lang.port + " " + ifl.size());
                        break;
                    case p4emu:
                        ifn = "veth0a";
                        hwd.add("ip link add veth0a type veth peer name veth0b");
                        userHwdet.setupIface(hwd, "veth0a", 8192);
                        userHwdet.setupIface(hwd, "veth0b", 8192);
                        String a = "";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " " + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4emu.bin 127.0.0.1 " + servP4lang.port + " " + ifl.size() + a + " veth0b");
                        break;
                    case p4sw:
                        ifn = "enp6s0";
                        userHwdet.setupIface(hwd, ifn, 8192);
                        break;
                    default:
                        return null;
                }
                hwc.add("int eth0 eth - 127.0.0.1 19999 127.0.0.1 19998");
                hwc.add("proc veth0 " + path + "pcapInt.bin " + ifn + " 19998 127.0.0.1 19999 127.0.0.1");
                break;
            default:
                return null;
        }
        List<String> res = new ArrayList<String>();
        res.add("# here are my suggested changes:");
        res.add("");
        res.add("cat > " + path + hwdn + " << EOF");
        res.addAll(hw1);
        res.addAll(hwd);
        res.addAll(hw2);
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

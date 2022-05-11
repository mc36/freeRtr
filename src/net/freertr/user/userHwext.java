package net.freertr.user;

import java.util.ArrayList;
import java.util.List;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgInit;
import net.freertr.pack.packOpenflow;
import net.freertr.serv.servP4lang;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * process hw externalization
 *
 * @author matecsaba
 */
public class userHwext {

    /**
     * create instance
     */
    public userHwext() {
    }

    private enum dpTyp {
        opnflw, p4emu, p4xdp, p4dpdk, p4sw
    }

    private String pref = "./rtr-";

    private String hwdn = "hwdet-all.sh";

    private dpTyp dpt = null;

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public void doer(cmds cmd) {
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
                if (s.equals("p4xdp")) {
                    dpt = dpTyp.p4xdp;
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
            return;
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
            return;
        }
        List<String> swc = bits.txt2buf(pref + cfgInit.swCfgEnd);
        if (swc == null) {
            orig.error("error reading sw config");
            return;
        }
        List<String> hwd = bits.txt2buf(path + hwdn);
        if (hwd == null) {
            orig.error("error reading " + hwdn);
            return;
        }
        List<String> hw1 = new ArrayList<String>();
        List<String> hw2 = new ArrayList<String>();
        if (hwd.indexOf("### dataplane ###") >= 0) {
            orig.error("already exists");
            return;
        }
        int o = hwd.indexOf("### main ###");
        if (o < 0) {
            orig.error("error splitting " + hwdn);
            return;
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
            String pnm[] = cfgIfc.dissectName(a);
            a = pnm[0] + pnm[1];
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
        if ((dpt != dpTyp.p4sw) && (ifp.size() < 1)) {
            orig.error("no interfaces found");
            return;
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
            case p4xdp:
            case p4sw:
                dpv = "p4";
                break;
            default:
                return;
        }
        hwc.add("tcp2vrf 2323 " + dpv + " 23 127.0.0.1");
        swc.add(cmds.comment);
        swc.add("vrf definition " + dpv);
        swc.add(cmds.tabulator + cmds.finish);
        swc.add(cmds.comment);
        for (i = 0; i < ifr.size(); i++) {
            swc.add("interface " + ifr.get(i));
            swc.add(cmds.tabulator + cmds.finish);
            swc.add(cmds.comment);
        }
        swc.add("server telnet " + dpv);
        swc.add(cmds.tabulator + "security protocol telnet");
        swc.add(cmds.tabulator + "vrf " + dpv);
        swc.add(cmds.tabulator + cmds.finish);
        swc.add(cmds.comment);
        userHwdet.setupVeth(hwd, "veth1a", "veth1b");
        userHwdet.setupIface(hwd, "veth1a", 1500);
        userHwdet.setupIface(hwd, "veth1b", 8192);
        hwd.add("ip link set veth1a up address 00:00:11:11:22:22 mtu 1500");
        hwd.add("ip addr add 10.255.255.1/24 dev veth1a");
        hwd.add("ip route add 0.0.0.0/0 via 10.255.255.254 dev veth1a");
        hwd.add("echo 0 > /proc/sys/net/ipv6/conf/veth1a/disable_ipv6");
        String res = "";
        switch (dpt) {
            case opnflw:
                hwd.add("ovs-vsctl init");
                hwd.add("ovs-vsctl del-br sw");
                hwd.add("ovs-vsctl add-br sw");
                hwd.add("ovs-vsctl set-controller sw tcp:127.0.0.1:" + packOpenflow.port);
                for (i = 0; i < ifp.size(); i++) {
                    hwd.add("ovs-vsctl add-port sw " + ifp.get(i));
                }
                hwc.add("tcp2vrf " + packOpenflow.port + " " + dpv + " " + packOpenflow.port + " 127.0.0.1");
                swc.add("server openflow " + dpv);
                swc.add(cmds.tabulator + "export-vrf " + vrf.get(0));
                for (i = 0; i < ifr.size(); i++) {
                    swc.add(cmds.tabulator + "export-port " + ifr.get(i) + " " + (i + 1));
                }
                swc.add(cmds.tabulator + "vrf " + dpv);
                swc.add(cmds.tabulator + cmds.finish);
                break;
            case p4emu:
            case p4xdp:
            case p4dpdk:
            case p4sw:
                hwc.add("tcp2vrf " + servP4lang.port + " " + dpv + " " + servP4lang.port + " 127.0.0.1");
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
                        userHwdet.setupVeth(hwd, "veth0a", "veth0b");
                        userHwdet.setupIface(hwd, "veth0a", 8192);
                        userHwdet.setupIface(hwd, "veth0b", 8192);
                        String a = "";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " --vdev=net_af_packet" + i + ",iface=" + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4dpdk.bin" + a + " --vdev=net_af_packet" + ifl.size() + ",iface=veth0b -- 127.0.0.1 " + servP4lang.port + " " + ifl.size());
                        res = ", please enable nic bindings in " + hwdn;
                        break;
                    case p4emu:
                        ifn = "veth0a";
                        userHwdet.setupVeth(hwd, "veth0a", "veth0b");
                        userHwdet.setupIface(hwd, "veth0a", 8192);
                        userHwdet.setupIface(hwd, "veth0b", 8192);
                        a = "";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " " + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4emu.bin 127.0.0.1 " + servP4lang.port + " " + ifl.size() + a + " veth0b");
                        break;
                    case p4xdp:
                        ifn = "veth0a";
                        hwd.add("ulimit -l unlimited");
                        userHwdet.setupVeth(hwd, "veth0a", "veth0b");
                        userHwdet.setupIface(hwd, "veth0a", 2048);
                        userHwdet.setupIface(hwd, "veth0b", 2048);
                        hwd.add("ip link set " + ifn + " xdp obj p4xdp_pass.bin sec p4xdp_pass");
                        a = "";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " " + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4xdp_user.bin 127.0.0.1 " + servP4lang.port + " " + ifl.size() + a + " veth0b");
                        break;
                    case p4sw:
                        ifn = "ens1";
                        userHwdet.setupIface(hwd, ifn, 8192);
                        hwc.add("proc bfswd " + path + "start_bfswd.sh");
                        hwc.add("proc bffwd " + path + "bf_forwarder.py");
                        break;
                    default:
                        return;
                }
                hwc.add("int eth0 eth - 127.0.0.1 19999 127.0.0.1 19998");
                hwc.add("proc cpuport " + path + "pcapInt.bin " + ifn + " 19998 127.0.0.1 19999 127.0.0.1");
                break;
            default:
                return;
        }
        hw1.addAll(hwd);
        hw1.addAll(hw2);
        if (bits.buf2txt(true, hw1, path + hwdn)) {
            orig.error("error saving " + hwdn);
            return;
        }
        if (bits.buf2txt(true, hwc, pref + cfgInit.hwCfgEnd)) {
            orig.error("error saving hw config");
            return;
        }
        if (bits.buf2txt(true, swc, pref + cfgInit.swCfgEnd)) {
            orig.error("error saving sw config");
            return;
        }
        orig.error("finished" + res);
    }

}

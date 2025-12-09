package org.freertr.user;

import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.pack.packOpenflow;
import org.freertr.serv.servP4lang;
import org.freertr.util.bits;
import org.freertr.util.cmds;

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
        opnflw, p4emu, p4map, p4raw, p4xsk, p4udp, p4urng, p4xdp, p4dpdk, p4sw
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
                if (s.equals("p4map")) {
                    dpt = dpTyp.p4map;
                    continue;
                }
                if (s.equals("p4raw")) {
                    dpt = dpTyp.p4raw;
                    continue;
                }
                if (s.equals("p4xsk")) {
                    dpt = dpTyp.p4xsk;
                    continue;
                }
                if (s.equals("p4udp")) {
                    dpt = dpTyp.p4udp;
                    continue;
                }
                if (s.equals("p4urng")) {
                    dpt = dpTyp.p4urng;
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
        bits.buf2txt(true, hwc, pref + cfgInit.hwCfgEnd + userUpgrade.bakExt);
        bits.buf2txt(true, swc, pref + cfgInit.swCfgEnd + userUpgrade.bakExt);
        bits.buf2txt(true, hwd, path + hwdn + userUpgrade.bakExt);
        List<String> hw1 = new ArrayList<String>();
        List<String> hw2 = new ArrayList<String>();
        if (hwd.indexOf("### dataplane ###") >= 0) {
            orig.error("already exists");
            return;
        }
        int o = hwd.indexOf("### lines ###");
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
        List<String> ifr = new ArrayList<String>();
        List<String> mac = new ArrayList<String>();
        List<Integer> prt = new ArrayList<Integer>();
        for (i = hwc.size() - 1; i >= 0; i--) {
            cmd = new cmds("ln", hwc.get(i));
            String a = cmd.word();
            if (!a.equals("proc")) {
                continue;
            }
            cmd.word();
            a = cmd.word();
            boolean sck = a.endsWith("socat");
            boolean tap = a.endsWith("tapInt.bin");
            if (!a.endsWith("pcapInt.bin") && !a.endsWith("rawInt.bin") && !a.endsWith("mapInt.bin") && !a.endsWith("xskInt.bin") && !a.endsWith("urngInt.bin") && !a.endsWith("bsdInt.bin") && !a.endsWith("cmp1int.bin") && !a.endsWith("cmp2int.bin") && !tap && !sck) {
                continue;
            }
            String s = cmd.word();
            if ((i + 1) >= hwc.size()) {
                continue;
            }
            if (sck) {
                int p = s.indexOf(":");
                if (p < 1) {
                    continue;
                }
                a = s.substring(0, p).toLowerCase();
                s = s.substring(p + 1, s.length());
                tap = a.equals("tun");
            }
            cmd = new cmds("ln", hwc.get(i + 1));
            a = cmd.word();
            if (!a.equals("int")) {
                continue;
            }
            a = cmd.word();
            String pnm[] = cfgIfc.dissectName(a);
            if (pnm == null) {
                continue;
            }
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    continue;
                }
                if (a.equals("eth")) {
                    break;
                }
            }
            a = cmd.word();
            if (a.length() < 1) {
                a = "-";
            }
            cmd.word();
            if (dpt == dpTyp.p4udp) {
                hwc.remove(i + 1);
            } else {
                hwc.remove(i);
                hwc.remove(i);
            }
            if (tap) {
                s = "veth1b";
            }
            ifp.add(0, s);
            ifl.add(0, pnm[0] + pnm[1] + pnm[2]);
            ifr.add(0, "sdn" + pnm[1] + pnm[2]);
            mac.add(0, a);
            prt.add(0, bits.str2num(cmd.word()));
        }
        orig.error("found " + ifp.size() + " interfaces");
        if ((dpt != dpTyp.p4sw) && (ifp.size() < 1)) {
            orig.error("no interfaces found");
            return;
        }
        for (i = 0; i < ifl.size(); i++) {
            orig.error("ifc " + i + ": " + ifr.get(i) + " " + ifl.get(i) + " " + ifp.get(i) + " " + mac.get(i));
        }
        List<String> vrf = new ArrayList<String>();
        List<String> brd = new ArrayList<String>();
        for (o = 0; o < swc.size(); o++) {
            String a = swc.get(o);
            if (a.startsWith("vrf definition ")) {
                vrf.add(a.substring(15, a.length()));
                continue;
            }
            if (a.startsWith("bridge ")) {
                brd.add(a.substring(7, a.length()));
                continue;
            }
            for (i = ifl.size() - 1; i >= 0; i--) {
                a = a.replaceAll(ifl.get(i), ifr.get(i));
            }
            swc.set(o, a);
        }
        orig.error("found " + vrf.size() + " vrfs and " + brd.size() + " bridges");
        if ((vrf.size() + brd.size()) < 1) {
            orig.error("nothing found");
            return;
        }
        for (i = 0; i < vrf.size(); i++) {
            orig.error("vrf " + i + ": " + vrf.get(i));
        }
        for (i = 0; i < brd.size(); i++) {
            orig.error("bridge " + i + ": " + brd.get(i));
        }
        String dpv = null;
        switch (dpt) {
            case opnflw:
                dpv = "of";
                break;
            case p4dpdk:
            case p4emu:
            case p4map:
            case p4raw:
            case p4udp:
            case p4xsk:
            case p4urng:
            case p4xdp:
            case p4sw:
                dpv = "p4";
                break;
            default:
                return;
        }
        hwc.add("tcp2vrf 2323 " + dpv + " 23 127.0.0.1");
        for (i = 0; i < ifr.size(); i++) {
            o = swc.indexOf("interface " + ifr.get(i));
            if (o < 0) {
                continue;
            }
            swc.add(o + 1, cmds.tabulator + "macaddr " + mac.get(i));
        }
        swc.add(cmds.comment);
        swc.add("vrf definition " + dpv);
        swc.add(cmds.tabulator + cmds.finish);
        swc.add(cmds.comment);
        swc.add("server telnet " + dpv);
        swc.add(cmds.tabulator + "security protocol telnet");
        swc.add(cmds.tabulator + "vrf " + dpv);
        swc.add(cmds.tabulator + cmds.finish);
        swc.add(cmds.comment);
        if (dpt != dpTyp.p4udp) {
            userHwdet.setupVeth(hwd, path, userHwdet.ifcTyp.raw, "veth1a", "veth1b");
            userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth1a", 1500, "00:00:11:11:22:22");
            userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth1b", 8192, null);
            userHwdet.routeIface(hwd, "veth1a");
        }
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
                if (vrf.size() > 0) {
                    swc.add(cmds.tabulator + "export-vrf " + vrf.get(0));
                }
                for (i = 0; i < ifr.size(); i++) {
                    swc.add(cmds.tabulator + "export-port " + ifr.get(i) + " " + (i + 1));
                }
                swc.add(cmds.tabulator + "vrf " + dpv);
                swc.add(cmds.tabulator + cmds.finish);
                break;
            case p4emu:
            case p4map:
            case p4raw:
            case p4udp:
            case p4xsk:
            case p4urng:
            case p4xdp:
            case p4dpdk:
            case p4sw:
                hwd.add("ulimit -l unlimited");
                hwc.add("tcp2vrf " + servP4lang.port + " " + dpv + " " + servP4lang.port + " 127.0.0.1");
                swc.add("interface ethernet0");
                swc.add(cmds.tabulator + "description p4 cpu port");
                swc.add(cmds.tabulator + "no shutdown");
                swc.add(cmds.tabulator + cmds.finish);
                swc.add(cmds.comment);
                swc.add("server p4lang " + dpv);
                for (i = 0; i < vrf.size(); i++) {
                    swc.add(cmds.tabulator + "export-vrf " + vrf.get(i));
                }
                for (i = 0; i < brd.size(); i++) {
                    swc.add(cmds.tabulator + "export-bridge " + brd.get(i));
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
                        hwd.add("#modprobe uio_pci_generic");
                        hwd.add("echo 128 > /proc/sys/vm/nr_hugepages");
                        hwd.add("modprobe vfio-pci");
                        for (i = 0; i < ifp.size(); i++) {
                            hwd.add("dpdk-devbind.py -b vfio-pci " + ifp.get(i));
                        }
                        ifn = "veth0a";
                        userHwdet.setupVeth(hwd, path, userHwdet.ifcTyp.raw, "veth0a", "veth0b");
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0a", 8192, null);
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0b", 8192, null);
                        String a = "";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " " + "--vdev=net_af_packet" + i + ",iface=" + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4dpdk.bin" + a + " " + "--vdev=net_af_packet" + ifl.size() + ",iface=veth0b -- 127.0.0.1 " + servP4lang.port + " " + ifl.size());
                        hwc.add("proc cpuport " + path + "pcapInt.bin " + ifn + " 19998 127.0.0.1 19999 127.0.0.1");
                        res = ", please verify nic bindings in " + hwdn;
                        break;
                    case p4emu:
                        ifn = "veth0a";
                        userHwdet.setupVeth(hwd, path, userHwdet.ifcTyp.raw, "veth0a", "veth0b");
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0a", 8192, null);
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0b", 8192, null);
                        a = "";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " " + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4emu.bin 127.0.0.1 " + servP4lang.port + " " + ifl.size() + a + " veth0b");
                        hwc.add("proc cpuport " + path + "pcapInt.bin " + ifn + " 19998 127.0.0.1 19999 127.0.0.1");
                        break;
                    case p4map:
                        ifn = "veth0a";
                        userHwdet.setupVeth(hwd, path, userHwdet.ifcTyp.raw, "veth0a", "veth0b");
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0a", 8192, null);
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0b", 8192, null);
                        a = "";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " " + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4map.bin 127.0.0.1 " + servP4lang.port + " " + ifl.size() + a + " veth0b");
                        hwc.add("proc cpuport " + path + "mapInt.bin " + ifn + " 19998 127.0.0.1 19999 127.0.0.1");
                        break;
                    case p4raw:
                        ifn = "veth0a";
                        userHwdet.setupVeth(hwd, path, userHwdet.ifcTyp.raw, "veth0a", "veth0b");
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0a", 8192, null);
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0b", 8192, null);
                        a = "";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " " + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4raw.bin 127.0.0.1 " + servP4lang.port + " " + ifl.size() + a + " veth0b");
                        hwc.add("proc cpuport " + path + "rawInt.bin " + ifn + " 19998 127.0.0.1 19999 127.0.0.1");
                        break;
                    case p4udp:
                        a = "";
                        for (i = 0; i < ifp.size(); i++) {
                            o = prt.get(i);
                            a += " " + o + " " + (o + 1);
                        }
                        hwc.add("proc p4emu " + path + "p4udp.bin 127.0.0.1 " + servP4lang.port + " " + ifl.size() + " 127.0.0.1 127.0.0.1" + a + " 19998 19999");
                        break;
                    case p4xsk:
                        ifn = "veth0a";
                        userHwdet.setupVeth(hwd, path, userHwdet.ifcTyp.raw, "veth0a", "veth0b");
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0a", 8192, null);
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0b", 8192, null);
                        a = " skb";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " " + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4xsk.bin 127.0.0.1 " + servP4lang.port + " " + ifl.size() + a + " veth0b");
                        hwc.add("proc cpuport " + path + "xskInt.bin " + ifn + " skb 19998 127.0.0.1 19999 127.0.0.1");
                        break;
                    case p4urng:
                        ifn = "veth0a";
                        userHwdet.setupVeth(hwd, path, userHwdet.ifcTyp.raw, "veth0a", "veth0b");
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0a", 8192, null);
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0b", 8192, null);
                        a = "";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " " + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4urng.bin 127.0.0.1 " + servP4lang.port + " " + ifl.size() + a + " veth0b");
                        hwc.add("proc cpuport " + path + "urngInt.bin " + ifn + " 19998 127.0.0.1 19999 127.0.0.1");
                        break;
                    case p4xdp:
                        ifn = "veth0a";
                        userHwdet.setupVeth(hwd, path, userHwdet.ifcTyp.raw, "veth0a", "veth0b");
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0a", 2048, null);
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, "veth0b", 2048, null);
                        hwd.add("ip link set " + ifn + " xdpgeneric off");
                        hwd.add("ip link set " + ifn + " xdpgeneric obj p4xdp_pass.bin sec p4xdp_pass");
                        a = " skb";
                        for (i = 0; i < ifp.size(); i++) {
                            a += " " + ifp.get(i);
                        }
                        hwc.add("proc p4emu " + path + "p4xdp_user.bin 127.0.0.1 " + servP4lang.port + " " + ifl.size() + a + " veth0b");
                        hwc.add("proc cpuport " + path + "pcapInt.bin " + ifn + " 19998 127.0.0.1 19999 127.0.0.1");
                        break;
                    case p4sw:
                        ifn = "ens1";
                        userHwdet.setupIface(hwd, path, userHwdet.ifcTyp.raw, ifn, 8192, null);
                        hwc.add("proc bfswd " + path + "start_bfswd.sh");
                        hwc.add("proc bffwd " + path + "bf_forwarder.py");
                        hwc.add("proc cpuport " + path + "pcapInt.bin " + ifn + " 19998 127.0.0.1 19999 127.0.0.1");
                        break;
                    default:
                        return;
                }
                hwc.add("int ether0 eth - 127.0.0.1 19999 127.0.0.1 19998");
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

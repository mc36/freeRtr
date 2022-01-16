package net.freertr.user;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import net.freertr.cfg.cfgInit;
import net.freertr.tab.tabGen;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.version;

/**
 * process hw detection
 *
 * @author matecsaba
 */
public class userHwdet {

    /**
     * create instance
     */
    public userHwdet() {
    }

    private enum ifcTyp {
        socat, pcap, raw, map
    }

    private int nextPort = 0;

    private int ifcNum = 0;

    private int linNum = 0;

    private int crsNum = 10000;

    private int tapNum = 20000;

    private List<String> starter;

    private List<String> config;

    private String scrBeg = "#!/bin/sh";

    private String prefix = "hwdet-";

    private boolean binMain = false;

    private boolean inlineLoop = false;

    private ifcTyp ifaceType = ifcTyp.socat;

    private ifcTyp lineType = ifcTyp.socat;

    private String path = "./";

    private String lstEth = "hwdet.eth";

    private String lstSer = "hwdet.ser";

    private String lstMac = "hwdet.mac";

    private tabGen<userHwdetIface> macLst = new tabGen<userHwdetIface>();

    private void addComment(String s) {
        starter.add("");
        starter.add("### " + s + " ###");
        starter.add("echo starting " + s + ".");
    }

    private void makeLoop(String fn, List<String> pre, List<String> lop) {
        List<String> txt = new ArrayList<String>();
        txt.add(scrBeg);
        txt.add("");
        for (int i = 0; i < pre.size(); i++) {
            txt.add(pre.get(i));
        }
        txt.add("while (true); do");
        for (int i = 0; i < lop.size(); i++) {
            txt.add("  " + lop.get(i));
        }
        txt.add("  sleep 1");
        txt.add("  done");
        bits.buf2txt(true, txt, path + prefix + fn);
        starter.add("start-stop-daemon -S -b -x " + path + prefix + fn);
    }

    private void makeLoop(String fn, List<String> pre, String cmd) {
        if (!inlineLoop) {
            makeLoop(fn, pre, bits.str2lst(cmd));
            return;
        }
        starter.addAll(pre);
        config.add("proc " + fn + " " + cmd);
    }

    /**
     * set up veth
     *
     * @param lst list
     * @param n1 first name
     * @param n2 second name
     */
    public static void setupVeth(List<String> lst, String n1, String n2) {
        lst.add("ip link add " + n1 + " type veth peer name veth0");
        lst.add("ip link set veth0 name " + n2);
    }

    /**
     * set up interface
     *
     * @param lst list
     * @param nam name
     * @param mtu mtu value
     */
    public static void setupIface(List<String> lst, String nam, int mtu) {
        lst.add("ip link set " + nam + " up multicast on promisc on mtu " + mtu);
        lst.add("ethtool -K " + nam + " rx off");
        lst.add("ethtool -K " + nam + " tx off");
        lst.add("ethtool -K " + nam + " sg off");
        lst.add("ethtool -K " + nam + " tso off");
        lst.add("ethtool -K " + nam + " ufo off");
        lst.add("ethtool -K " + nam + " gso off");
        lst.add("ethtool -K " + nam + " gro off");
        lst.add("ethtool -K " + nam + " lro off");
        lst.add("ethtool -K " + nam + " rxvlan off");
        lst.add("ethtool -K " + nam + " txvlan off");
        lst.add("ethtool -K " + nam + " ntuple off");
        lst.add("ethtool -K " + nam + " rxhash off");
        lst.add("ethtool --set-eee " + nam + " eee off");
    }

    private void createIface(String nam, String adr) {
        ifcNum += 1;
        int p1 = nextPort + 1;
        int p2 = nextPort + 2;
        nextPort += 10;
        String stat = "";
        String cmd = "";
        switch (ifaceType) {
            case socat:
                cmd = "socat INTERFACE:" + nam + " UDP4-DATAGRAM:127.0.0.1:" + p1 + ",bind=127.0.0.1:" + p2 + ",reuseaddr";
                break;
            case pcap:
                cmd = path + "pcapInt.bin " + nam + " " + p2 + " 127.0.0.1 " + p1 + " 127.0.0.1";
                break;
            case raw:
                cmd = path + "rawInt.bin " + nam + " " + p2 + " 127.0.0.1 " + p1 + " 127.0.0.1";
                stat = "stat ";
                break;
            case map:
                cmd = path + "mapInt.bin " + nam + " " + p2 + " 127.0.0.1 " + p1 + " 127.0.0.1";
                stat = "stat ";
                break;
        }
        List<String> ifc = new ArrayList<String>();
        setupIface(ifc, nam, 1500);
        makeLoop("ifc" + ifcNum + ".sh", ifc, cmd);
        config.add("int " + "eth" + ifcNum + " " + stat + "eth " + adr + " 127.0.0.1 " + p1 + " 127.0.0.1 " + p2);
    }

    private void createLine(String nam) {
        linNum += 1;
        int p1 = nextPort + 1;
        int p2 = nextPort + 2;
        nextPort += 10;
        nam = "/dev/ttyS" + nam;
        String cmd = "";
        switch (lineType) {
            case socat:
                cmd = "socat TCP4-LISTEN:" + p1 + ",reuseaddr FILE:" + nam + ",sane,b9600,cs8,raw,echo=0,crtscts=0";
                break;
            case raw:
                cmd = path + "ttyLin.bin " + nam + " " + p1;
                break;
            default:
                break;
        }
        makeLoop("lin" + linNum + ".sh", bits.str2lst(path + "modem.bin " + nam + " \"speedset 9600\" \"ctrlset 3\""), cmd);
        config.add("line tty" + linNum + " 127.0.0.1 " + p2 + " 127.0.0.1 " + p1);
    }

    private void createCross() {
        crsNum += 1;
        int p1 = nextPort + 1;
        int p2 = nextPort + 2;
        nextPort += 10;
        config.add("int eth" + crsNum + " eth - 127.0.0.1 " + p1 + " 127.0.0.1 " + p2);
        crsNum += 1;
        config.add("int eth" + crsNum + " eth - 127.0.0.1 " + p2 + " 127.0.0.1 " + p1);
    }

    private void detectMacs(String fn) {
        addComment("macs");
        List<String> res = bits.txt2buf(fn);
        if (res == null) {
            return;
        }
        for (int ln = 0; ln < res.size(); ln++) {
            String a = res.get(ln).trim();
            int i = a.indexOf(" mtu ");
            if (i < 0) {
                continue;
            }
            i = a.indexOf(": ");
            if (i < 0) {
                continue;
            }
            if (i > 4) {
                continue;
            }
            a = a.substring(i + 2, a.length());
            i = a.indexOf(": ");
            if (i < 0) {
                continue;
            }
            userHwdetIface ntry = new userHwdetIface();
            ntry.name = a.substring(0, i).trim();
            a = res.get(ln + 1).trim();
            if (!a.startsWith("link/ether")) {
                continue;
            }
            ntry.mac = a.substring(11, 28);
            macLst.add(ntry);
        }
        for (int i = 0; i < macLst.size(); i++) {
            starter.add("# " + macLst.get(i) + " #");
        }
    }

    private void detectIfaces(String fn) {
        addComment("interfaces");
        final String unneeded = "/lo/dummy0/";
        List<String> res = bits.txt2buf(fn);
        if (res == null) {
            return;
        }
        Collections.sort(res);
        for (int cnt = 0; cnt < res.size(); cnt++) {
            String s = res.get(cnt);
            int i = s.indexOf(":");
            if (i < 0) {
                continue;
            }
            s = s.substring(0, i).trim();
            if (s.length() < 1) {
                continue;
            }
            if (unneeded.indexOf("/" + s.toLowerCase() + "/") >= 0) {
                continue;
            }
            userHwdetIface ntry = new userHwdetIface();
            ntry.name = s.trim();
            ntry = macLst.find(ntry);
            if (ntry == null) {
                createIface(s, "-");
            } else {
                createIface(s, ntry.mac);
            }
        }
    }

    private void detectLines(String fn) {
        addComment("lines");
        List<String> res = bits.txt2buf(fn);
        if (res == null) {
            return;
        }
        Collections.sort(res);
        for (int cnt = 0; cnt < res.size(); cnt++) {
            String s = res.get(cnt);
            int i = s.indexOf(":");
            if (i < 0) {
                continue;
            }
            String a = s.substring(0, i).trim();
            if (a.length() > 2) {
                continue;
            }
            s = s.substring(i + 1, s.length()).trim();
            i = s.indexOf(" ");
            if (i < 0) {
                continue;
            }
            s = s.substring(0, i).trim().toLowerCase();
            if (s.indexOf("unknown") > 0) {
                continue;
            }
            createLine(a);
        }
    }

    private void detectCrosses(String num) {
        for (int i = 0; i < bits.str2num(num); i++) {
            createCross();
        }
    }

    private void detectTuntap(String tap) {
        if (tap.length() < 1) {
            return;
        }
        tapNum += 1;
        int p1 = nextPort + 1;
        int p2 = nextPort + 2;
        nextPort += 10;
        String cmd;
        if (ifaceType == ifcTyp.socat) {
            int i = tap.indexOf(" ");
            cmd = "socat TUN:" + tap.substring(0, i) + ",tun-name=tap" + tapNum + ",tun-type=tap,iff-no-pi,iff-broadcast,iff-up" + " UDP4-DATAGRAM:127.0.0.1:" + p1 + ",bind=127.0.0.1:" + p2 + ",reuseaddr";
        } else {
            cmd = path + "tapInt.bin tap" + tapNum + " " + p2 + " 127.0.0.1 " + p1 + " 127.0.0.1 " + tap;
        }
        makeLoop("tap" + tapNum + ".sh", bits.str2lst(""), cmd);
        config.add("int eth" + tapNum + " eth - 127.0.0.1 " + p1 + " 127.0.0.1 " + p2);
    }

    private void detectTcpvrf(String tcp) {
        if (tcp.length() < 1) {
            return;
        }
        config.add("tcp2vrf " + tcp);
    }

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public void doer(cmds cmd) {
        cmd.error("detecting hardware");
        String mem = "384m";
        String cross = "0";
        String tuntap = "";
        String tcpvrf = "";
        String hwId = "xxx";
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            s = s.toLowerCase();
            if (s.equals("mem")) {
                mem = cmd.word();
                continue;
            }
            if (s.equals("cross")) {
                cross = cmd.word();
                continue;
            }
            if (s.equals("path")) {
                path = cmd.word();
                continue;
            }
            if (s.equals("eth")) {
                lstEth = cmd.word();
                continue;
            }
            if (s.equals("ser")) {
                lstSer = cmd.word();
                continue;
            }
            if (s.equals("mac")) {
                lstMac = cmd.word();
                continue;
            }
            if (s.equals("tuntap")) {
                tuntap = cmd.word();
                tuntap += " " + cmd.word();
                continue;
            }
            if (s.equals("tcpvrf")) {
                tcpvrf = cmd.word();
                tcpvrf += " " + cmd.word();
                tcpvrf += " " + cmd.word();
                continue;
            }
            if (s.equals("iface")) {
                s = cmd.word();
                if (s.equals("socat")) {
                    ifaceType = ifcTyp.socat;
                }
                if (s.equals("pcap")) {
                    ifaceType = ifcTyp.pcap;
                }
                if (s.equals("raw")) {
                    ifaceType = ifcTyp.raw;
                }
                if (s.equals("map")) {
                    ifaceType = ifcTyp.map;
                }
                continue;
            }
            if (s.equals("line")) {
                s = cmd.word();
                if (s.equals("socat")) {
                    lineType = ifcTyp.socat;
                }
                if (s.equals("raw")) {
                    lineType = ifcTyp.raw;
                }
                continue;
            }
            if (s.equals("inline")) {
                inlineLoop = true;
                continue;
            }
            if (s.equals("external")) {
                inlineLoop = false;
                continue;
            }
            if (s.equals("binary")) {
                binMain = true;
                continue;
            }
            if (s.equals("java")) {
                binMain = false;
                continue;
            }
            if (s.equals("hwid")) {
                hwId = cmd.word();
                continue;
            }
            cmd.badCmd();
            return;
        }
        nextPort = 20000;
        ifcNum = 0;
        linNum = 0;
        starter = new ArrayList<String>();
        config = new ArrayList<String>();
        config.add("hwid " + hwId);
        starter.add(scrBeg);
        starter.add("");
        starter.add("cd " + path);
        starter.add("echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6");
        starter.add("echo 1 > /proc/sys/net/ipv6/conf/default/disable_ipv6");
        starter.add("echo 0 > /proc/sys/net/ipv6/conf/lo/disable_ipv6");
        starter.add("ip link set lo up mtu 65535");
        starter.add("ip addr add 127.0.0.1/8 dev lo");
        starter.add("ip addr add ::1/128 dev lo");
        starter.add("ulimit -c unlimited");
        starter.add("#modprobe -r kvm_intel");
        starter.add("#modprobe kvm_intel nested=1");
        starter.add("#echo 1 > /sys/kernel/mm/ksm/run");
        starter.add("#modprobe uio_pci_generic");
        starter.add("#echo 64 > /proc/sys/vm/nr_hugepages");
        starter.add("#modprobe vfio-pci");
        starter.add("#dpdk-devbind.py -b vfio-pci 00:03.0");
        detectMacs(path + lstMac);
        detectIfaces(path + lstEth);
        detectCrosses(cross);
        detectTuntap(tuntap);
        detectTcpvrf(tcpvrf);
        inlineLoop = false;
        detectLines(path + lstSer);
        addComment("main");
        String s;
        if (binMain) {
            s = path + "rtr.bin";
        } else {
            s = "java -Xmx" + mem + " -jar " + version.getFileName();
        }
        List<String> lop = new ArrayList<String>();
        lop.add("cd " + path);
        lop.add("stty raw < /dev/tty");
        lop.add(s + " router " + path + "rtr-");
        lop.add("if [ $? -eq 4 ] ; then");
        lop.add("  sync");
        lop.add("  reboot -f");
        lop.add("fi");
        lop.add("stty cooked < /dev/tty");
        makeLoop("main.sh", bits.str2lst(""), lop);
        starter.add("exit 0");
        bits.buf2txt(true, config, path + "rtr-" + cfgInit.hwCfgEnd);
        bits.buf2txt(true, starter, path + prefix + "all.sh");
        cmd.error("iface=" + ifcNum + " macs=" + macLst.size() + " line=" + linNum + " cross=" + crsNum % 100 + " tuntap=" + tapNum % 100 + " mem=" + mem);
    }

}

class userHwdetIface implements Comparator<userHwdetIface> {

    public String name = "";

    public String mac = "";

    public int compare(userHwdetIface t, userHwdetIface t1) {
        return t.name.compareTo(t1.name);
    }

    public String toString() {
        return name + " " + mac;
    }

}

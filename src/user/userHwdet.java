package user;

import addr.addrIP;
import addr.addrPrefix;
import cfg.cfgInit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import tab.tabGen;
import util.bits;
import util.cmds;
import util.version;

/**
 * process hw detection
 *
 * @author matecsaba
 */
public class userHwdet {

    private enum ifcTyp {

        socat, pcap, raw, map
    }

    private int nextPort;

    private int ifcNum;

    private int linNum;

    private int crsNum = 10000;

    private int tapNum = 20000;

    private List<String> starter;

    private List<String> config;

    private String scrBeg = "#!/bin/sh";

    private String prefix = "hwdet-";

    private boolean binMain;

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
        bits.buf2txt(true, txt, fn);
        starter.add("/bin/busybox start-stop-daemon -S -b -x " + fn);
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
                cmd = "./pcapInt.bin " + nam + " " + p2 + " 127.0.0.1 " + p1 + " 127.0.0.1";
                break;
            case raw:
                cmd = "./rawInt.bin " + nam + " " + p2 + " 127.0.0.1 " + p1 + " 127.0.0.1";
                stat = "stat ";
                break;
            case map:
                cmd = "./mapInt.bin " + nam + " " + p2 + " 127.0.0.1 " + p1 + " 127.0.0.1";
                stat = "stat ";
                break;
        }
        List<String> ifc = bits.str2lst("sleep 5");
        ifc.add("ifconfig " + nam + " multicast allmulti promisc mtu 1500 up");
        ifc.add("ethtool -K " + nam + " rx off");
        ifc.add("ethtool -K " + nam + " tx off");
        ifc.add("ethtool -K " + nam + " sg off");
        ifc.add("ethtool -K " + nam + " tso off");
        ifc.add("ethtool -K " + nam + " ufo off");
        ifc.add("ethtool -K " + nam + " gso off");
        ifc.add("ethtool -K " + nam + " gro off");
        ifc.add("ethtool -K " + nam + " lro off");
        ifc.add("ethtool -K " + nam + " rxvlan off");
        ifc.add("ethtool -K " + nam + " txvlan off");
        ifc.add("ethtool -K " + nam + " ntuple off");
        ifc.add("ethtool -K " + nam + " rxhash off");
        ifc.add("ethtool --set-eee " + nam + " eee off");
        makeLoop(path + prefix + "ifc" + ifcNum + ".sh", ifc, bits.str2lst(cmd));
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
                cmd = "./ttyLin.bin " + nam + " " + p1;
                break;
            default:
                break;
        }
        makeLoop(path + prefix + "lin" + linNum + ".sh", bits.str2lst("./modem.bin " + nam + " \"speedset 9600\""), bits.str2lst(cmd));
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
        String iface = null;
        String macadr = null;
        List<String> res = bits.txt2buf(fn);
        if (res == null) {
            return;
        }
        for (int i = 0; i < res.size(); i++) {
            String a = res.get(i).trim();
            int o = a.indexOf(": flags=");
            int p = a.indexOf("  Link encap:");
            if (o >= 0) {
                iface = a.substring(0, o);
            }
            if (p >= 0) {
                iface = a.substring(0, p);
            }
            o = a.indexOf("HWaddr");
            p = a.indexOf("ether");
            if (o >= 0) {
                macadr = a.substring(o + 6, a.length());
            }
            if (p >= 0) {
                try {
                    macadr = a.substring(p + 5, a.indexOf("txqueue"));
                } catch (Exception e) {
                }
            }
            if (iface == null) {
                continue;
            }
            if (macadr == null) {
                continue;
            }
            userHwdetIface ntry = new userHwdetIface();
            ntry.name = iface.trim();
            ntry.mac = macadr.trim();
            macLst.add(ntry);
            iface = null;
            macadr = null;
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
        String fn = path + prefix + "tap" + tapNum + ".sh";
        if (ifaceType == ifcTyp.socat) {
            makeLoop(fn, bits.str2lst(""), bits.str2lst("socat TUN:" + tap + ",tun-name=tap" + tapNum + ",tun-type=tap,iff-no-pi,iff-broadcast,iff-up" + " UDP4-DATAGRAM:127.0.0.1:" + p1 + ",bind=127.0.0.1:" + p2 + ",reuseaddr"));
        } else {
            addrPrefix<addrIP> prf = addrPrefix.str2ip(tap);
            prf.network.fromString(tap.substring(0, tap.indexOf("/")));
            makeLoop(fn, bits.str2lst(""), bits.str2lst("./tapInt.bin tap" + tapNum + " " + p2 + " 127.0.0.1 " + p1 + " 127.0.0.1 " + prf.network.toIPv4() + " " + prf.mask.toIPv4()));
        }
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
        starter.add("ifconfig lo up 127.0.0.1");
        starter.add("#modprobe -r kvm_intel");
        starter.add("#modprobe kvm_intel nested=1");
        starter.add("#echo 1 > /sys/kernel/mm/ksm/run");
        detectMacs(path + lstMac);
        detectIfaces(path + lstEth);
        detectCrosses(cross);
        detectTuntap(tuntap);
        detectTcpvrf(tcpvrf);
        detectLines(path + lstSer);
        addComment("main");
        String s;
        if (binMain) {
            s = "./rtr.bin";
        } else {
            s = "java -Xmx" + mem + " -jar " + version.getFileName();
        }
        List<String> lop = bits.str2lst(s + " router " + path + "rtr-");
        lop.add("if [[ $? = 4 ]] ; then");
        lop.add("  sync");
        lop.add("  reboot -f");
        lop.add("else");
        lop.add("  sleep 1");
        lop.add("fi");
        makeLoop(path + prefix + "main.sh", bits.str2lst("cd " + path), lop);
        starter.add("");
        starter.add("sleep 5");
        starter.add("route add default gw 10.255.255.254");
        starter.add("echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6");
        starter.add("echo 0 > /proc/sys/net/ipv6/conf/tap20001/disable_ipv6");
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

package org.freertr.user;

import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgInit;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.cmds;

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

    /**
     * interface types
     */
    public enum ifcTyp {
        /**
         * socat
         */
        socat,
        /**
         * pcap
         */
        pcap,
        /**
         * raw
         */
        raw,
        /**
         * map
         */
        map,
        /**
         * uring
         */
        urng,
        /**
         * xsk
         */
        xsk,
        /**
         * bsd
         */
        bsd,
        /**
         * cmp1
         */
        cmp1,
        /**
         * cmp2
         */
        cmp2,
    }

    /**
     * beginning of scripts
     */
    public static String scrBeg = "#!/bin/sh";

    private int nextPort = 0;

    private int ifcNum = 0;

    private int linNum = 0;

    private int crsNum = 10000;

    private int tapNum = 20000;

    private List<String> starter;

    private List<String> config;

    private String prefix = "hwdet-";

    private boolean binMain = false;

    private boolean inlineLoop = false;

    private boolean busyWait = false;

    private ifcTyp ifaceType = ifcTyp.socat;

    private ifcTyp lineType = ifcTyp.socat;

    private String path = "./";

    private String lstSer = "hwdet.ser";

    private String lstEth = "hwdet.eth";

    private String exclIfc = "";

    private String exclSer = "";

    private String inclIfc = "";

    private String inclSer = "";

    private String justIfc = "";

    private String justSer = "";

    private void addComment(String s) {
        starter.add("");
        starter.add("### " + s + " ###");
        starter.add("echo starting " + s + ".");
    }

    private void makeLoop(String fn, List<String> pre, List<String> lop, boolean fin) {
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
        if (!busyWait) {
            starter.add("start-stop-daemon -S -b -x " + path + prefix + fn);
            return;
        }
        if (!fin) {
            starter.add(path + prefix + fn + " > /dev/null &");
            return;
        }
        starter.add(path + prefix + fn + " > /dev/null");
    }

    private void makeLoop(String fn, List<String> pre, String cmd) {
        if (!inlineLoop) {
            makeLoop(fn, pre, bits.str2lst(cmd), false);
            return;
        }
        starter.addAll(pre);
        config.add("proc " + fn + " " + cmd);
    }

    /**
     * string to interface type
     *
     * @param s string to convert
     * @return interface type
     */
    public static ifcTyp string2type(String s) {
        if (s.equals("socat")) {
            return ifcTyp.socat;
        }
        if (s.equals("pcap")) {
            return ifcTyp.pcap;
        }
        if (s.equals("raw")) {
            return ifcTyp.raw;
        }
        if (s.equals("map")) {
            return ifcTyp.map;
        }
        if (s.equals("xsk")) {
            return ifcTyp.xsk;
        }
        if (s.equals("bsd")) {
            return ifcTyp.bsd;
        }
        if (s.equals("cmp1")) {
            return ifcTyp.cmp1;
        }
        if (s.equals("cmp2")) {
            return ifcTyp.cmp2;
        }
        if (s.equals("urng")) {
            return ifcTyp.urng;
        }
        return null;
    }

    /**
     * set up veth
     *
     * @param lst list
     * @param pth path
     * @param typ type
     * @param n1 first name
     * @param n2 second name
     */
    public static void setupVeth(List<String> lst, String pth, ifcTyp typ, String n1, String n2) {
        if (typ != ifcTyp.socat) {
            lst.add(pth + "veth.bin " + n1 + " " + n2);
            return;
        }
        lst.add("ip link add " + n1 + " type veth peer name veth0");
        lst.add("ip link set veth0 name " + n2);
    }

    /**
     * set up interface
     *
     * @param lst list
     * @param nam name
     */
    public static void routeIface(List<String> lst, String nam) {
        lst.add("ip addr add 10.255.255.1/24 dev " + nam + "");
        lst.add("ip route add 0.0.0.0/0 via 10.255.255.254 dev " + nam + "");
        lst.add("echo 0 > /proc/sys/net/ipv6/conf/" + nam + "/disable_ipv6");
    }

    /**
     * set up interface
     *
     * @param lst list
     * @param pth path
     * @param typ type
     * @param nam name
     * @param mtu mtu value
     * @param mac mac value
     */
    public static void setupIface(List<String> lst, String pth, ifcTyp typ, String nam, int mtu, String mac) {
        switch (typ) {
            case bsd:
            case cmp1:
            case cmp2:
                if (mac == null) {
                    mac = "";
                } else {
                    mac = " hw ether " + mac;
                }
                lst.add("ndp -i " + nam + " disabled -auto_linklocal");
                lst.add("ifconfig " + nam + " mtu " + mtu + mac + " up");
                return;
            case socat:
                if (mac == null) {
                    mac = "";
                } else {
                    mac = " address " + mac;
                }
                lst.add("ip link set " + nam + " up multicast on promisc on mtu " + mtu + mac);
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
                lst.add("ethtool " + "--set-eee " + nam + " eee off");
                return;
            default:
                if (mac == null) {
                    mac = "";
                } else {
                    mac = " " + mac;
                }
                lst.add(pth + "seth.bin " + nam + " " + mtu + mac);
                return;
        }
    }

    /**
     * interface to command
     *
     * @param path path of tools
     * @param typ interface type
     * @param nam name of interface
     * @param ps port to send
     * @param pb port to bind
     * @return command to execute
     */
    public static String interface2command(String path, ifcTyp typ, String nam, int ps, int pb) {
        switch (typ) {
            case socat:
                return "socat INTERFACE:" + nam + " UDP4-DATAGRAM:127.0.0.1:" + ps + ",bind=127.0.0.1:" + pb + ",reuseaddr";
            case pcap:
                return path + "pcapInt.bin " + nam + " " + pb + " 127.0.0.1 " + ps + " 127.0.0.1";
            case raw:
                return path + "rawInt.bin " + nam + " " + pb + " 127.0.0.1 " + ps + " 127.0.0.1";
            case map:
                return path + "mapInt.bin " + nam + " " + pb + " 127.0.0.1 " + ps + " 127.0.0.1";
            case xsk:
                return path + "xskInt.bin " + nam + " skb " + pb + " 127.0.0.1 " + ps + " 127.0.0.1";
            case bsd:
                return path + "bsdInt.bin " + nam + " " + pb + " 127.0.0.1 " + ps + " 127.0.0.1";
            case cmp1:
                return path + "cmp1int.bin " + nam + " " + pb + " 127.0.0.1 " + ps + " 127.0.0.1";
            case cmp2:
                return path + "cmp2int.bin " + nam + " " + pb + " 127.0.0.1 " + ps + " 127.0.0.1";
            case urng:
                return path + "urngInt.bin " + nam + " " + pb + " 127.0.0.1 " + ps + " 127.0.0.1";
            default:
                return null;
        }
    }

    /**
     * interface to command
     *
     * @param path path of tools
     * @param typ interface type
     * @param nam1 name of interface
     * @param nam2 name of interface
     * @return command to execute
     */
    public static String connection2command(String path, ifcTyp typ, String nam1, String nam2) {
        switch (typ) {
            case socat:
                return "socat INTERFACE:" + nam1 + " INTERFACE:" + nam2;
            case pcap:
                return path + "pcap2pcap.bin " + nam1 + " " + nam2;
            default:
                return null;
        }
    }

    /**
     * interface state commands
     *
     * @param typ interface type
     * @return state capability
     */
    public static String interface2stats(ifcTyp typ) {
        switch (typ) {
            case raw:
                return "stat ";
            case map:
                return "stat ";
            case urng:
                return "stat ";
            default:
                return "";
        }
    }

    private void createIface(String nam, String adr) {
        ifcNum += 1;
        int p1 = nextPort + 1;
        int p2 = nextPort + 2;
        nextPort += 10;
        String stat = interface2stats(ifaceType);
        String cmd = interface2command(path, ifaceType, nam, p1, p2);
        List<String> ifc = new ArrayList<String>();
        setupIface(ifc, path, ifaceType, nam, 1500, null);
        makeLoop("ifc" + ifcNum + ".sh", ifc, cmd);
        config.add("int " + "ether" + ifcNum + " " + stat + "eth " + adr + " 127.0.0.1 " + p1 + " 127.0.0.1 " + p2);
    }

    private void createLine(String nam) {
        linNum += 1;
        int p1 = nextPort + 1;
        int p2 = nextPort + 2;
        nextPort += 10;
        String cmd;
        switch (lineType) {
            case socat:
                cmd = "socat TCP4-LISTEN:" + p1 + ",reuseaddr FILE:" + nam + ",sane,b9600,cs8,raw,echo=0,crtscts=0";
                break;
            case raw:
                cmd = path + "ttyLin.bin " + nam + " " + p1;
                break;
            default:
                return;
        }
        makeLoop("lin" + linNum + ".sh", bits.str2lst(path + "modem.bin " + nam + " \"speedset 9600\" \"ctrlset 3\""), cmd);
        config.add("line tty" + linNum + " 127.0.0.1 " + p2 + " 127.0.0.1 " + p1);
    }

    private void createIfaces(String s) {
        cmds cmd = new cmds("lst", s);
        for (;;) {
            if (cmd.size() < 1) {
                break;
            }
            s = cmd.word("/");
            if (s.length() < 1) {
                continue;
            }
            createIface(s, "-");
        }
    }

    private void createLines(String s) {
        cmds cmd = new cmds("lst", s);
        for (;;) {
            if (cmd.size() < 1) {
                break;
            }
            s = cmd.word("/");
            if (s.length() < 1) {
                continue;
            }
            createLine(s);
        }
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

    private void detectIfaces(String fn) {
        List<String> res = bits.txt2buf(fn);
        if (res == null) {
            return;
        }
        List<userHwifc> lst = new ArrayList<userHwifc>();
        for (int i = 0; i < res.size(); i++) {
            userHwifc ntry = userHwifc.fromRaw(res, i);
            if (ntry == null) {
                continue;
            }
            lst.add(ntry);
        }
        starter.add("");
        starter.add("### macs ###");
        for (int i = 0; i < lst.size(); i++) {
            starter.add("# " + lst.get(i) + " #");
        }
        addComment("interfaces");
        if (justIfc.length() > 0) {
            createIfaces(justIfc);
            return;
        }
        for (int i = 0; i < lst.size(); i++) {
            userHwifc ntry = lst.get(i);
            if (exclIfc.indexOf("/" + ntry.name + "/") >= 0) {
                continue;
            }
            createIface(ntry.name, ntry.mac);
        }
        createIfaces(inclIfc);
    }

    private void detectLines(String fn) {
        addComment("lines");
        if (justSer.length() > 0) {
            createLines(justSer);
            return;
        }
        List<String> res = bits.txt2buf(fn);
        if (res == null) {
            return;
        }
        for (int cnt = 0; cnt < res.size(); cnt++) {
            String s = res.get(cnt);
            int i = s.indexOf(":");
            if (i < 0) {
                continue;
            }
            s = s.substring(0, i).trim();
            if (exclSer.indexOf("/" + s + "/") >= 0) {
                continue;
            }
            createLine(s);
        }
        createLines(inclSer);
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

    private void doReboot(List<String> lst, int cod) {
        lst.add("if [ $? -eq " + cod + " ] ; then");
        lst.add("  sync");
        switch (ifaceType) {
            case bsd:
            case cmp1:
            case cmp2:
                lst.add("  reboot");
                break;
            default:
                lst.add("  reboot -f");
                break;
        }
        lst.add("fi");
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
        String java = "";
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
            if (s.equals("java")) {
                java = cmd.word();
                continue;
            }
            if (s.equals("nojava")) {
                java = "";
                continue;
            }
            if (s.equals("ser")) {
                lstSer = cmd.word();
                continue;
            }
            if (s.equals("eth")) {
                lstEth = cmd.word();
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
                ifaceType = string2type(s);
                continue;
            }
            if (s.equals("line")) {
                s = cmd.word();
                lineType = string2type(s);
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
            if (s.equals("busywait")) {
                busyWait = true;
                continue;
            }
            if (s.equals("daemons")) {
                busyWait = false;
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
            if (s.equals("exclifc")) {
                exclIfc = "/" + cmd.word() + "/";
                continue;
            }
            if (s.equals("exclser")) {
                exclSer = "/" + cmd.word() + "/";
                continue;
            }
            if (s.equals("inclifc")) {
                inclIfc = "/" + cmd.word() + "/";
                continue;
            }
            if (s.equals("inclser")) {
                inclSer = "/" + cmd.word() + "/";
                continue;
            }
            if (s.equals("justifc")) {
                justIfc = "/" + cmd.word() + "/";
                continue;
            }
            if (s.equals("justser")) {
                justSer = "/" + cmd.word() + "/";
                continue;
            }
            cmd.badCmd();
            return;
        }
        nextPort = 20000;
        ifcNum = 0;
        linNum = 0;
        String rtr;
        if (binMain) {
            rtr = path + "rtr.bin";
        } else {
            rtr = java + "java -Xmx" + mem + " -jar " + cfgInit.getFileName();
        }
        starter = new ArrayList<String>();
        config = new ArrayList<String>();
        config.add("hwid " + hwId);
        starter.add(scrBeg);
        starter.add("");
        starter.add("cd " + path);
        switch (ifaceType) {
            case bsd:
            case cmp1:
            case cmp2:
                starter.add("ifconfig -a > " + lstEth);
                break;
            default:
                starter.add("ip link show > " + lstEth);
                break;
        }
        starter.add(rtr + " test hwred path " + path + " eth " + lstEth);
        doReboot(starter, 20);
        switch (ifaceType) {
            case bsd:
            case cmp1:
            case cmp2:
                starter.add("ifconfig lo0 127.0.0.1 mtu 65535 up");
                break;
            default:
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
                break;
        }
        detectIfaces(path + lstEth);
        detectCrosses(cross);
        detectTuntap(tuntap);
        detectTcpvrf(tcpvrf);
        inlineLoop = false;
        detectLines(path + lstSer);
        addComment("main");
        List<String> lop = new ArrayList<String>();
        lop.add("cd " + path);
        lop.add("stty raw < /dev/tty");
        lop.add(rtr + " router " + path + "rtr-");
        doReboot(lop, 4);
        lop.add("stty cooked < /dev/tty");
        makeLoop("main.sh", bits.str2lst(""), lop, true);
        starter.add("exit 0");
        bits.buf2txt(true, config, path + "rtr-" + cfgInit.hwCfgEnd);
        bits.buf2txt(true, starter, path + prefix + "all.sh");
        cmd.error("iface=" + ifcNum + " line=" + linNum + " cross=" + crsNum % 100 + " tuntap=" + tapNum % 100 + " mem=" + mem);
    }

}

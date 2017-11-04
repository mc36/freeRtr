package cfg;

import addr.addrMac;
import ifc.ifcUdpInt;
import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pipe.pipeConnect;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeShell;
import pipe.pipeSide;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.version;

/**
 * one vdc configuration
 *
 * @author matecsaba
 */
public class cfgVdc implements Comparator<cfgVdc>, Runnable, cfgGeneric {

    /**
     * name of this vdc
     */
    public String name;

    /**
     * description of this interface
     */
    public String description = "";

    /**
     * image to use
     */
    public String image1name = null;

    /**
     * image to use
     */
    public String image2name = null;

    /**
     * image to use
     */
    public String image3name = null;

    /**
     * image to use
     */
    public String image4name = null;

    /**
     * bios to use
     */
    public String biosName = null;

    /**
     * cdrom to use
     */
    public String cdromName = null;

    /**
     * uuid to use
     */
    public String uuidValue = null;

    /**
     * memory to give
     */
    public int imageMem = 512;

    /**
     * cpus to give
     */
    public int imageCpu = 1;

    /**
     * mac address base
     */
    public addrMac macBase = null;

    /**
     * nic type
     */
    public String nicType = "e1000";

    /**
     * list of interfaces
     */
    public final tabGen<cfgVdcIfc> ifaces = new tabGen<cfgVdcIfc>();

    /**
     * list of local connects
     */
    public final tabGen<cfgVdcIfc> locals = new tabGen<cfgVdcIfc>();

    /**
     * list of connections
     */
    public final tabGen<cfgVdcConn> conns = new tabGen<cfgVdcConn>();

    /**
     * console pipeline
     */
    public pipeSide con;

    /**
     * restart count
     */
    public int restartC;

    /**
     * restart time
     */
    public long restartT;

    private pipeShell proc;

    private pipeSide pipe;

    private String cfgBase;

    private boolean need2run;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "vdc definition .*! no description",
        "vdc definition .*! image null",
        "vdc definition .*! disk2 null",
        "vdc definition .*! disk3 null",
        "vdc definition .*! disk4 null",
        "vdc definition .*! cdrom null",
        "vdc definition .*! bios null",
        "vdc definition .*! uuid null",
        "vdc definition .*! mac null",
        "vdc definition .*! memory 512",
        "vdc definition .*! cores 1",
        "vdc definition .*! nic e1000"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgVdc o1, cfgVdc o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "vdc " + name;
    }

    /**
     * create new vdc
     *
     * @param nam name of vdc
     */
    public cfgVdc(String nam) {
        name = nam.trim();
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public cfgVdc copyBytes() {
        cfgVdc n = new cfgVdc(name);
        n.description = description;
        n.uuidValue = uuidValue;
        n.image1name = image1name;
        n.image2name = image2name;
        n.image3name = image3name;
        n.image4name = image4name;
        n.imageMem = imageMem;
        n.imageCpu = imageCpu;
        n.nicType = nicType;
        n.biosName = biosName;
        n.cdromName = cdromName;
        if (macBase != null) {
            n.macBase = macBase.copyBytes();
        }
        for (int i = 0; i < ifaces.size(); i++) {
            n.ifaces.add(ifaces.get(i));
        }
        for (int i = 0; i < conns.size(); i++) {
            n.conns.add(conns.get(i));
        }
        for (int i = 0; i < locals.size(); i++) {
            n.locals.add(locals.get(i));
        }
        return n;
    }

    /**
     * delete connection
     *
     * @param ifc connecting interface
     */
    protected void delConn(String ifc) {
        cfgVdcConn ntry = conns.del(new cfgVdcConn(ifc, this));
        if (ntry == null) {
            return;
        }
        ntry.peer.conns.del(ntry);
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2,. description        description of this vdc");
        l.add("2 2,.   [text]           text describing this vdc");
        l.add("1 2  rename              rename this vdc");
        l.add("2 .    <name>            set new name of vdc");
        l.add("1 2  interface           add interface to this vdc");
        l.add("2 .    <name>            name of interface");
        l.add("1 2  connect             add connection to other vdc");
        l.add("2 3    <name>            name of interface");
        l.add("3 .      <name>          name of peer vdc");
        l.add("1 2  local               add connection to this vdc");
        l.add("2 3,.  <name>            name of interface");
        l.add("3 .      redundancy      flagged for redundancy");
        l.add("1 2  bios                set bios image to use");
        l.add("2 2,.  <name>            name of image");
        l.add("1 2  image               set external image to use");
        l.add("2 2,.  <name>            name of image");
        l.add("1 2  disk2               set external image to use");
        l.add("2 2,.  <name>            name of image");
        l.add("1 2  disk3               set external image to use");
        l.add("2 2,.  <name>            name of image");
        l.add("1 2  disk4               set external image to use");
        l.add("2 2,.  <name>            name of image");
        l.add("1 2  cdrom               set cdrom image to use");
        l.add("2 2,.  <name>            name of image");
        l.add("1 2  uuid                set uuid to use");
        l.add("2 .    <name>            uuid value");
        l.add("1 2  memory              memory of vdc");
        l.add("2 .    <num>             megabytes");
        l.add("1 2  cores               cpu of vdc");
        l.add("2 .    <num>             cores");
        l.add("1 2  mac                 mac address base");
        l.add("2 .    <addr>            address");
        l.add("1 2  nic                 type of nic");
        l.add("2 .    <name>            vendor");
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("vdc definition " + name);
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        for (int i = 0; i < ifaces.size(); i++) {
            l.add(cmds.tabulator + "interface " + ifaces.get(i));
        }
        for (int i = 0; i < locals.size(); i++) {
            l.add(cmds.tabulator + "local " + locals.get(i));
        }
        for (int i = 0; i < conns.size(); i++) {
            l.add(cmds.tabulator + "connect " + conns.get(i));
        }
        l.add(cmds.tabulator + "uuid " + uuidValue);
        l.add(cmds.tabulator + "bios " + biosName);
        l.add(cmds.tabulator + "image " + image1name);
        l.add(cmds.tabulator + "disk2 " + image2name);
        l.add(cmds.tabulator + "disk3 " + image3name);
        l.add(cmds.tabulator + "disk4 " + image4name);
        l.add(cmds.tabulator + "cdrom " + cdromName);
        l.add(cmds.tabulator + "memory " + imageMem);
        l.add(cmds.tabulator + "cores " + imageCpu);
        l.add(cmds.tabulator + "nic " + nicType);
        l.add(cmds.tabulator + "mac " + macBase);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("rename")) {
            a = cmd.word();
            cfgVdc v = cfgAll.vdcFind(a, false);
            if (v != null) {
                cmd.error("vdc already exists");
                return;
            }
            name = a;
            return;
        }
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("bios")) {
            biosName = cmd.getRemaining();
            return;
        }
        if (a.equals("image")) {
            image1name = cmd.getRemaining();
            return;
        }
        if (a.equals("disk2")) {
            image2name = cmd.getRemaining();
            return;
        }
        if (a.equals("disk3")) {
            image3name = cmd.getRemaining();
            return;
        }
        if (a.equals("disk4")) {
            image4name = cmd.getRemaining();
            return;
        }
        if (a.equals("uuid")) {
            uuidValue = cmd.word();
            return;
        }
        if (a.equals("cdrom")) {
            cdromName = cmd.getRemaining();
            return;
        }
        if (a.equals("memory")) {
            imageMem = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("cores")) {
            imageCpu = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("nic")) {
            nicType = cmd.word();
            return;
        }
        if (a.equals("mac")) {
            macBase = new addrMac();
            macBase.fromString(cmd.word());
            return;
        }
        if (a.equals("interface")) {
            cfgVdcIfc res = new cfgVdcIfc(cfgIfc.normName(cmd.word(), false), "");
            res = cfgInit.ifaceLst.find(res);
            if (res == null) {
                cmd.error("no such interface");
                return;
            }
            cfgAll.vdcNoIfc(res);
            ifaces.add(res);
            return;
        }
        if (a.equals("local")) {
            a = cfgIfc.normName(cmd.word(), false);
            cfgIfc.ifaceType typ = cfgIfc.string2type(a);
            if (typ == null) {
                cmd.error("bad name");
                return;
            }
            if (cfgInit.ifaceLst.find(new cfgVdcIfc(a, "")) != null) {
                cmd.error("physical interface exists");
                return;
            }
            if (conns.find(new cfgVdcConn(a, this)) != null) {
                cmd.error("connection exists");
                return;
            }
            cfgVdcIfc ntry = new cfgVdcIfc(a, "");
            ntry.redundancy = cmd.word().equals("redundancy");
            locals.add(ntry);
            if (!cfgInit.booting) {
                return;
            }
            if (cfgAll.ifcFind(a, false) != null) {
                return;
            }
            ntry.port = cfgInit.vdcPortBeg;
            ifcUdpInt hdr = new ifcUdpInt("127.0.0.1", cfgInit.vdcPortBeg, "127.0.0.1", cfgInit.vdcPortBeg + 1, "-", typ != cfgIfc.ifaceType.ether, false);
            cfgIfc ifc = cfgAll.ifcAdd(a, typ, hdr);
            ifc.initPhysical();
            if (debugger.cfgInitHw) {
                logger.debug("iface " + hdr);
            }
            cfgInit.vdcPortBeg += 2;
            return;
        }
        if (a.equals("connect")) {
            a = cfgIfc.normName(cmd.word(), false);
            if (cfgIfc.string2type(a) == null) {
                cmd.error("bad name");
                return;
            }
            if (cfgInit.ifaceLst.find(new cfgVdcIfc(a, "")) != null) {
                cmd.error("physical interface exists");
                return;
            }
            if (locals.find(new cfgVdcIfc(a, "")) != null) {
                cmd.error("local exists");
                return;
            }
            cfgVdc peer = cfgAll.vdcFind(cmd.word(), false);
            if (peer == null) {
                cmd.error("no such vdc");
                return;
            }
            if (peer.locals.find(new cfgVdcIfc(a, "")) != null) {
                cmd.error("peer local exists");
                return;
            }
            delConn(a);
            peer.delConn(a);
            cfgVdcConn c1 = new cfgVdcConn(a, peer);
            cfgVdcConn c2 = new cfgVdcConn(a, this);
            c1.conn = c2;
            c2.conn = c1;
            conns.add(c1);
            peer.conns.add(c2);
            return;
        }
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("description")) {
            description = "";
            return;
        }
        if (a.equals("bios")) {
            biosName = null;
            return;
        }
        if (a.equals("image")) {
            image1name = null;
            return;
        }
        if (a.equals("disk2")) {
            image2name = null;
            return;
        }
        if (a.equals("disk3")) {
            image3name = null;
            return;
        }
        if (a.equals("disk4")) {
            image4name = null;
            return;
        }
        if (a.equals("uuid")) {
            uuidValue = null;
            return;
        }
        if (a.equals("cdrom")) {
            cdromName = null;
            return;
        }
        if (a.equals("memory")) {
            imageMem = 512;
            return;
        }
        if (a.equals("cores")) {
            imageCpu = 1;
            return;
        }
        if (a.equals("nic")) {
            nicType = "e1000";
            return;
        }
        if (a.equals("mac")) {
            macBase = null;
            return;
        }
        if (a.equals("interface")) {
            ifaces.del(new cfgVdcIfc(cfgIfc.normName(cmd.word(), false), ""));
            return;
        }
        if (a.equals("local")) {
            locals.del(new cfgVdcIfc(cfgIfc.normName(cmd.word(), false), ""));
            return;
        }
        if (a.equals("connect")) {
            delConn(cfgIfc.normName(cmd.word(), false));
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "vdc";
    }

    public void run() {
        for (;;) {
            try {
                restartT = bits.getTime();
                doRound();
                restartC++;
                if (!need2run) {
                    break;
                }
                bits.sleep(5000);
                logger.info("restarting vdc " + name);
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
        logger.info("stopped vdc " + name);
    }

    private void doRound() {
        pipeLine pl = new pipeLine(65536, false);
        pipe = pl.getSide();
        String cmd = null;
        addrMac mac;
        if (macBase == null) {
            mac = addrMac.getRandom();
        } else {
            mac = macBase.copyBytes();
        }
        addrMac one = new addrMac();
        one.fromString("0000:0000:0001");
        if (image1name == null) {
            cmd = "java " + cfgInit.jvmParam + " -Xmx" + imageMem + "m -jar " + version.getFileName() + " routerc " + cfgBase;
        } else {
            cmd = "qemu-system-x86_64 -monitor none -serial stdio -nographic -no-reboot -enable-kvm -hda " + image1name + " -m " + imageMem;
            if (biosName != null) {
                cmd += " -bios " + biosName;
            }
            if (cdromName != null) {
                cmd += " -cdrom " + cdromName;
            }
            if (image2name != null) {
                cmd += " -hdb " + image2name;
            }
            if (image3name != null) {
                cmd += " -hdc " + image3name;
            }
            if (image4name != null) {
                cmd += " -hdd " + image4name;
            }
            if (uuidValue != null) {
                cmd += " -uuid " + uuidValue;
            }
            if (imageCpu > 1) {
                cmd += " -smp cores=" + imageCpu + ",threads=1,sockets=1";
            }
            int vl = 1;
            for (int i = 0; i < locals.size(); i++) {
                cfgVdcIfc ntry = locals.get(i);
                cmd += " -net nic,vlan=" + vl + ",model=" + nicType + ",macaddr=" + mac.toEmuStr() + " -net socket,vlan=" + vl + ",udp=:" + ntry.port + ",localaddr=:" + (ntry.port + 1);
                vl++;
                mac.setAdd(mac, one);
            }
            for (int i = 0; i < conns.size(); i++) {
                cfgVdcConn ntry = conns.get(i);
                cmd += " -net nic,vlan=" + vl + ",macaddr=" + mac.toEmuStr() + " -net socket,vlan=" + vl + ",udp=:" + ntry.conn.port + ",localaddr=:" + ntry.port;
                vl++;
                mac.setAdd(mac, one);
            }
        }
        proc = pipeShell.exec(pl.getSide(), cmd, null, true, true);
        if (proc == null) {
            return;
        }
        for (;;) {
            if (!proc.isRunning()) {
                break;
            }
            if (con == null) {
                pipeDiscard.flush(pipe);
                bits.sleep(1000);
                continue;
            }
            boolean b = pipeConnect.redirect(pipe, con);
            b |= pipeConnect.redirect(con, pipe);
            if (b) {
                con.setClose();
                con = null;
            }
            bits.sleep(100);
        }
    }

    /**
     * destroy this vdc
     */
    public synchronized void stopNow() {
        need2run = false;
        restartNow();
    }

    /**
     * restart this vdc
     */
    public synchronized void restartNow() {
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
        try {
            proc.kill(0);
        } catch (Exception e) {
        }
    }

    /**
     * start this vdc now
     *
     * @param defs defaults
     * @param mibs snmp mibs
     * @param beg vdc begin port
     * @param end vdc end port
     */
    public synchronized void startNow(List<String> defs, List<String> mibs,
            int beg, int end) {
        logger.info("starting vdc " + name);
        need2run = true;
        cfgBase = cfgInit.cfgFileSw;
        int i = cfgBase.lastIndexOf("/");
        if (i < 0) {
            cfgBase = "";
        } else {
            cfgBase = cfgBase.substring(0, i + 1);
        }
        cfgBase += "vdc-" + name + "-";
        List<String> l = new ArrayList<String>();
        l.add("hwid " + cfgInit.hwIdNum + "-" + name);
        l.add("port " + beg + " " + end);
        l.add("jvm " + cfgInit.jvmParam);
        l.add("url " + cfgAll.upgradeServer);
        for (i = 0; i < defs.size(); i++) {
            l.add("def " + defs.get(i));
        }
        for (i = 0; i < mibs.size(); i++) {
            l.add("snmp " + mibs.get(i));
        }
        for (i = 0; i < ifaces.size(); i++) {
            cfgVdcIfc ntry = ifaces.get(i);
            l.add("int " + ntry.line);
            cfgIfc ifc = cfgAll.ifcFind(ntry.name, false);
            if (ifc == null) {
                continue;
            }
            cfgAll.ifcDel(ntry.name, false);
            try {
                ifc.thread.closeDn();
            } catch (Exception e) {
            }
        }
        addrMac mac;
        if (macBase == null) {
            mac = addrMac.getRandom();
        } else {
            mac = macBase.copyBytes();
        }
        addrMac one = new addrMac();
        one.fromString("0000:0000:0001");
        for (i = 0; i < locals.size(); i++) {
            cfgVdcIfc ntry = locals.get(i);
            String a = "";
            if (ntry.redundancy) {
                a = " red";
            }
            l.add("int " + ntry.name + a + " " + ntry.name.substring(0, 3) + " " + mac + " 127.0.0.1 " + (ntry.port + 1) + " 127.0.0.1 " + ntry.port);
            mac.setAdd(mac, one);
        }
        for (i = 0; i < conns.size(); i++) {
            cfgVdcConn ntry = conns.get(i);
            if (ntry.port < 1) {
                ntry.port = cfgInit.vdcPortBeg;
                ntry.conn.port = cfgInit.vdcPortBeg + 1;
                cfgInit.vdcPortBeg += 2;
            }
            l.add("int " + ntry.name + " " + ntry.name.substring(0, 3) + " " + mac + " 127.0.0.1 " + ntry.port + " 127.0.0.1 " + ntry.conn.port);
            mac.setAdd(mac, one);
        }
        bits.buf2txt(true, l, cfgBase + cfgInit.hwCfgEnd);
        if (!new File(cfgBase + cfgInit.swCfgEnd).exists()) {
            l = new ArrayList<String>();
            l.add("hostname " + cfgAll.hostName + "-" + name);
            bits.buf2txt(true, l, cfgBase + cfgInit.swCfgEnd);
        }
        new Thread(this).start();
    }

}

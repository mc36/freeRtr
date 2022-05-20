package net.freertr.cfg;

import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.ifc.ifcUdpInt;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeShell;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logBuf;
import net.freertr.util.logger;
import net.freertr.util.version;

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
     * description of this vdc
     */
    public String description = "";

    /**
     * respawn on termination
     */
    public boolean respawn = true;

    /**
     * kill children on termination
     */
    public boolean children = true;

    /**
     * action logging
     */
    public boolean logAct = false;

    /**
     * console logging
     */
    public boolean logCon = false;

    /**
     * console collector
     */
    public logBuf logCol;

    /**
     * config to use
     */
    public String configFile = null;

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
     * user to use
     */
    public String userValue = null;

    /**
     * cpu pinning
     */
    public String cpuPinning = null;

    /**
     * cpu type
     */
    public String cpuType = null;

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
     * time between runs
     */
    protected int interval = 1000;

    /**
     * initial delay
     */
    protected int initial = 1000;

    /**
     * redundancy priority
     */
    protected int redunPrio;

    /**
     * random time between runs
     */
    public int randInt;

    /**
     * random initial delay
     */
    public int randIni;

    /**
     * time range when allowed
     */
    public cfgTime time;

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
     * list of pci
     */
    public final tabGen<cfgVdcPci> pcis = new tabGen<cfgVdcPci>();

    /**
     * list of usb
     */
    public final tabGen<cfgVdcUsb> usbs = new tabGen<cfgVdcUsb>();

    /**
     * list of tcp
     */
    public final tabGen<cfgVdcTcp> tcps = new tabGen<cfgVdcTcp>();

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
    public final static String[] defaultL = {
        "vdc definition .*! no description",
        "vdc definition .*! respawn",
        "vdc definition .*! children",
        "vdc definition .*! config null",
        "vdc definition .*! image null",
        "vdc definition .*! disk2 null",
        "vdc definition .*! disk3 null",
        "vdc definition .*! disk4 null",
        "vdc definition .*! cdrom null",
        "vdc definition .*! bios null",
        "vdc definition .*! pinning null",
        "vdc definition .*! uuid null",
        "vdc definition .*! user null",
        "vdc definition .*! mac null",
        "vdc definition .*! cpu null",
        "vdc definition .*! memory 512",
        "vdc definition .*! priority 0",
        "vdc definition .*! cores 1",
        "vdc definition .*! nic e1000",
        "vdc definition .*! time 1000",
        "vdc definition .*! delay 1000",
        "vdc definition .*! no log-actions",
        "vdc definition .*! no log-console",
        "vdc definition .*! no log-collect",
        "vdc definition .*! random-time 0",
        "vdc definition .*! random-delay 0",
        "vdc definition .*! no range"
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
        n.redunPrio = redunPrio;
        n.respawn = respawn;
        n.children = children;
        n.uuidValue = uuidValue;
        n.userValue = userValue;
        n.cpuPinning = cpuPinning;
        n.cpuType = cpuType;
        n.initial = initial;
        n.interval = interval;
        n.randIni = randIni;
        n.randInt = randInt;
        n.time = time;
        n.configFile = configFile;
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
        for (int i = 0; i < pcis.size(); i++) {
            n.pcis.add(pcis.get(i));
        }
        for (int i = 0; i < usbs.size(); i++) {
            n.usbs.add(usbs.get(i));
        }
        for (int i = 0; i < tcps.size(); i++) {
            n.tcps.add(tcps.get(i));
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

    public void getHelp(userHelping l) {
        l.add(null, "1  2,.    description                description of this vdc");
        l.add(null, "2  2,.      [text]                   text describing this vdc");
        l.add(null, "1  .      respawn                    restart on termination");
        l.add(null, "1  .      children                   kill children on termination");
        l.add(null, "1  2      rename                     rename this vdc");
        l.add(null, "2  .        <str>                    set new name of vdc");
        l.add(null, "1  2      interface                  add interface to this vdc");
        l.add(null, "2  .        <name:ifc>               name of interface");
        l.add(null, "1  2      connect                    add connection to other vdc");
        l.add(null, "2  3        <name:ifc>               name of interface");
        l.add(null, "3  .          <name:vdc>             name of peer vdc");
        l.add(null, "1  2      local                      add connection to this vdc");
        l.add(null, "2  3,.      <name:ifc>               name of interface");
        l.add(null, "3  .          redundancy             flagged for redundancy");
        l.add(null, "1  2      config                     set config file to use");
        l.add(null, "2  2,.      <str>                    name of image");
        l.add(null, "1  2      bios                       set bios image to use");
        l.add(null, "2  2,.      <str>                    name of image");
        l.add(null, "1  2      image                      set external image to use");
        l.add(null, "2  2,.      <str>                    name of image");
        l.add(null, "1  2      disk2                      set external image to use");
        l.add(null, "2  2,.      <str>                    name of image");
        l.add(null, "1  2      disk3                      set external image to use");
        l.add(null, "2  2,.      <str>                    name of image");
        l.add(null, "1  2      disk4                      set external image to use");
        l.add(null, "2  2,.      <str>                    name of image");
        l.add(null, "1  2      cdrom                      set cdrom image to use");
        l.add(null, "2  2,.      <str>                    name of image");
        l.add(null, "1  2      uuid                       set uuid to use");
        l.add(null, "2  .        <str>                    uuid value");
        l.add(null, "1  2      user                       set user to use");
        l.add(null, "2  .        <str>                    user value");
        l.add(null, "1  2      pinning                    set pinning mask");
        l.add(null, "2  .        <str>                    cpu mask in hex");
        l.add(null, "1  2      cpu                        set cpu type");
        l.add(null, "2  .        <str>                    type parameters");
        l.add(null, "1  2      memory                     memory of vdc");
        l.add(null, "2  .        <num>                    megabytes");
        l.add(null, "1  2      priority                   specify redundancy priority");
        l.add(null, "2  .        <num>                    priority");
        l.add(null, "1  2      cores                      cpu of vdc");
        l.add(null, "2  .        <num>                    cores");
        l.add(null, "1  2      mac                        mac address base");
        l.add(null, "2  .        <addr>                   address");
        l.add(null, "1  2      nic                        type of nic");
        l.add(null, "2  .        <str>                    vendor");
        l.add(null, "1  2      pci                        pass through pci device");
        l.add(null, "2  3        <num>                    bus");
        l.add(null, "3  4          <num>                  device");
        l.add(null, "4  .            <num>                function");
        l.add(null, "1  2      usb                        pass through usb device");
        l.add(null, "2  3        <num>                    bus");
        l.add(null, "3  .          <num>                  port");
        l.add(null, "1  2      tcp2vrf                    pass host port in");
        l.add(null, "2  3        <num>                    host port");
        l.add(null, "3  4          <str>                  vdc vrf");
        l.add(null, "4  5,.          <num>                vdc port");
        l.add(null, "5  .              <addr>             host ip to bind to");
        l.add(null, "1  2      time                       specify time between runs");
        l.add(null, "2  .        <num>                    milliseconds between runs");
        l.add(null, "1  2      delay                      specify initial delay");
        l.add(null, "2  .        <num>                    milliseconds before start");
        l.add(null, "1  2      random-time                specify random time between runs");
        l.add(null, "2  .        <num>                    milliseconds between runs");
        l.add(null, "1  2      random-delay               specify random initial delay");
        l.add(null, "2  .        <num>                    milliseconds before start");
        l.add(null, "1  2      range                      specify time range");
        l.add(null, "2  .        <name:tm>                name of time map");
        l.add(null, "1  .      log-actions                log actions");
        l.add(null, "1  .      log-console                log console activity");
        l.add(null, "1  2      log-collect                collect console activity");
        l.add(null, "2  .        <num>                    lines to store");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("vdc definition " + name);
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        cmds.cfgLine(l, !respawn, cmds.tabulator, "respawn", "");
        cmds.cfgLine(l, !children, cmds.tabulator, "children", "");
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
        l.add(cmds.tabulator + "user " + userValue);
        l.add(cmds.tabulator + "pinning " + cpuPinning);
        l.add(cmds.tabulator + "cpu " + cpuType);
        l.add(cmds.tabulator + "bios " + biosName);
        l.add(cmds.tabulator + "config " + configFile);
        l.add(cmds.tabulator + "image " + image1name);
        l.add(cmds.tabulator + "disk2 " + image2name);
        l.add(cmds.tabulator + "disk3 " + image3name);
        l.add(cmds.tabulator + "disk4 " + image4name);
        l.add(cmds.tabulator + "cdrom " + cdromName);
        l.add(cmds.tabulator + "memory " + imageMem);
        l.add(cmds.tabulator + "priority " + redunPrio);
        l.add(cmds.tabulator + "cores " + imageCpu);
        l.add(cmds.tabulator + "nic " + nicType);
        l.add(cmds.tabulator + "mac " + macBase);
        for (int i = 0; i < pcis.size(); i++) {
            l.add(cmds.tabulator + "pci " + pcis.get(i));
        }
        for (int i = 0; i < usbs.size(); i++) {
            l.add(cmds.tabulator + "usb " + usbs.get(i));
        }
        for (int i = 0; i < tcps.size(); i++) {
            l.add(cmds.tabulator + "tcp2vrf " + tcps.get(i));
        }
        cmds.cfgLine(l, !logAct, cmds.tabulator, "log-actions", "");
        cmds.cfgLine(l, !logCon, cmds.tabulator, "log-console", "");
        cmds.cfgLine(l, logCol == null, cmds.tabulator, "log-collect", "" + logBuf.getSize(logCol));
        l.add(cmds.tabulator + "delay " + initial);
        l.add(cmds.tabulator + "time " + interval);
        l.add(cmds.tabulator + "random-time " + randInt);
        l.add(cmds.tabulator + "random-delay " + randIni);
        cmds.cfgLine(l, time == null, cmds.tabulator, "range", "" + time);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
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
        if (a.equals("respawn")) {
            respawn = true;
            return;
        }
        if (a.equals("children")) {
            children = true;
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
        if (a.equals("config")) {
            configFile = cmd.getRemaining();
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
        if (a.equals("user")) {
            userValue = cmd.word();
            return;
        }
        if (a.equals("pinning")) {
            cpuPinning = cmd.word();
            return;
        }
        if (a.equals("cpu")) {
            cpuType = cmd.word();
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
        if (a.equals("priority")) {
            redunPrio = bits.str2num(cmd.word());
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
            cfgVdcIfc res = new cfgVdcIfc(cfgIfc.dissectName(cmd.word())[0], "");
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
            a = cfgIfc.dissectName(cmd.word())[0];
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
            if (cfgAll.ifcFind(a, 0) != null) {
                return;
            }
            ntry.port = cfgInit.vdcPortBeg;
            ifcUdpInt hdr = new ifcUdpInt("127.0.0.1", cfgInit.vdcPortBeg, "127.0.0.1", cfgInit.vdcPortBeg + 1, "-", typ != cfgIfc.ifaceType.ether, false);
            cfgIfc ifc = cfgAll.ifcAdd(a, typ, hdr, 1);
            ifc.initPhysical();
            if (debugger.cfgInitHw) {
                logger.debug("iface " + hdr);
            }
            cfgInit.vdcPortBeg += 2;
            return;
        }
        if (a.equals("connect")) {
            a = cfgIfc.dissectName(cmd.word())[0];
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
        if (a.equals("pci")) {
            cfgVdcPci dev = new cfgVdcPci();
            dev.bus = bits.str2num(cmd.word());
            dev.dev = bits.str2num(cmd.word());
            dev.fnc = bits.str2num(cmd.word());
            pcis.add(dev);
            return;
        }
        if (a.equals("usb")) {
            cfgVdcUsb dev = new cfgVdcUsb();
            dev.bus = bits.str2num(cmd.word());
            dev.prt = bits.str2num(cmd.word());
            usbs.add(dev);
            return;
        }
        if (a.equals("tcp2vrf")) {
            cfgVdcTcp dev = new cfgVdcTcp();
            dev.portH = bits.str2num(cmd.word());
            dev.vrf = cmd.word();
            dev.portV = bits.str2num(cmd.word());
            if (cmd.size() > 1) {
                dev.adr = new addrIP();
                dev.adr.fromString(cmd.word());
            }
            tcps.add(dev);
            return;
        }
        if (a.equals("log-actions")) {
            logAct = true;
            return;
        }
        if (a.equals("log-collect")) {
            logCol = new logBuf(bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("log-console")) {
            logCon = true;
            return;
        }
        if (a.equals("delay")) {
            initial = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("time")) {
            interval = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("random-time")) {
            randInt = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("random-delay")) {
            randIni = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("range")) {
            time = cfgAll.timeFind(cmd.word(), false);
            return;
        }
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("respawn")) {
            respawn = false;
            return;
        }
        if (a.equals("children")) {
            children = false;
            return;
        }
        if (a.equals("description")) {
            description = "";
            return;
        }
        if (a.equals("bios")) {
            biosName = null;
            return;
        }
        if (a.equals("config")) {
            configFile = null;
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
        if (a.equals("user")) {
            userValue = null;
            return;
        }
        if (a.equals("pinning")) {
            cpuPinning = null;
            return;
        }
        if (a.equals("cpu")) {
            cpuType = null;
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
        if (a.equals("priority")) {
            redunPrio = 0;
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
            ifaces.del(new cfgVdcIfc(cfgIfc.dissectName(cmd.word())[0], ""));
            return;
        }
        if (a.equals("local")) {
            locals.del(new cfgVdcIfc(cfgIfc.dissectName(cmd.word())[0], ""));
            return;
        }
        if (a.equals("connect")) {
            delConn(cfgIfc.dissectName(cmd.word())[0]);
            return;
        }
        if (a.equals("pci")) {
            cfgVdcPci dev = new cfgVdcPci();
            dev.bus = bits.str2num(cmd.word());
            dev.dev = bits.str2num(cmd.word());
            dev.fnc = bits.str2num(cmd.word());
            pcis.del(dev);
            return;
        }
        if (a.equals("usb")) {
            cfgVdcUsb dev = new cfgVdcUsb();
            dev.bus = bits.str2num(cmd.word());
            dev.prt = bits.str2num(cmd.word());
            usbs.del(dev);
            return;
        }
        if (a.equals("tcp2vrf")) {
            cfgVdcTcp dev = new cfgVdcTcp();
            dev.portH = bits.str2num(cmd.word());
            dev.vrf = cmd.word();
            dev.portV = bits.str2num(cmd.word());
            tcps.del(dev);
            return;
        }
        if (a.equals("log-actions")) {
            logAct = false;
            return;
        }
        if (a.equals("log-collect")) {
            logCol = null;
            return;
        }
        if (a.equals("log-console")) {
            logCon = false;
            return;
        }
        if (a.equals("random-time")) {
            randInt = 0;
            return;
        }
        if (a.equals("random-delay")) {
            randIni = 0;
            return;
        }
        if (a.equals("range")) {
            time = null;
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "vdc";
    }

    public void run() {
        for (;;) {
            if (!cfgInit.booting) {
                break;
            }
            bits.sleep(1000);
        }
        int del = initial;
        if (randIni > 0) {
            del += bits.random(1, randIni);
        }
        if (del > 0) {
            bits.sleep(del);
        }
        for (;;) {
            bits.sleep(interval);
            if (!need2run) {
                break;
            }
            if (!respawn) {
                continue;
            }
            try {
                doRound();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
        if (logAct) {
            logger.info("stopped vdc " + name);
        }
    }

    private synchronized void doRound() {
        if (time != null) {
            if (time.matches(bits.getTime() + cfgAll.timeServerOffset)) {
                return;
            }
        }
        if (logAct) {
            logger.info("restarting vdc " + name);
        }
        if (randInt > 0) {
            bits.sleep(bits.random(1, randInt));
        }
        restartT = bits.getTime();
        restartC++;
        pipeLine pl = new pipeLine(65536, false);
        pipe = pl.getSide();
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
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
            String a = cfgBase + cfgInit.hwCfgEnd;
            String s = cfgBase + cfgInit.swCfgEnd;
            if (configFile != null) {
                s = configFile;
            }
            cmd = version.getJvmExec() + " " + cfgInit.jvmParam + " -Xmx" + imageMem + "m -jar " + version.getFileName() + " routercs " + a + " " + s;
        } else {
            cmd = "qemu-system-x86_64 -monitor none -serial stdio -nographic -no-reboot -enable-kvm -drive file=" + image1name + ",format=raw,cache=unsafe -m " + imageMem;
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
            if (cpuType != null) {
                cmd += " -cpu " + cpuType;
            }
            int vl = 1;
            for (int i = 0; i < locals.size(); i++) {
                cfgVdcIfc ntry = locals.get(i);
                cmd += " -netdev socket,id=n" + vl + ",udp=:" + ntry.port + ",localaddr=:" + (ntry.port + 1) + " -device " + nicType + ",netdev=n" + vl + ",mac=" + mac.toEmuStr();
                vl++;
                mac.setAdd(mac, one);
            }
            for (int i = 0; i < conns.size(); i++) {
                cfgVdcConn ntry = conns.get(i);
                cmd += " -netdev socket,id=n" + vl + ",udp=:" + ntry.conn.port + ",localaddr=:" + ntry.port + " -device " + nicType + ",netdev=n" + vl + ",mac=" + mac.toEmuStr();
                vl++;
                mac.setAdd(mac, one);
            }
            for (int i = 0; i < pcis.size(); i++) {
                cfgVdcPci dev = pcis.get(i);
                cmd += " -device vfio-pci,host=" + bits.toHexB(dev.bus) + ":" + bits.toHexB(dev.dev) + "." + dev.fnc;
            }
            if (usbs.size() > 0) {
                cmd += " -usb";
            }
            for (int i = 0; i < usbs.size(); i++) {
                cfgVdcUsb dev = usbs.get(i);
                cmd += " -device usb-host,hostbus=" + dev.bus + ",hostport=" + dev.prt;
            }
        }
        if (cpuPinning != null) {
            cmd = "taskset " + cpuPinning + " " + cmd;
        }
        if (userValue != null) {
            cmd = "sudo -u " + userValue + " " + cmd;
        }
        proc = pipeShell.exec(pl.getSide(), cmd, null, true, true, false, children);
        if (proc == null) {
            return;
        }
        for (;;) {
            if (!proc.isRunning()) {
                break;
            }
            if (con == null) {
                pipeDiscard.logLines("vdc " + name + " said ", pipe, logCon, logCol);
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
    public void stopNow() {
        need2run = false;
        restartNow();
    }

    /**
     * restart this vdc
     */
    public void restartNow() {
        try {
            proc.kill();
        } catch (Exception e) {
        }
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
    }

    /**
     * set respawn status
     *
     * @param res status
     */
    public void setRespawn(boolean res) {
        respawn = res;
    }

    private void addParam(List<String> l, String typ, String val) {
        if (val == null) {
            return;
        }
        l.add(typ + " " + val);
    }

    /**
     * start this vdc now
     *
     * @param defs defaults
     * @param inhs inheritables
     * @param beg vdc begin port
     * @param end vdc end port
     */
    public void startNow(List<String> defs, List<String> inhs, int beg, int end) {
        need2run = true;
        cfgBase = version.getRWpath() + "vdc-" + name + "-";
        List<String> l = new ArrayList<String>();
        l.add("hwid " + cfgInit.hwIdNum + "-" + name);
        l.add("hwsn " + uuidValue);
        l.add("rwpath " + version.getRWpath());
        l.add("port " + beg + " " + end);
        l.add("prio " + redunPrio);
        addParam(l, "jvm", cfgInit.jvmParam);
        addParam(l, "url", cfgAll.upgradeServer);
        addParam(l, "key", cfgAll.upgradePubKey);
        for (int i = 0; i < defs.size(); i++) {
            l.add("def " + defs.get(i));
        }
        for (int i = 0; i < inhs.size(); i++) {
            l.add(inhs.get(i));
        }
        for (int i = 0; i < tcps.size(); i++) {
            l.add("tcp2vrf " + tcps.get(i));
        }
        for (int i = 0; i < ifaces.size(); i++) {
            cfgVdcIfc ntry = ifaces.get(i);
            l.add("int " + ntry.line);
            cfgIfc ifc = cfgAll.ifcFind(ntry.name, 0);
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
        for (int i = 0; i < locals.size(); i++) {
            cfgVdcIfc ntry = locals.get(i);
            String a = "";
            if (ntry.redundancy) {
                a = " red";
            }
            l.add("int " + ntry.name + a + " " + ntry.name.substring(0, 3) + " " + mac + " 127.0.0.1 " + (ntry.port + 1) + " 127.0.0.1 " + ntry.port);
            mac.setAdd(mac, one);
        }
        for (int i = 0; i < conns.size(); i++) {
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
        String a = cfgBase + cfgInit.swCfgEnd;
        if (configFile != null) {
            a = configFile;
        }
        if (!new File(a).exists()) {
            l = new ArrayList<String>();
            l.add("hostname " + cfgAll.hostName + "-" + name);
            bits.buf2txt(true, l, a);
        }
        new Thread(this).start();
    }

    /**
     * get info
     *
     * @return info
     */
    public String getShow() {
        return name + "|" + restartC + "|" + pipeShell.info(proc) + "|" + bits.timePast(restartT) + "|" + bits.time2str(cfgAll.timeZoneName, restartT + cfgAll.timeServerOffset, 3);
    }

}

class cfgVdcTcp implements Comparator<cfgVdcTcp> {

    public int portH;

    public String vrf;

    public int portV;

    public addrIP adr;

    public int compare(cfgVdcTcp o1, cfgVdcTcp o2) {
        if (o1.portH < o2.portH) {
            return -1;
        }
        if (o1.portH > o2.portH) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        String a = portH + " " + vrf + " " + portV;
        if (adr == null) {
            return a;
        }
        return a + " " + adr;
    }

}

class cfgVdcPci implements Comparator<cfgVdcPci> {

    public int bus;

    public int dev;

    public int fnc;

    public int compare(cfgVdcPci o1, cfgVdcPci o2) {
        if (o1.bus < o2.bus) {
            return -1;
        }
        if (o1.bus > o2.bus) {
            return +1;
        }
        if (o1.dev < o2.dev) {
            return -1;
        }
        if (o1.dev > o2.dev) {
            return +1;
        }
        if (o1.fnc < o2.fnc) {
            return -1;
        }
        if (o1.fnc > o2.fnc) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return bus + " " + dev + " " + fnc;
    }

}

class cfgVdcUsb implements Comparator<cfgVdcUsb> {

    public int bus;

    public int prt;

    public int compare(cfgVdcUsb o1, cfgVdcUsb o2) {
        if (o1.bus < o2.bus) {
            return -1;
        }
        if (o1.bus > o2.bus) {
            return +1;
        }
        if (o1.prt < o2.prt) {
            return -1;
        }
        if (o1.prt > o2.prt) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return bus + " " + prt;
    }

}

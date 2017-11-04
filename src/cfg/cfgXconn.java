package cfg;

import addr.addrIP;
import clnt.clntDlsw;
import clnt.clntErspan;
import clnt.clntEtherIp;
import clnt.clntUti;
import clnt.clntNvGre;
import clnt.clntL2tp2;
import clnt.clntL2tp3;
import clnt.clntMplsPwe;
import clnt.clntPckOudp;
import clnt.clntPptp;
import clnt.clntVxlan;
import clnt.clntGeneve;
import ifc.ifcConnect;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packLdpPwe;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * xconnect configuration
 *
 * @author matecsaba
 */
public class cfgXconn implements Comparator<cfgXconn>, cfgGeneric {

    /**
     * name of connect
     */
    public String name;

    /**
     * pw encapsulation
     */
    public int pwtype = packLdpPwe.pwtEthPort;

    /**
     * pw mtu
     */
    public int pwmtu = 1500;

    /**
     * side 1
     */
    public cfgXconnSide side1 = new cfgXconnSide();

    /**
     * side 2
     */
    public cfgXconnSide side2 = new cfgXconnSide();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "xconnect .*! mtu 1500",
        "xconnect .*! type ethernet",
        "xconnect .*! no side1",
        "xconnect .*! no side2"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgXconn o1, cfgXconn o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "xconnect " + name;
    }

    private void getSideHelp(userHelping l) {
        l.add("2 3     <name>                      vrf to use");
        l.add("3 4       <name>                    source interface to use");
        l.add("4 5         pckoudp                 pckoudp encapsulation");
        l.add("4 5         pptp                    pptp encapsulation");
        l.add("4 5         l2tp2                   l2tp v2 encapsulation");
        l.add("4 5         l2tp3                   l2tp v3 encapsulation");
        l.add("4 5         pweompls                pwe over mpls encapsulation");
        l.add("4 5         erspan                  erspan encapsulation");
        l.add("4 5         dlsw                    dlsw encapsulation");
        l.add("4 5         etherip                 etherip encapsulation");
        l.add("4 5         uti                     uti encapsulation");
        l.add("4 5         nvgre                   nvgre encapsulation");
        l.add("4 5         vxlan                   vxlan encapsulation");
        l.add("4 5         geneve                  geneve encapsulation");
        l.add("5 6           <addr>                address of target");
        l.add("6 .             <num>               vc id");
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2  mtu                            specify vc mtu");
        l.add("2 .    <num>                        mtu");
        l.add("1 2  type                           type of pseudowire");
        l.add("2 .    ethernet                     ethernet mode");
        l.add("2 .    ip                           ip mode");
        l.add("2 .    vlan                         vlan mode");
        l.add("2 .    hdlc                         hdlc mode");
        l.add("2 .    ppp                          ppp mode");
        l.add("2 .    fr-dlci                      fr dlci mode");
        l.add("2 .    atm-aal5                     atm aal5 mode");
        l.add("1 2  side1                          specify first side of connection");
        getSideHelp(l);
        l.add("1 2  side2                          specify seconds side of connection");
        getSideHelp(l);
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("xconnect " + name);
        cmds.cfgLine(l, pwmtu == 0, cmds.tabulator, "mtu", "" + pwmtu);
        cmds.cfgLine(l, pwtype < 1, cmds.tabulator, "type", packLdpPwe.type2string(pwtype));
        cmds.cfgLine(l, !side1.ready2run(), cmds.tabulator, "side1", side1.getCfg());
        cmds.cfgLine(l, !side2.ready2run(), cmds.tabulator, "side2", side2.getCfg());
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        stop2run();
        String s = cmd.word();
        if (s.equals("type")) {
            pwtype = packLdpPwe.string2type(cmd.word());
            start2run();
            return;
        }
        if (s.equals("mtu")) {
            pwmtu = bits.str2num(cmd.word());
            start2run();
            return;
        }
        if (s.equals("side1")) {
            side1.doCfg(cmd);
            start2run();
            return;
        }
        if (s.equals("side2")) {
            side2.doCfg(cmd);
            start2run();
            return;
        }
        if (!s.equals("no")) {
            cmd.badCmd();
            return;
        }
        s = cmd.word();
        if (s.equals("side1")) {
            side1 = new cfgXconnSide();
            return;
        }
        if (s.equals("side2")) {
            side2 = new cfgXconnSide();
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "xconn";
    }

    /**
     * stop running
     */
    public void stop2run() {
        side1.stop2run();
        side2.stop2run();
    }

    /**
     * start running
     */
    public void start2run() {
        side1.stop2run();
        side2.stop2run();
        if (!side1.ready2run()) {
            return;
        }
        if (!side2.ready2run()) {
            return;
        }
        ifcConnect con = new ifcConnect();
        side1.upper = con.getSide1();
        side2.upper = con.getSide2();
        side1.lower = this;
        side2.lower = this;
        side1.start2run();
        side2.start2run();
    }

}

class cfgXconnSide {

    private cfgVrf vrf;

    private cfgIfc ifc;

    private cfgVpdn.protocolType mod;

    private addrIP adr;

    private int vcid;

    private clntPckOudp pou;

    private clntVxlan vxl;

    private clntGeneve gnv;

    private clntPptp pptp;

    private clntL2tp2 l2tp2;

    private clntL2tp3 l2tp3;

    private clntMplsPwe pwom;

    private clntErspan erspan;

    private clntDlsw dlsw;

    private clntEtherIp etherip;

    private clntUti uti;

    private clntNvGre nvgre;

    public cfgXconn lower;

    public ifcUp upper = new ifcNull();

    public boolean ready2run() {
        return mod != null;
    }

    public void stop2run() {
        if (pou != null) {
            pou.workStop();
            pou = null;
        }
        if (vxl != null) {
            vxl.workStop();
            vxl = null;
        }
        if (gnv != null) {
            gnv.workStop();
            gnv = null;
        }
        if (pptp != null) {
            pptp.workStop();
            pptp = null;
        }
        if (l2tp2 != null) {
            l2tp2.workStop();
            l2tp2 = null;
        }
        if (l2tp3 != null) {
            l2tp3.workStop();
            l2tp3 = null;
        }
        if (pwom != null) {
            pwom.workStop();
            pwom = null;
        }
        if (erspan != null) {
            erspan.workStop();
            erspan = null;
        }
        if (dlsw != null) {
            dlsw.workStop();
            dlsw = null;
        }
        if (etherip != null) {
            etherip.workStop();
            etherip = null;
        }
        if (uti != null) {
            uti.workStop();
            uti = null;
        }
        if (nvgre != null) {
            nvgre.workStop();
            nvgre = null;
        }
    }

    public void start2run() {
        switch (mod) {
            case prPou:
                pou = new clntPckOudp();
                pou.target = "" + adr;
                pou.vrf = vrf;
                pou.srcIfc = ifc;
                pou.prtR = vcid;
                pou.prtL = vcid;
                pou.setUpper(upper);
                pou.workStart();
                break;
            case prVxlan:
                vxl = new clntVxlan();
                vxl.target = "" + adr;
                vxl.vrf = vrf;
                vxl.srcIfc = ifc;
                vxl.inst = vcid;
                vxl.setUpper(upper);
                vxl.workStart();
                break;
            case prGeneve:
                gnv = new clntGeneve();
                gnv.target = "" + adr;
                gnv.vrf = vrf;
                gnv.srcIfc = ifc;
                gnv.vni = vcid;
                gnv.setUpper(upper);
                gnv.workStart();
                break;
            case prPptp:
                pptp = new clntPptp();
                pptp.target = "" + adr;
                pptp.vrf = vrf;
                pptp.srcIfc = ifc;
                pptp.direction = true;
                pptp.called = "" + vcid;
                pptp.setUpper(upper);
                pptp.workStart();
                break;
            case prL2tp2:
                l2tp2 = new clntL2tp2();
                l2tp2.target = "" + adr;
                l2tp2.vrf = vrf;
                l2tp2.srcIfc = ifc;
                l2tp2.direction = true;
                l2tp2.called = "" + vcid;
                l2tp2.calling = "" + vcid;
                l2tp2.setUpper(upper);
                l2tp2.workStart();
                break;
            case prL2tp3:
                l2tp3 = new clntL2tp3();
                l2tp3.pwType = lower.pwtype;
                l2tp3.target = "" + adr;
                l2tp3.vrf = vrf;
                l2tp3.srcIfc = ifc;
                l2tp3.vcid = "" + vcid;
                l2tp3.direction = true;
                l2tp3.setUpper(upper);
                l2tp3.workStart();
                break;
            case prPwom:
                pwom = new clntMplsPwe();
                pwom.pwType = lower.pwtype;
                pwom.pwMtu = lower.pwmtu;
                pwom.target = "" + adr;
                pwom.vrf = vrf;
                pwom.srcIfc = ifc;
                pwom.vcid = vcid;
                pwom.ctrlWrd = false;
                pwom.descr = lower.name;
                pwom.setUpper(upper);
                pwom.workStart();
                break;
            case prErspan:
                erspan = new clntErspan();
                erspan.target = "" + adr;
                erspan.vrf = vrf;
                erspan.srcIfc = ifc;
                erspan.spnid = vcid;
                erspan.vlnid = vcid;
                erspan.setUpper(upper);
                erspan.workStart();
                break;
            case prDlsw:
                dlsw = new clntDlsw();
                dlsw.target = "" + adr;
                dlsw.vrf = vrf;
                dlsw.srcIfc = ifc;
                dlsw.setUpper(upper);
                dlsw.workStart();
                break;
            case prEtherip:
                etherip = new clntEtherIp();
                etherip.target = "" + adr;
                etherip.vrf = vrf;
                etherip.srcIfc = ifc;
                etherip.setUpper(upper);
                etherip.workStart();
                break;
            case prUti:
                uti = new clntUti();
                uti.target = "" + adr;
                uti.vrf = vrf;
                uti.srcIfc = ifc;
                uti.tunKey = vcid;
                uti.setUpper(upper);
                uti.workStart();
                break;
            case prNvgre:
                nvgre = new clntNvGre();
                nvgre.target = "" + adr;
                nvgre.vrf = vrf;
                nvgre.srcIfc = ifc;
                nvgre.vsid = vcid;
                nvgre.setUpper(upper);
                nvgre.workStart();
                break;
            default:
                break;
        }
    }

    public String getCfg() {
        if (!ready2run()) {
            return "";
        }
        return vrf.name + " " + ifc.name + " " + cfgVpdn.type2str(mod) + " " + adr + " " + vcid;
    }

    public void doCfg(cmds cmd) {
        vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            cmd.error("no such vrf");
            return;
        }
        ifc = cfgAll.ifcFind(cmd.word(), false);
        if (ifc == null) {
            cmd.error("no such interface");
            return;
        }
        String md = cmd.word();
        adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        vcid = bits.str2num(cmd.word());
        if (vcid < 1) {
            cmd.error("bad vcid");
            return;
        }
        mod = cfgVpdn.str2type(md);
        if (mod == null) {
            cmd.error("bad mode");
            return;
        }
    }

}

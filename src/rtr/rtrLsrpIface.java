package rtr;

import addr.addrIP;
import addr.addrIPv4;
import auth.authLocal;
import cfg.cfgAll;
import cfg.cfgCert;
import cfg.cfgKey;
import cry.cryKeyDSA;
import cry.cryKeyECDSA;
import cry.cryKeyRSA;
import ip.ipFwdIface;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import prt.prtGenConn;
import prt.prtServP;
import serv.servGeneric;
import tab.tabGen;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * lsrp interface
 *
 * @author matecsaba
 */
public class rtrLsrpIface implements Comparator<rtrLsrpIface>, prtServP {

    /**
     * hello interval
     */
    public int helloTimer = 5000;

    /**
     * dead interval
     */
    public int deadTimer = 15000;

    /**
     * default metric
     */
    public int metric = 10;

    /**
     * affinity
     */
    public int affinity;

    /**
     * srlg
     */
    public int srlg;

    /**
     * bfd enabled
     */
    public boolean bfdTrigger = false;

    /**
     * passive interface
     */
    public boolean passiveInt = false;

    /**
     * suppress interface address
     */
    public boolean suppressAddr = false;

    /**
     * authentication string
     */
    public String authentication = null;

    /**
     * split horizon
     */
    public boolean splitHorizon = true;

    /**
     * the interface this works on
     */
    protected final ipFwdIface iface;

    /**
     * the udp connection it uses to multicast
     */
    protected prtGenConn conn;

    /**
     * the lower layer
     */
    protected rtrLsrp lower;

    /**
     * list of neighbors
     */
    protected tabGen<rtrLsrpNeigh> neighs;

    /**
     * rsa key to use
     */
    protected cfgKey<cryKeyRSA> keyRsa;

    /**
     * dsa key to use
     */
    protected cfgKey<cryKeyDSA> keyDsa;

    /**
     * ecdsa key to use
     */
    protected cfgKey<cryKeyECDSA> keyEcDsa;

    /**
     * rsa certificate to use
     */
    protected cfgCert certRsa;

    /**
     * dsa certificate to use
     */
    protected cfgCert certDsa;

    /**
     * ecdsa certificate to use
     */
    protected cfgCert certEcDsa;

    /**
     * security method
     */
    protected int encryptionMethod = 0;

    /**
     * create one instance
     *
     * @param parent the rip protocol
     * @param ifc the ip interface to work on
     */
    public rtrLsrpIface(rtrLsrp parent, ipFwdIface ifc) {
        lower = parent;
        iface = ifc;
        neighs = new tabGen<rtrLsrpNeigh>();
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        lower.udpCore.listenStop(iface, rtrLsrp.port, null, 0, 0);
        conn.setClosing();
    }

    /**
     * register to udp
     */
    public void register2udp() {
        addrIP adr = new addrIP();
        if (iface.addr.isIPv4()) {
            adr.fromString("224.0.0.228");
        } else {
            adr.fromString("ff02::228");
        }
        lower.udpCore.packetListen(this, iface, rtrLsrp.port, null, 0, 0, "lsrp", null, -1);
        conn = lower.udpCore.packetConnect(this, iface, rtrLsrp.port, adr, rtrLsrp.port, "lsrp", null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 0;
    }

    /**
     * list of neighbors
     *
     * @param res list to update
     */
    protected void showNeighs(userFormat res) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrLsrpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            res.add(iface + "|" + nei.rtrId + "|" + nei.name + "|" + nei.peer + "|" + nei.isReady() + "|" + bits.timePast(nei.upTime));
        }
    }

    /**
     * get configuration
     *
     * @param l list to add
     * @param beg beginning
     */
    public void routerGetConfig(List<String> l, String beg) {
        l.add(cmds.tabulator + beg + "enable");
        cmds.cfgLine(l, !splitHorizon, cmds.tabulator, beg + "split-horizon", "");
        cmds.cfgLine(l, !passiveInt, cmds.tabulator, beg + "passive", "");
        cmds.cfgLine(l, !bfdTrigger, cmds.tabulator, beg + "bfd", "");
        cmds.cfgLine(l, !suppressAddr, cmds.tabulator, beg + "suppress-prefix", "");
        cmds.cfgLine(l, encryptionMethod <= 0, cmds.tabulator, beg + "encryption", servGeneric.proto2string(encryptionMethod) + " " + keyRsa + " " + keyDsa + " " + keyEcDsa + " " + certRsa + " " + certDsa + " " + certEcDsa);
        cmds.cfgLine(l, authentication == null, cmds.tabulator, beg + "password", authLocal.passwdEncode(authentication));
        l.add(cmds.tabulator + beg + "metric " + metric);
        l.add(cmds.tabulator + beg + "affinity " + affinity);
        l.add(cmds.tabulator + beg + "srlg " + srlg);
        l.add(cmds.tabulator + beg + "hello-time " + helloTimer);
        l.add(cmds.tabulator + beg + "dead-time " + deadTimer);
    }

    /**
     * get help text
     *
     * @param l list to update
     */
    public static void routerGetHelp(userHelping l) {
        l.add("4 .         enable                  enable protocol processing");
        l.add("4 .         split-horizon           dont advertise back on rx interface");
        l.add("4 .         bfd                     enable bfd triggered down");
        l.add("4 .         passive                 do not form neighborship");
        l.add("4 .         suppress-prefix         do not advertise interface");
        l.add("4 5         encryption              select encryption method");
        l.add("5 6           ssh                   select secure shell");
        l.add("5 6           tls                   select transport layer security");
        l.add("6 7             <name>              rsa key");
        l.add("7 8               <name>            dsa key");
        l.add("8 9                 <name>          ecdsa key");
        l.add("9 10                  <name>        rsa certificate");
        l.add("10 11                   <name>      dsa certificate");
        l.add("11 .                      <name>    ecdsa certificate");
        l.add("4 5         password                password for authentication");
        l.add("5 .           <text>                set password");
        l.add("4 5         metric                  interface metric");
        l.add("5 .           <num>                 metric");
        l.add("4 5         affinity                set affinity");
        l.add("5 .           <num>                 affinity");
        l.add("4 5         srlg                    set srlg");
        l.add("5 .           <num>                 srlg");
        l.add("4 5         hello-time              time between hellos");
        l.add("5 .           <num>                 time in ms");
        l.add("4 5         dead-time               time before neighbor down");
        l.add("5 .           <num>                 time in ms");
    }

    /**
     * do one config
     *
     * @param a command
     * @param cmd parameters
     */
    public void routerDoConfig(String a, cmds cmd) {
        if (a.equals("bfd")) {
            bfdTrigger = true;
            return;
        }
        if (a.equals("split-horizon")) {
            splitHorizon = true;
            return;
        }
        if (a.equals("password")) {
            authentication = authLocal.passwdDecode(cmd.word());
            return;
        }
        if (a.equals("encryption")) {
            encryptionMethod = servGeneric.string2proto(cmd.word());
            keyRsa = cfgAll.keyFind(cfgAll.rsakeys, cmd.word(), false);
            keyDsa = cfgAll.keyFind(cfgAll.dsakeys, cmd.word(), false);
            keyEcDsa = cfgAll.keyFind(cfgAll.ecdsakeys, cmd.word(), false);
            certRsa = cfgAll.certFind(cmd.word(), false);
            certDsa = cfgAll.certFind(cmd.word(), false);
            certEcDsa = cfgAll.certFind(cmd.word(), false);
            return;
        }
        if (a.equals("suppress-prefix")) {
            suppressAddr = true;
            lower.todo.set(0);
            lower.notif.wakeup();
            return;
        }
        if (a.equals("passive")) {
            passiveInt = true;
            return;
        }
        if (a.equals("hello-time")) {
            helloTimer = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("dead-time")) {
            deadTimer = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("metric")) {
            metric = bits.str2num(cmd.word());
            lower.todo.set(0);
            lower.notif.wakeup();
            return;
        }
        if (a.equals("affinity")) {
            affinity = bits.str2num(cmd.word());
            lower.todo.set(0);
            lower.notif.wakeup();
            return;
        }
        if (a.equals("srlg")) {
            srlg = bits.str2num(cmd.word());
            lower.todo.set(0);
            lower.notif.wakeup();
            return;
        }
        cmd.badCmd();
    }

    /**
     * undo one config
     *
     * @param a command
     * @param cmd parameters
     */
    public void routerUnConfig(String a, cmds cmd) {
        if (a.equals("bfd")) {
            bfdTrigger = false;
            return;
        }
        if (a.equals("split-horizon")) {
            splitHorizon = false;
            return;
        }
        if (a.equals("password")) {
            authentication = null;
            return;
        }
        if (a.equals("suppress-prefix")) {
            suppressAddr = false;
            lower.todo.set(0);
            lower.notif.wakeup();
            return;
        }
        if (a.equals("passive")) {
            passiveInt = false;
            return;
        }
        if (a.equals("encryption")) {
            encryptionMethod = 0;
            return;
        }
        cmd.badCmd();
    }

    public String toString() {
        return "lsrp on " + iface;
    }

    public int compare(rtrLsrpIface o1, rtrLsrpIface o2) {
        if (o1.iface.ifwNum < o2.iface.ifwNum) {
            return -1;
        }
        if (o1.iface.ifwNum > o2.iface.ifwNum) {
            return +1;
        }
        return 0;
    }

    private void sendHello(prtGenConn id) {
        packHolder pck = new packHolder(true, true);
        pck.putFill(0, 16, 255);
        pck.putSkip(16);
        pck.putAddr(0, lower.routerID);
        pck.putSkip(4);
        if (!passiveInt) {
            for (int i = 0; i < neighs.size(); i++) {
                rtrLsrpNeigh nei = neighs.get(i);
                pck.putAddr(0, nei.rtrId);
                pck.putSkip(4);
            }
        }
        pck.merge2beg();
        id.send2net(pck);
    }

    /**
     * close interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * start connection
     *
     * @param id connection
     * @return false if success, true if error
     */
    public boolean datagramAccept(prtGenConn id) {
        id.timeout = deadTimer;
        id.workInterval = helloTimer;
        return false;
    }

    /**
     * stop connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
        if (conn == null) {
            return;
        }
        if (id.compare(id, conn) != 0) {
            return;
        }
        if (debugger.rtrLsrpEvnt) {
            logger.debug("tx hello " + id);
        }
        sendHello(conn);
        long tim = bits.getTime();
        for (int i = neighs.size() - 1; i >= 0; i--) {
            rtrLsrpNeigh nei = neighs.get(i);
            if ((tim - nei.lastHeard) < deadTimer) {
                continue;
            }
            nei.stopWork();
        }
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false if success, true if error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        id.setClosing();
        if (passiveInt) {
            return true;
        }
        for (int i = 0; i < 16; i++) {
            if (pck.getByte(i) != 255) {
                return true;
            }
        }
        addrIPv4 peer = new addrIPv4();
        pck.getAddr(peer, 16);
        pck.getSkip(20);
        int seen = 0;
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            addrIPv4 adr = new addrIPv4();
            pck.getAddr(adr, 0);
            pck.getSkip(4);
            if (adr.compare(adr, lower.routerID) == 0) {
                seen++;
            }
        }
        if (debugger.rtrLsrpEvnt) {
            logger.debug("rx hello " + id);
        }
        rtrLsrpNeigh nei = new rtrLsrpNeigh(lower, this, peer, id.peerAddr);
        rtrLsrpNeigh old = neighs.add(nei);
        if (old == null) {
            nei.startWork();
            sendHello(conn);
        } else {
            nei = old;
        }
        if (seen > 0) {
            nei.lastHeard = bits.getTime();
        }
        return false;
    }

    /**
     * close this interface
     */
    public void routerCloseNow() {
        unregister2udp();
        for (int i = 0; i < neighs.size(); i++) {
            rtrLsrpNeigh nei = neighs.get(i);
            nei.stopWork();
        }
    }

    /**
     * got better advertisement
     *
     * @param dat advertisement
     */
    public void gotAdvert(rtrLsrpData dat) {
        if (!splitHorizon) {
            return;
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrLsrpNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.advert.put(dat.copyHead());
        }
    }

}

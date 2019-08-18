package ifc;

import addr.addrEmpty;
import addr.addrEui;
import addr.addrIPv4;
import addr.addrType;
import auth.authGeneric;
import auth.authLocal;
import auth.authenDown;
import auth.authenHead;
import auth.autherChap;
import auth.autherDoer;
import auth.autherEap;
import auth.autherPap;
import cfg.cfgAll;
import cfg.cfgAuther;
import cfg.cfgIfc;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * point to point protocol (rfc1661) encapsulation handler
 *
 * @author matecsaba
 */
public class ifcPpp implements ifcUp, ifcDn, authenDown {

    /**
     * sent username
     */
    public String sentUser = null;

    /**
     * sent password
     */
    public String sentPass = null;

    /**
     * sent async char map
     */
    public int sentAccm;

    /**
     * sent max recv unit
     */
    public int sentMru;

    /**
     * authentication list to use for remote
     */
    public authGeneric authenRem = null;

    /**
     * my configured address
     */
    public addrEui locIfIdCfg;

    /**
     * my configured address
     */
    public addrIPv4 locAddrCfg;

    /**
     * peer configured address
     */
    public addrIPv4 remAddrCfg;

    /**
     * dns1 address, null=negotiated
     */
    public addrIPv4 dns1addrCfg;

    /**
     * dns2 address, null=negotiated
     */
    public addrIPv4 dns2addrCfg;

    /**
     * local address required
     */
    public boolean locAddrReq;

    /**
     * refuse pap
     */
    public boolean refusePap;

    /**
     * refuse chap
     */
    public boolean refuseChap;

    /**
     * refuse eap
     */
    public boolean refuseEap;

    /**
     * size of header
     */
    public final static int size = 4;

    /**
     * last known state
     */
    public state.states lastState = state.states.down;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * current keepalive interval (0=disabled)
     */
    public int keepaliveInterval = 5;

    /**
     * nak retry limit
     */
    public int nakRetryLimit = 16;

    /**
     * server that handler received packets
     */
    public ifcUp upper = new ifcNull();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    /**
     * config class
     */
    public cfgIfc cfger;

    private Timer keepTimer;

    private int curMode = modeLcp;

    private int reqSent;

    private final static int modeLcp = 1;

    private final static int modeAuth = 2;

    private final static int modeUp = 3;

    /**
     * lcp
     */
    public ifcPppLcp ctrlLcp;

    /**
     * authentication
     */
    public autherDoer ctrlAuth;

    /**
     * ip4vp
     */
    public ifcPppIp4 ctrlIp4;

    /**
     * ip6cp
     */
    public ifcPppIp6 ctrlIp6;

    /**
     * bcp
     */
    public ifcPppBrdg ctrlBrdg;

    /**
     * mplscp
     */
    public ifcPppMpls ctrlMpls;

    /**
     * osicp
     */
    public ifcPppOsi ctrlOsi;

    /**
     * ipxcp
     */
    public ifcPppIpx ctrlIpx;

    /**
     * ecp
     */
    public ifcPppCrypt ctrlCrypt;

    /**
     * scp
     */
    public ifcPppNsh ctrlNsh;

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
    }

    /**
     * close interface
     */
    public void closeUp() {
        setState(state.states.close);
        upper.closeUp();
    }

    /**
     * close interface
     */
    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
    }

    /**
     * flap interface
     */
    public void flapped() {
        clearState();
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
        setState(lastState);
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return lastState;
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        if (checkPeerState(stat)) {
            return;
        }
        restartTimer(false);
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return lower.getMTUsize() - size;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return lower.getBandwidth();
    }

    /**
     * create new instance
     */
    public ifcPpp() {
        ctrlLcp = new ifcPppLcp(this);
        ctrlIp4 = new ifcPppIp4(this);
        ctrlIp6 = new ifcPppIp6(this);
        ctrlBrdg = new ifcPppBrdg(this);
        ctrlMpls = new ifcPppMpls(this);
        ctrlOsi = new ifcPppOsi(this);
        ctrlIpx = new ifcPppIpx(this);
        ctrlCrypt = new ifcPppCrypt(this);
        ctrlNsh = new ifcPppNsh(this);
        clearState();
        restartTimer(false);
    }

    public String toString() {
        return "ppp on " + lower;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelping l) {
        l.add("2 3     keepalive                   keepalive timer");
        l.add("3 .       <num>                     time in seconds");
        l.add("2 3     username                    name of user to send");
        l.add("3 .       <text>                    username");
        l.add("2 3     password                    password of user to send");
        l.add("3 .       <text>                    password");
        l.add("2 3     accm                        async character map");
        l.add("3 .       <num>                     accm");
        l.add("2 3     mru                         max receive unit");
        l.add("3 .       <num>                     mru");
        l.add("2 3     refuseauth                  never use authentication protocol");
        l.add("3 .       pap                       password authentication protocol");
        l.add("3 .       chap                      challenge handshake authentication protocol");
        l.add("3 .       eap                       extensible authentication protocol");
        l.add("2 3     authentication              set peer authentication list");
        l.add("3 .       <text>                    name of list");
        l.add("2 3     naktry                      nak retry limit");
        l.add("3 .       <num>                     number of tries");
        l.add("2 3     ip4cp                       ipv4 control protocol");
        l.add("3 .       open                      force to open state");
        l.add("3 .       close                     force to close state");
        l.add("3 .       reqaddr                   require local address");
        l.add("3 4       peer                      set peer address");
        l.add("4 .         <text>                  address");
        l.add("3 4       local                     set local address");
        l.add("4 .         <text>                  address");
        l.add("3 4       dns1                      set dns1 address");
        l.add("4 .         <text>                  address");
        l.add("3 4       dns2                      set dns1 address");
        l.add("4 .         <text>                  address");
        l.add("2 3     ip6cp                       ipv6 control protocol");
        l.add("3 .       open                      force to open state");
        l.add("3 .       close                     force to close state");
        l.add("3 4       local                     set local address");
        l.add("4 .         <text>                  address");
        l.add("2 3     bcp                         bridge control protocol");
        l.add("3 .       open                      force to open state");
        l.add("3 .       close                     force to close state");
        l.add("2 3     mplscp                      multiprotocol label switching control protocol");
        l.add("3 .       open                      force to open state");
        l.add("3 .       close                     force to close state");
        l.add("2 3     osicp                       open systems interconnect control protocol");
        l.add("3 .       open                      force to open state");
        l.add("3 .       close                     force to close state");
        l.add("2 3     ipxcp                       ipx control protocol");
        l.add("3 .       open                      force to open state");
        l.add("3 .       close                     force to close state");
        l.add("2 3     ecp                         encryption control protocol");
        l.add("3 .       open                      force to open state");
        l.add("3 .       close                     force to close state");
        l.add("2 3     scp                         service control protocol");
        l.add("3 .       open                      force to open state");
        l.add("3 .       close                     force to close state");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "keepalive " + keepaliveInterval);
        cmds.cfgLine(l, authenRem == null, cmds.tabulator, "ppp authentication", "" + authenRem);
        cmds.cfgLine(l, sentUser == null, cmds.tabulator, "ppp username", sentUser);
        cmds.cfgLine(l, sentPass == null, cmds.tabulator, "ppp password", authLocal.passwdEncode(sentPass));
        cmds.cfgLine(l, !refusePap, cmds.tabulator, "ppp refuseauth pap", "");
        cmds.cfgLine(l, !refuseChap, cmds.tabulator, "ppp refuseauth chap", "");
        cmds.cfgLine(l, !refuseEap, cmds.tabulator, "ppp refuseauth eap", "");
        l.add(beg + "naktry " + nakRetryLimit);
        l.add(beg + "accm " + sentAccm);
        l.add(beg + "mru " + sentMru);
        cmds.cfgLine(l, !ctrlIp4.forced2close(), cmds.tabulator, "ppp ip4cp close", "");
        cmds.cfgLine(l, !ctrlIp4.forced2open(), cmds.tabulator, "ppp ip4cp open", "");
        cmds.cfgLine(l, remAddrCfg == null, cmds.tabulator, "ppp ip4cp peer", "" + remAddrCfg);
        cmds.cfgLine(l, locAddrCfg == null, cmds.tabulator, "ppp ip4cp local", "" + locAddrCfg);
        cmds.cfgLine(l, dns1addrCfg == null, cmds.tabulator, "ppp ip4cp dns1", "" + dns1addrCfg);
        cmds.cfgLine(l, dns2addrCfg == null, cmds.tabulator, "ppp ip4cp dns2", "" + dns2addrCfg);
        cmds.cfgLine(l, !locAddrReq, cmds.tabulator, "ppp ip4cp reqaddr", "");
        cmds.cfgLine(l, !ctrlIp6.forced2close(), cmds.tabulator, "ppp ip6cp close", "");
        cmds.cfgLine(l, !ctrlIp6.forced2open(), cmds.tabulator, "ppp ip6cp open", "");
        cmds.cfgLine(l, locIfIdCfg == null, cmds.tabulator, "ppp ip6cp local", "" + locIfIdCfg);
        cmds.cfgLine(l, !ctrlBrdg.forced2close(), cmds.tabulator, "ppp bcp close", "");
        cmds.cfgLine(l, !ctrlBrdg.forced2open(), cmds.tabulator, "ppp bcp open", "");
        cmds.cfgLine(l, !ctrlMpls.forced2close(), cmds.tabulator, "ppp mplscp close", "");
        cmds.cfgLine(l, !ctrlMpls.forced2open(), cmds.tabulator, "ppp mplscp open", "");
        cmds.cfgLine(l, !ctrlOsi.forced2close(), cmds.tabulator, "ppp osicp close", "");
        cmds.cfgLine(l, !ctrlOsi.forced2open(), cmds.tabulator, "ppp osicp open", "");
        cmds.cfgLine(l, !ctrlIpx.forced2close(), cmds.tabulator, "ppp ipxcp close", "");
        cmds.cfgLine(l, !ctrlIpx.forced2open(), cmds.tabulator, "ppp ipxcp open", "");
        cmds.cfgLine(l, !ctrlCrypt.forced2close(), cmds.tabulator, "ppp ecp close", "");
        cmds.cfgLine(l, !ctrlCrypt.forced2open(), cmds.tabulator, "ppp ecp open", "");
        cmds.cfgLine(l, !ctrlNsh.forced2close(), cmds.tabulator, "ppp scp close", "");
        cmds.cfgLine(l, !ctrlNsh.forced2open(), cmds.tabulator, "ppp scp open", "");
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("refuseauth")) {
            a = cmd.word();
            if (a.equals("pap")) {
                refusePap = true;
                return;
            }
            if (a.equals("chap")) {
                refuseChap = true;
                return;
            }
            if (a.equals("eap")) {
                refuseEap = true;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("ip4cp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlIp4.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlIp4.forceClose(true);
                return;
            }
            if (a.equals("peer")) {
                addrIPv4 adr = new addrIPv4();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad ip address");
                    return;
                }
                remAddrCfg = adr;
                return;
            }
            if (a.equals("local")) {
                addrIPv4 adr = new addrIPv4();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad ip address");
                    return;
                }
                locAddrCfg = adr;
                return;
            }
            if (a.equals("reqaddr")) {
                locAddrReq = true;
                return;
            }
            if (a.equals("dns1")) {
                addrIPv4 adr = new addrIPv4();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad ip address");
                    return;
                }
                dns1addrCfg = adr;
                return;
            }
            if (a.equals("dns2")) {
                addrIPv4 adr = new addrIPv4();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad ip address");
                    return;
                }
                dns2addrCfg = adr;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("ip6cp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlIp6.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlIp6.forceClose(true);
                return;
            }
            if (a.equals("local")) {
                addrEui adr = new addrEui();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad ip address");
                    return;
                }
                locIfIdCfg = adr;
                return;
            }
        }
        if (a.equals("bcp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlBrdg.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlBrdg.forceClose(true);
                return;
            }
        }
        if (a.equals("mplscp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlMpls.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlMpls.forceClose(true);
                return;
            }
        }
        if (a.equals("osicp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlOsi.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlOsi.forceClose(true);
                return;
            }
        }
        if (a.equals("ipxcp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlIpx.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlIpx.forceClose(true);
                return;
            }
        }
        if (a.equals("ecp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlCrypt.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlCrypt.forceClose(true);
                return;
            }
        }
        if (a.equals("scp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlNsh.forceOpen(true);
                return;
            }
            if (a.equals("close")) {
                ctrlNsh.forceClose(true);
                return;
            }
        }
        if (a.equals("keepalive")) {
            keepaliveInterval = bits.str2num(cmd.word());
            restartTimer(false);
            return;
        }
        if (a.equals("username")) {
            sentUser = cmd.getRemaining();
            return;
        }
        if (a.equals("password")) {
            sentPass = authLocal.passwdDecode(cmd.getRemaining());
            return;
        }
        if (a.equals("accm")) {
            sentAccm = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("mru")) {
            sentMru = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("naktry")) {
            nakRetryLimit = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("authentication")) {
            cfgAuther auth = cfgAll.autherFind(cmd.word(), null);
            if (auth == null) {
                cmd.error("no such authentication list");
                return;
            }
            authenRem = auth.getAuther();
            return;
        }
        cmd.badCmd();
    }

    /**
     * undo configuration
     *
     * @param cmd command
     */
    public void unConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("refuseauth")) {
            a = cmd.word();
            if (a.equals("pap")) {
                refusePap = false;
                return;
            }
            if (a.equals("chap")) {
                refuseChap = false;
                return;
            }
            if (a.equals("eap")) {
                refuseEap = false;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("ip4cp")) {
            a = cmd.word();
            if (a.equals("peer")) {
                remAddrCfg = null;
                return;
            }
            if (a.equals("local")) {
                locAddrCfg = null;
                return;
            }
            if (a.equals("reqaddr")) {
                locAddrReq = false;
                return;
            }
            if (a.equals("dns1")) {
                dns1addrCfg = null;
                return;
            }
            if (a.equals("dns2")) {
                dns2addrCfg = null;
                return;
            }
            if (a.equals("open")) {
                ctrlIp4.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlIp4.forceClose(false);
                return;
            }
            cmd.badCmd();
        }
        if (a.equals("ip6cp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlIp6.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlIp6.forceClose(false);
                return;
            }
            if (a.equals("local")) {
                locIfIdCfg = null;
                return;
            }
        }
        if (a.equals("bcp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlBrdg.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlBrdg.forceClose(false);
                return;
            }
        }
        if (a.equals("mplscp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlMpls.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlMpls.forceClose(false);
                return;
            }
        }
        if (a.equals("osicp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlOsi.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlOsi.forceClose(false);
                return;
            }
        }
        if (a.equals("ipxcp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlIpx.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlIpx.forceClose(false);
                return;
            }
        }
        if (a.equals("ecp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlCrypt.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlCrypt.forceClose(false);
                return;
            }
        }
        if (a.equals("scp")) {
            a = cmd.word();
            if (a.equals("open")) {
                ctrlNsh.forceOpen(false);
                return;
            }
            if (a.equals("close")) {
                ctrlNsh.forceClose(false);
                return;
            }
        }
        if (a.equals("username")) {
            sentUser = null;
            return;
        }
        if (a.equals("password")) {
            sentPass = null;
            return;
        }
        if (a.equals("accm")) {
            sentAccm = 0;
            return;
        }
        if (a.equals("mru")) {
            sentMru = 0;
            return;
        }
        if (a.equals("authentication")) {
            authenRem = null;
            return;
        }
        cmd.badCmd();
    }

    /**
     * clear state info
     */
    public void clearState() {
        ctrlAuth = null;
        curMode = modeLcp;
        ctrlLcp.clearState();
        ctrlIp4.clearState();
        ctrlIp6.clearState();
        ctrlBrdg.clearState();
        ctrlMpls.clearState();
        ctrlOsi.clearState();
        ctrlIpx.clearState();
        ctrlCrypt.clearState();
        ctrlNsh.clearState();
    }

    /**
     * check peer state from timers
     *
     * @param force try to force line protocol
     * @return true if state changed
     */
    public boolean checkPeerState(state.states force) {
        state.states stat = state.states.down;
        if (ctrlLcp.getReady()) {
            stat = state.states.up;
        }
        if (keepaliveInterval < 1) {
            stat = state.states.up;
        }
        if (state.toForceable(force) == state.states.admin) {
            stat = state.states.admin;
        }
        if (lastState == stat) {
            return false;
        }
        if (stat != state.states.up) {
            clearState();
        }
        lastState = stat;
        if (debugger.ifcPppEvnt) {
            logger.debug("line proto=" + state.conv2string(stat));
        }
        if (stat != state.states.up) {
            lower.flapped();
        } else {
            sendKeepReq();
        }
        cntr.stateChange(stat);
        upper.setState(stat);
        return true;
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        if (lastState == state.states.admin) {
            return;
        }
        if (shutdown) {
            return;
        }
        if (keepaliveInterval < 1) {
            return;
        }
        keepTimer = new Timer();
        ifcPppTxKeep task = new ifcPppTxKeep(this);
        keepTimer.schedule(task, 500, keepaliveInterval * 1000);
    }

    /**
     * put address and control bytes
     *
     * @param pck packet to use
     * @param proto protocol number
     */
    public void putAddrCtrlProto(packHolder pck, int proto) {
        pck.putByte(0, 0xff); // address
        pck.putByte(1, 0x03); // control
        pck.msbPutW(2, proto); // protocol
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * send ncp control packet
     *
     * @param pck packet to update
     * @param proto protocol number
     * @param code code number
     * @param id id value
     */
    public void sendNcpCtrl(packHolder pck, int proto, int code, int id) {
        authenHead cis = new authenHead();
        cis.code = code;
        cis.id = id;
        cis.updatePack(pck);
        putAddrCtrlProto(pck, proto);
        lower.sendPack(pck);
        if (debugger.ifcPppEvnt) {
            logger.debug("tx " + ifcPppNcp.code2str(code) + " id=" + id);
        }
    }

    /**
     * received one control packet
     *
     * @param pck packet received
     * @param cp control protocol got it
     * @param proto protocol id
     * @return true went up
     */
    public boolean recvNcpCtrl(packHolder pck, ifcPppNcp cp, int proto) {
        authenHead cis = new authenHead();
        if (cis.parsePack(pck)) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return false;
        }
        if (debugger.ifcPppEvnt) {
            logger.debug("rx " + ifcPppNcp.code2str(cis.code) + " id=" + cis.id);
        }
        boolean b = cp.getReady();
        cp.recvPck(pck, cis.code, cis.id);
        if (b || (!cp.getReady())) {
            return false;
        }
        if (cfger == null) {
            return true;
        }
        switch (proto) {
            case ifcPppIp4.pppCtrl:
                cfger.addr4changed(ctrlIp4.locAddrCur, cfger.mask4, ctrlIp4.remAddrCur);
                break;
            case ifcPppIp6.pppCtrl:
                cfger.addr6changed(ctrlIp6.locAddrCur.toIPv6(), cfger.mask6, ctrlIp6.remAddrCur.toIPv6());
                break;
        }
        return true;
    }

    /**
     * received authentication packet
     *
     * @param pck received packet
     */
    public void recvAuthPack(packHolder pck) {
        if (ctrlAuth == null) {
            return;
        }
        authenHead cis = new authenHead();
        if (cis.parsePack(pck)) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (debugger.ifcPppEvnt) {
            logger.debug("rx " + cis);
        }
        ctrlAuth.recvPck(pck, cis.code, cis.id);
    }

    /**
     * send authentication packet
     *
     * @param pck packet
     * @param proto protocol
     * @param code code
     * @param id id
     * @param msg message
     */
    public void sendAuthPack(packHolder pck, int proto, int code, int id, String msg) {
        authenHead cis = new authenHead();
        cis.code = code;
        cis.id = id;
        cis.updatePack(pck);
        putAddrCtrlProto(pck, proto);
        lower.sendPack(pck);
        if (debugger.ifcPppEvnt) {
            logger.debug("tx " + msg);
        }
    }

    public void recvAuthPack(String msg) {
        if (debugger.ifcPppEvnt) {
            logger.debug("rx " + msg);
        }
    }

    /**
     * check for authentication up
     */
    public void checkAuthUp() {
        if (ctrlAuth == null) {
            return;
        }
        if (ctrlAuth.working) {
            return;
        }
        boolean b = ctrlAuth.succeed;
        if (debugger.ifcPppEvnt) {
            logger.debug("authentication passed=" + b);
        }
        if (!b) {
            clearState();
            return;
        }
        curMode = modeUp;
        sendKeepReq();
    }

    /**
     * send ncp configure request
     *
     * @param ncp protocol to use
     */
    public void sendConfReq(ifcPppNcp ncp) {
        if (!ncp.getNeeded()) {
            return;
        }
        if (ncp.getReady()) {
            return;
        }
        if (ncp.sawReq3 < 1) {
            reqSent++;
            ncp.sendReq();
        }
        ncp.sawReq3 = 0;
    }

    /**
     * send keepalive over lcp
     */
    public void sendKeepReq() {
        int oldMod = curMode;
        switch (curMode) {
            case modeLcp:
                if (!ctrlLcp.getReady()) {
                    ctrlLcp.sendReq();
                    break;
                }
                ctrlIp4.clearState();
                ctrlIp6.clearState();
                ctrlBrdg.clearState();
                ctrlMpls.clearState();
                ctrlOsi.clearState();
                ctrlIpx.clearState();
                ctrlCrypt.clearState();
                ctrlNsh.clearState();
                if (ctrlLcp.authLoc > 0) {
                    ctrlAuth = autherDoer.getWorker(this, ctrlLcp.authLoc);
                    if (ctrlAuth == null) {
                        curMode = modeUp;
                        break;
                    }
                    ctrlAuth.sentUser = sentUser;
                    ctrlAuth.sentPass = sentPass;
                    curMode = modeAuth;
                    break;
                }
                if (authenRem != null) {
                    ctrlAuth = autherDoer.getWorker(this, ctrlLcp.authRem);
                    ctrlAuth.authenRem = authenRem;
                    curMode = modeAuth;
                    break;
                }
                curMode = modeUp;
                break;
            case modeAuth:
                ctrlAuth.sendReq();
                checkAuthUp();
                break;
            case modeUp:
                reqSent = 0;
                sendConfReq(ctrlLcp);
                sendConfReq(ctrlIp4);
                sendConfReq(ctrlIp6);
                sendConfReq(ctrlBrdg);
                sendConfReq(ctrlMpls);
                sendConfReq(ctrlOsi);
                sendConfReq(ctrlIpx);
                sendConfReq(ctrlCrypt);
                sendConfReq(ctrlNsh);
                if (reqSent > 0) {
                    break;
                }
                ctrlLcp.sendEchoReq();
                break;
            default:
                clearState();
                break;
        }
        if (oldMod != curMode) {
            sendKeepReq();
        }
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        // int addr = pck.getByte(0); // address
        // int ctrl = pck.getByte(1); // control
        int prot = pck.msbGetW(2); // protocol
        pck.getSkip(size);
        int newProt = -1;
        switch (prot) {
            case ifcPppMpls.pppDataU:
                newProt = ifcPppMpls.ethTypU;
                break;
            case ifcPppIp4.pppData:
                newProt = ifcPppIp4.ethTyp;
                break;
            case ifcPppIp6.pppData:
                newProt = ifcPppIp6.ethTyp;
                break;
            case ifcPppBrdg.pppData:
                ifcPppBrdg.patchPackRx(pck);
                newProt = ifcPppBrdg.ethTyp;
                break;
            case ifcPppOsi.pppData:
                newProt = ifcPppOsi.ethTyp;
                break;
            case ifcPppIpx.pppData:
                newProt = ifcPppIpx.ethTyp;
                break;
            case ifcPppCrypt.pppData:
                newProt = ifcPppCrypt.ethTyp;
                break;
            case ifcPppNsh.pppData:
                newProt = ifcPppNsh.ethTyp;
                break;
            case ifcPppMpls.pppDataM:
                newProt = ifcPppMpls.ethTypM;
                break;
            case ifcPppIp4.pppCtrl:
                recvNcpCtrl(pck, ctrlIp4, prot);
                break;
            case ifcPppIp6.pppCtrl:
                recvNcpCtrl(pck, ctrlIp6, prot);
                break;
            case ifcPppBrdg.pppCtrl:
                recvNcpCtrl(pck, ctrlBrdg, prot);
                break;
            case ifcPppMpls.pppCtrl:
                recvNcpCtrl(pck, ctrlMpls, prot);
                break;
            case ifcPppOsi.pppCtrl:
                recvNcpCtrl(pck, ctrlOsi, prot);
                break;
            case ifcPppIpx.pppCtrl:
                recvNcpCtrl(pck, ctrlIpx, prot);
                break;
            case ifcPppCrypt.pppCtrl:
                recvNcpCtrl(pck, ctrlCrypt, prot);
                break;
            case ifcPppNsh.pppCtrl:
                recvNcpCtrl(pck, ctrlNsh, prot);
                break;
            case ifcPppLcp.pppCtrl:
                recvNcpCtrl(pck, ctrlLcp, prot);
                checkPeerState(state.states.up);
                break;
            case autherEap.pppCtrl:
            case autherChap.pppCtrl:
            case autherPap.pppCtrl:
                recvAuthPack(pck);
                checkAuthUp();
                break;
            default:
                cntr.drop(pck, counter.reasons.badCmd);
                pck.getSkip(-2);
                sendNcpCtrl(pck, ifcPppLcp.pppCtrl, ifcPppNcp.codeProtRej, bits.randomB());
                break;
        }
        if (newProt < 0) {
            return;
        }
        if (curMode != modeUp) {
            return;
        }
        pck.msbPutW(0, newProt);
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        int prot = pck.msbGetW(0); // ethertype
        pck.getSkip(2);
        int newProt = -1;
        switch (prot) {
            case ifcPppMpls.ethTypU:
                ctrlMpls.setLocalNeed();
                newProt = ifcPppMpls.pppDataU;
                break;
            case ifcPppIp4.ethTyp:
                ctrlIp4.setLocalNeed();
                newProt = ifcPppIp4.pppData;
                break;
            case ifcPppIp6.ethTyp:
                ctrlIp6.setLocalNeed();
                newProt = ifcPppIp6.pppData;
                break;
            case ifcPppBrdg.ethTyp:
                ifcPppBrdg.patchPackTx(pck);
                ctrlBrdg.setLocalNeed();
                newProt = ifcPppBrdg.pppData;
                break;
            case ifcPppOsi.ethTyp:
                ctrlOsi.setLocalNeed();
                newProt = ifcPppOsi.pppData;
                break;
            case ifcPppIpx.ethTyp:
                ctrlIpx.setLocalNeed();
                newProt = ifcPppIpx.pppData;
                break;
            case ifcPppCrypt.ethTyp:
                ctrlCrypt.setLocalNeed();
                newProt = ifcPppCrypt.pppData;
                break;
            case ifcPppNsh.ethTyp:
                ctrlNsh.setLocalNeed();
                newProt = ifcPppNsh.pppData;
                break;
            case ifcPppMpls.ethTypM:
                ctrlMpls.setLocalNeed();
                newProt = ifcPppMpls.pppDataM;
                break;
            default:
                cntr.drop(pck, counter.reasons.badEthTyp);
                return;
        }
        if (newProt < 0) {
            return;
        }
        if (curMode != modeUp) {
            return;
        }
        putAddrCtrlProto(pck, newProt);
        lower.sendPack(pck);
    }

}

class ifcPppTxKeep extends TimerTask {

    private ifcPpp lower;

    public ifcPppTxKeep(ifcPpp parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendKeepReq();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

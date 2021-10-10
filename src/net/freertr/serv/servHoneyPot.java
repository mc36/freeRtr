package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.clnt.clntDns;
import net.freertr.pack.packDnsRec;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userExec;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userReader;
import net.freertr.util.cmds;
import net.freertr.util.logger;

/**
 * honeypot server
 *
 */
public class servHoneyPot extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servHoneyPot() {
    }

    /**
     * port number
     */
    public static final int port = 22;

    /**
     * command to run
     */
    public String command;

    /**
     * resolve addresses
     */
    public boolean resolve;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server honeypot .*! port " + port,
        "server honeypot .*! protocol " + proto2string(protoAllStrm),
        "server honeypot .*! no command",
        "server honeypot .*! no resolve"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "honeypot";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        cmds.cfgLine(lst, command == null, beg, "command", command);
        cmds.cfgLine(lst, !resolve, beg, "resolve", "");
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("command")) {
            command = cmd.getRemaining();
            return false;
        }
        if (s.equals("resolve")) {
            resolve = true;
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("command")) {
            command = null;
            return false;
        }
        if (s.equals("resolve")) {
            resolve = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  command                      command to execute");
        l.add("2 2,.  <name>                     exec command");
        l.add("1 .  resolve                      resolve addresses");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(60000);
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        new servHoneyPotConn(this, pipe, id.peerAddr.copyBytes(), id.portRem);
        return false;
    }

}

class servHoneyPotConn implements Runnable {

    private servHoneyPot lower;

    private pipeSide pipe;

    private addrIP addr;

    private int port;

    public servHoneyPotConn(servHoneyPot parent, pipeSide conn, addrIP peer, int prt) {
        lower = parent;
        pipe = conn;
        addr = peer;
        port = prt;
        new Thread(this).start();
    }

    public void run() {
        String s = "" + addr;
        if (lower.resolve) {
            clntDns clnt = new clntDns();
            clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(addr), false, packDnsRec.typePTR);
            s = s + " - " + clnt.getPTR();
        }
        s = s + " - " + port;
        pipe.linePut("you (" + s + ") have been logged!");
        pipe.setClose();
        logger.info("honeypot hit from " + s);
        if (lower.command == null) {
            return;
        }
        pipeLine pl = new pipeLine(32768, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userReader rdr = new userReader(pip, null);
        pip.settingsPut(pipeSetting.height, 0);
        userExec exe = new userExec(pip, rdr);
        exe.privileged = true;
        pip.setTime(60000);
        String a = exe.repairCommand(lower.command + " " + addr);
        exe.executeCommand(a);
        pip = pl.getSide();
        pl.setClose();
    }

}

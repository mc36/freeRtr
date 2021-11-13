package net.freertr.serv;

import java.util.List;
import net.freertr.line.lineBstun;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userLine;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * block serial tunneling
 *
 * @author matecsaba
 */
public class servBstun extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servBstun() {
    }

    /**
     * group number
     */
    public int grpNum;

    private userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server bstun .*! port " + lineBstun.port,
        "server bstun .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        lineBstun bst = new lineBstun(pipe, grpNum);
        lin.createHandler(bst.getPipe(), "" + id, 0);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        lin.getShRun(beg, l);
        l.add(beg + "group " + grpNum);
    }

    public boolean srvCfgStr(cmds cmd) {
        cmds c = cmd.copyBytes(false);
        String s = c.word();
        if (s.equals("group")) {
            grpNum = bits.str2num(c.word());
            return false;
        }
        return lin.doCfgStr(cmd);
    }

    public void srvHelp(userHelping l) {
        lin.getHelp(l);
        l.add(null, "1 2  group                        set group number");
        l.add(null, "2 .    <num>                      group number");
    }

    public String srvName() {
        return "bstun";
    }

    public int srvPort() {
        return lineBstun.port;
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

}

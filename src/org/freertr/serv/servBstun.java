package org.freertr.serv;

import java.util.List;
import org.freertr.line.lineBstun;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelping;
import org.freertr.user.userLine;
import org.freertr.util.bits;
import org.freertr.util.cmds;

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
        "server bstun .*!" + cmds.tabulator + "port " + lineBstun.port,
        "server bstun .*!" + cmds.tabulator + "protocol " + proto2string(protoAllStrm)
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

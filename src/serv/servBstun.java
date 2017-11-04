package serv;

import java.util.List;
import line.lineBstun;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import user.userLine;
import util.bits;
import util.cmds;

/**
 * block serial tunneling
 *
 * @author matecsaba
 */
public class servBstun extends servGeneric implements prtServS {

    /**
     * group number
     */
    public int grpNum;

    private userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
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
        lin.createHandler(bst.getPipe(), "" + id, false);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
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
        l.add("1 2  group                        set group number");
        l.add("2 .    <num>                      group number");
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

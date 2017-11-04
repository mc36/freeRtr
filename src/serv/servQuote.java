package serv;

import java.util.Comparator;
import java.util.List;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * quote of the day protocol (rfc865) server
 *
 * @author matecsaba
 */
public class servQuote extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 17;

    /**
     * lines
     */
    protected tabGen<servQuoteLine> lines = new tabGen<servQuoteLine>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server quote .*! port " + port,
        "server quote .*! protocol " + proto2string(protoAll)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "quote";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAll;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst) {
        for (int i = 0; i < lines.size(); i++) {
            servQuoteLine ntry = lines.get(i);
            if (ntry == null) {
                continue;
            }
            lst.add(beg + "tagline " + ntry.line);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("tagline")) {
            servQuoteLine l = new servQuoteLine(cmd.getRemaining());
            lines.put(l);
            return false;
        }
        if (!a.equals("no")) {
            return true;
        }
        a = cmd.word();
        if (a.equals("tagline")) {
            servQuoteLine l = new servQuoteLine(cmd.getRemaining());
            lines.del(l);
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  tagline                      add tagline");
        l.add("2 2,.  <text>                     line to add");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 5000;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        new servQuoteConn(this, pipe);
        return false;
    }

}

class servQuoteLine implements Comparator<servQuoteLine> {

    public String line;

    public servQuoteLine(String ln) {
        line = ln;
    }

    public int compare(servQuoteLine o1, servQuoteLine o2) {
        return o1.line.compareTo(o2.line);
    }

}

class servQuoteConn implements Runnable {

    private pipeSide pipe;

    private servQuote lower;

    public servQuoteConn(servQuote parent, pipeSide conn) {
        pipe = conn;
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        servQuoteLine ntry = lower.lines.get(bits.random(0, lower.lines.size()));
        if (ntry != null) {
            pipe.linePut(ntry.line);
        }
        pipe.setClose();
    }

}

package org.freertr.serv;

import java.util.List;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * quote of the day protocol (rfc865) server
 *
 * @author matecsaba
 */
public class servQuote extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servQuote() {
    }

    /**
     * port number
     */
    public final static int port = 17;

    /**
     * lines
     */
    protected tabGen<servQuoteLine> lines = new tabGen<servQuoteLine>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server quote .*", cmds.tabulator + "port " + port, null),
        new userFilter("server quote .*", cmds.tabulator + "protocol " + proto2string(protoAll), null)
    };

    public userFilter[] srvDefFlt() {
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

    public void srvShRun(String beg, List<String> lst, int filter) {
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
        if (!a.equals(cmds.negated)) {
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

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "tagline", "add tagline");
        l.add(null, false, 2, new int[]{2, -1}, "<text>", "line to add");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(5000);
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        new servQuoteConn(this, pipe);
        return false;
    }

    /**
     * get one line
     *
     * @return line
     */
    public String getOneLine() {
        servQuoteLine ntry = lines.get(bits.random(0, lines.size() + 1));
        if (ntry == null) {
            return "n/a";
        }
        return ntry.line;
    }

}

class servQuoteLine implements Comparable<servQuoteLine> {

    public String line;

    public servQuoteLine(String ln) {
        line = ln;
    }

    public int compareTo(servQuoteLine o) {
        return line.compareTo(o.line);
    }

}

class servQuoteConn implements Runnable {

    private pipeSide pipe;

    private servQuote lower;

    public servQuoteConn(servQuote parent, pipeSide conn) {
        pipe = conn;
        lower = parent;
        logger.startThread(this);
    }

    public void run() {
        pipe.linePut(lower.getOneLine());
        pipe.setClose();
    }

}

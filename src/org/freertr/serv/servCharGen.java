package org.freertr.serv;

import java.util.List;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * chargen (rfc864) server
 *
 * @author matecsaba
 */
public class servCharGen extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servCharGen() {
    }

    /**
     * port number
     */
    public final static int port = 19;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server chargen .*", cmds.tabulator + "port " + port, null),
        new userFilter("server chargen .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(10000);
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servCharGenDoer(pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
    }

    public boolean srvCfgStr(cmds cmd) {
        return true;
    }

    public void srvHelp(userHelp l) {
    }

    public String srvName() {
        return "chargen";
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

}

class servCharGenDoer implements Runnable {

    private final String chars = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ";

    private final String chars2 = chars + chars;

    private pipeSide pipe;

    public servCharGenDoer(pipeSide stream) {
        pipe = stream;
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                byte[] buf = new byte[1024];
                pipe.nonBlockGet(buf, 0, buf.length);
                if (pipe.isClosed() != 0) {
                    break;
                }
                for (int i = 0; i < chars.length(); i++) {
                    pipe.linePut(chars2.substring(i, i + 72));
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}

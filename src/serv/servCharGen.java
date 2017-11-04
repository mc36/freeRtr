package serv;

import java.util.List;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.cmds;
import util.logger;

/**
 * chargen (rfc864) server
 *
 * @author matecsaba
 */
public class servCharGen extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 19;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server chargen .*! port " + port,
        "server chargen .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 10000;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servCharGenDoer(pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
    }

    public boolean srvCfgStr(cmds cmd) {
        return true;
    }

    public void srvHelp(userHelping l) {
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
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                byte buf[] = new byte[1024];
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

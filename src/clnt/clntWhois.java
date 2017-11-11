package clnt;

import pipe.pipeDiscard;
import pipe.pipeProgress;
import pipe.pipeSide;
import serv.servGeneric;
import user.userTerminal;

/**
 * whois (rfc3912) client
 *
 * @author matecsaba
 */
public class clntWhois {

    /**
     * port number
     */
    public static final int port = 43;

    private pipeSide cons;

    /**
     * create new client
     *
     * @param console console to use
     */
    public clntWhois(pipeSide console) {
        cons = console;
    }

    /**
     * do one query
     *
     * @param h host
     * @param q query
     */
    public void doQuery(String h, String q) {
        pipeProgress con = new pipeProgress(pipeDiscard.needAny(cons));
        pipeSide pipe = new userTerminal(con).resolvAndConn(servGeneric.protoTcp, h, port, "whois");
        if (pipe == null) {
            return;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.linePut(q);
        for (;;) {
            String s = pipe.lineGet(0);
            cons.linePut(s);
            if (pipe.isClosed() != 0) {
                break;
            }
        }
        pipe.setClose();
    }

}

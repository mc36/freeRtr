package net.freertr.clnt;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeReader;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.user.userTerminal;

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

    private final clntProxy proxy;

    private final String server;

    private final pipeSide console;

    /**
     * create new client
     *
     * @param con console to log
     * @param srv server to use
     * @param prx proxy to use
     */
    public clntWhois(pipeSide con, clntProxy prx, String srv) {
        console = pipeDiscard.needAny(con);
        server = srv;
        proxy = prx;
    }

    /**
     * do one query
     *
     * @param quest question to ask
     * @return response, null if error
     */
    public List<String> doQuery(String quest) {
        console.linePut("querying " + quest + " at " + server);
        if (proxy == null) {
            return null;
        }
        addrIP trg = userTerminal.justResolv(server, 0);
        if (trg == null) {
            return null;
        }
        pipeProgress con = new pipeProgress(pipeDiscard.needAny(null));
        pipeSide pipe = proxy.doConnect(servGeneric.protoTcp, trg, port, "whois");
        if (pipe == null) {
            return null;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.linePut(quest);
        pipeReader rd = new pipeReader();
        rd.setLineMode(pipeSide.modTyp.modeCRorLF);
        pipeConnect.connect(pipe, rd.getPipe(), true);
        rd.waitFor();
        return rd.getResult();
    }

}

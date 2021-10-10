package net.freertr.clnt;

import java.util.List;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeReader;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.user.userTerminal;
import net.freertr.util.cmds;

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

    /**
     * question
     */
    public String quest;

    private final String server;

    /**
     * create new client
     *
     * @param srv server to use
     */
    public clntWhois(String srv) {
        server = srv;
    }

    /**
     * do one query
     *
     * @param cmd console to use, null if nothing
     * @return response, null if error
     */
    public List<String> doQuery(cmds cmd) {
        if (cmd != null) {
            cmd.error("querying " + quest + " at " + server + " " + port);
        }
        pipeProgress con = new pipeProgress(pipeDiscard.needAny(null));
        pipeSide pipe = new userTerminal(con).resolvAndConn(servGeneric.protoTcp, server, port, "whois");
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

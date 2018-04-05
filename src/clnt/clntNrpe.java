package clnt;

import java.util.ArrayList;
import java.util.List;
import pack.packNrpe;
import pipe.pipeDiscard;
import pipe.pipeProgress;
import pipe.pipeSide;
import serv.servGeneric;
import user.userTerminal;
import util.debugger;
import util.logger;

/**
 * nagios remote plugin client
 *
 * @author matecsaba
 */
public class clntNrpe {

    private pipeProgress cons;

    /**
     * server address
     */
    public String server;

    /**
     * check name
     */
    public String check;

    /**
     * result code
     */
    public int code;

    /**
     * result text
     */
    public List<String> text;

    /**
     * create new client
     *
     * @param console console to use
     */
    public clntNrpe(pipeSide console) {
        cons = new pipeProgress(pipeDiscard.needAny(console));
    }

    /**
     * perform check
     *
     * @return false on success, true on error
     */
    public boolean doCheck() {
        code = packNrpe.coUnk;
        text = new ArrayList<String>();
        pipeSide pipe = new userTerminal(cons).resolvAndConn(servGeneric.protoTcp, server, packNrpe.portNum, "nrpe");
        if (pipe == null) {
            text.add(check + " CRITICAL failed to connect to " + server);
            return true;
        }
        packNrpe pck = new packNrpe();
        pck.ver = 3;
        pck.typ = packNrpe.tyReq;
        pck.str = check;
        if (debugger.clntNrpeTraf) {
            logger.debug("tx:" + pck.dump());
        }
        pck.sendPack(pipe);
        boolean b = pck.recvPack(pipe);
        pipe.setClose();
        if (debugger.clntNrpeTraf) {
            logger.debug("rx:" + pck.dump());
        }
        if (b) {
            text.add(check + " CRITICAL got nothing from " + server);
            return true;
        }
        if (pck.typ != packNrpe.tyRep) {
            text.add(check + " CRITICAL got invalid packet from " + server);
            return true;
        }
        code = pck.cod;
        byte[] buf = pck.str.getBytes();
        String s = "";
        for (int i = 0; i < buf.length; i++) {
            if (buf[i] == pck.sep[0]) {
                if (s.length() < 1) {
                    continue;
                }
                text.add(s);
                s = "";
                continue;
            }
            byte[] cb = new byte[1];
            cb[0] = buf[i];
            s += new String(cb);
        }
        if (s.length() > 0) {
            text.add(s);
        }
        return pck.cod != packNrpe.coOk;
    }

}

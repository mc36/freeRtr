package net.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.pack.packNrpe;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.user.userTerminal;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * nagios remote plugin client
 *
 * @author matecsaba
 */
public class clntNrpe {

    private final clntProxy proxy;

    private final String server;

    private final pipeSide console;

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
     * @param con console to use
     * @param srv server to use
     * @param prx proxy to use
     */
    public clntNrpe(pipeSide con, clntProxy prx, String srv) {
        console = pipeDiscard.needAny(con);
        server = srv;
        proxy = prx;
    }

    /**
     * perform check
     *
     * @param check check to read
     * @return false on success, true on error
     */
    public boolean doCheck(String check) {
        console.linePut("querying " + check + " at " + server + " " + packNrpe.portNum);
        if (proxy == null) {
            return true;
        }
        addrIP trg = userTerminal.justResolv(server, 0);
        if (trg == null) {
            return true;
        }
        code = packNrpe.coUnk;
        text = new ArrayList<String>();
        pipeSide pipe = proxy.doConnect(servGeneric.protoTcp, trg, packNrpe.portNum, "nrpe");
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

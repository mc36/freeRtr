package org.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.pack.packNrpe;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGen;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.syncInt;

/**
 * nagios remote plugin client
 *
 * @author matecsaba
 */
public class clntNrpe {

    /**
     * startup counter
     */
    public final static syncInt cntrStart = new syncInt(0);

    /**
     * error counter
     */
    public final static syncInt cntrError = new syncInt(0);

    /**
     * stop counter
     */
    public final static syncInt cntrStop = new syncInt(0);

    private final cfgVrf vrf;

    private final cfgIfc ifc;

    private final String server;

    private final pipeSide console;

    /**
     * timeout
     */
    public int timeout = 60000;

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
     * @param v vrf to use
     * @param i interface to use
     */
    public clntNrpe(pipeSide con, cfgVrf v, cfgIfc i, String srv) {
        console = pipeDiscard.needAny(con);
        server = srv;
        vrf = v;
        ifc = i;
    }

    /**
     * perform check
     *
     * @param check check to read
     * @return false on success, true on error
     */
    public boolean doCheck(String check) {
        cntrStart.add(1);
        console.linePut("querying " + check + " at " + server);
        addrIP trg = clntDns.justResolv(server, 0);
        if (trg == null) {
            cntrError.add(1);
            return true;
        }
        code = packNrpe.coUnk;
        text = new ArrayList<String>();
        ipFwd fwd = vrf.getFwd(trg);
        prtGen prt = vrf.getTcp(trg);
        ipFwdIface ipif = null;
        if (ifc != null) {
            ipif = ifc.getFwdIfc(trg);
        }
        if (ipif == null) {
            ipif = ipFwdTab.findSendingIface(fwd, trg);
        }
        pipeSide pipe = prt.streamConnect(new pipeLine(65536, false), ipif, 0, trg, packNrpe.portNum, "nrpe", -1, null, -1, -1);
        if (pipe == null) {
            text.add(check + " CRITICAL failed to connect to " + server);
            cntrError.add(1);
            return true;
        }
        pipe.wait4ready(timeout);
        pipe.setTime(timeout);
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
            cntrError.add(1);
            return true;
        }
        if (pck.typ != packNrpe.tyRep) {
            text.add(check + " CRITICAL got invalid packet from " + server);
            cntrError.add(1);
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
        cntrStop.add(1);
        return pck.cod != packNrpe.coOk;
    }

}

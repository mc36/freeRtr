package net.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdTab;
import net.freertr.pack.packNrpe;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGen;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * nagios remote plugin client
 *
 * @author matecsaba
 */
public class clntNrpe {

    private final cfgVrf vrf;

    private final cfgIfc ifc;

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
        console.linePut("querying " + check + " at " + server);
        addrIP trg = userTerminal.justResolv(server, 0);
        if (trg == null) {
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
        pipeSide pipe = prt.streamConnect(new pipeLine(65536, false), ipif, 0, trg, packNrpe.portNum, "nrpe", null, -1);
        if (pipe == null) {
            text.add(check + " CRITICAL failed to connect to " + server);
            return true;
        }
        pipe.wait4ready(60000);
        pipe.setTime(60000);
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

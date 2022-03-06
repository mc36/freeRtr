package net.freertr.clnt;

import net.freertr.addr.addrIP;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAll;
import net.freertr.pack.packHolder;
import net.freertr.pack.packRadius;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.serv.servRadius;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * remote authentication dialin user (rfc2865) client
 *
 * @author matecsaba
 */
public class clntRadius {

    /**
     * create instance
     *
     * @param prx proxy
     */
    public clntRadius(clntProxy prx) {
        proxy = prx;
    }

    /**
     * target server
     */
    public String server = null;

    /**
     * shared secret
     */
    public String secret = null;

    /**
     * proxy to use
     */
    public clntProxy proxy;

    private String radUsr;

    private String radPwd;

    private packRadius radTx;

    private packRadius radRx;

    /**
     * do pap transaction
     *
     * @param user username
     * @param pass password
     * @return false on completion, true on error
     */
    public boolean doPap(String user, String pass) {
        radUsr = user;
        radPwd = pass;
        radTx = new packRadius();
        radTx.valUsrPwd = pass;
        return doXchg();
    }

    /**
     * do chap transaction
     *
     * @param user username
     * @param id id used
     * @param chal challenge sent
     * @param resp response got
     * @return false on completion, true on error
     */
    public boolean doChap(String user, int id, byte[] chal, byte[] resp) {
        radUsr = user;
        radPwd = "";
        radTx = new packRadius();
        radTx.valChpIdn = id;
        radTx.valChpChl = chal;
        radTx.valChpPwd = resp;
        return doXchg();
    }

    private boolean doXchg() {
        if (secret == null) {
            return true;
        }
        if (server == null) {
            return true;
        }
        addrIP trg = userTerminal.justResolv(server, 0);
        if (trg == null) {
            return true;
        }
        clntProxy prx = cfgAll.getClntPrx(proxy);
        if (prx == null) {
            return true;
        }
        pipeSide conn = prx.doConnect(servGeneric.protoUdp, trg, new servRadius().srvPort(), "radius");
        if (conn == null) {
            return true;
        }
        conn.setTime(5000);
        radTx.secret = secret;
        radTx.valUsrNam = radUsr;
        radTx.valNasPrt = 2;
        radTx.valNasId = "vty";
        radTx.valPrtTyp = 5;
        radTx.auther = new byte[16];
        for (int i = 0; i < radTx.auther.length; i++) {
            radTx.auther[i] = (byte) bits.randomB();
        }
        radTx.code = packRadius.typeAccReq;
        radTx.idnt = bits.randomB();
        if (debugger.clntRadiusTraf) {
            logger.debug("tx " + radTx.dump());
        }
        packHolder pckBin = new packHolder(true, true);
        radTx.createPacket(pckBin, false, null);
        pckBin.pipeSend(conn, 0, pckBin.dataSize(), 2);
        pckBin = conn.readPacket(true);
        conn.setClose();
        if (pckBin == null) {
            return true;
        }
        packRadius res = new packRadius();
        if (res.parsePacket(pckBin)) {
            return true;
        }
        if (debugger.clntRadiusTraf) {
            logger.debug("rx " + res.dump());
        }
        if (res.idnt != radTx.idnt) {
            return true;
        }
        byte[] buf = pckBin.getCopy();
        buf = packRadius.calcReplyAuthen(res.code, res.idnt, secret, radTx.auther, buf, 0, buf.length);
        for (int i = 0; i < buf.length; i++) {
            if (buf[i] != res.auther[i]) {
                return true;
            }
        }
        radRx = res;
        return false;
    }

    /**
     * check if access allowed
     *
     * @param par parent
     * @param priv privilege on success
     * @return result
     */
    public authResult checkResult(authGeneric par, int priv) {
        if (radTx == null) {
            return new authResult(par, authResult.authServerError, radUsr, radPwd);
        }
        if (radRx == null) {
            return new authResult(par, authResult.authServerError, radUsr, radPwd);
        }
        if (radRx.code != packRadius.typeAccAcc) {
            return new authResult(par, authResult.authBadUserPass, radUsr, radPwd);
        }
        authResult res = new authResult(par, authResult.authSuccessful, radUsr, radPwd);
        res.privilege = priv;
        if (radRx.valMgtPrv > 0) {
            res.privilege = radRx.valMgtPrv;
        }
        if (radRx.valFrmAdr4 != null) {
            res.ipv4addr = radRx.valFrmAdr4.copyBytes();
        }
        if (radRx.valFrmRou4 != null) {
            res.ipv4route = radRx.valFrmRou4;
        }
        if (radRx.valFrmAdr6 != null) {
            res.ipv6addr = radRx.valFrmAdr6.copyBytes();
        }
        if (radRx.valFrmIfi != null) {
            res.ipv6ifid = radRx.valFrmIfi.copyBytes();
        }
        if (radRx.valFrmRou6 != null) {
            res.ipv6route = radRx.valFrmRou6;
        }
        return res;
    }

}

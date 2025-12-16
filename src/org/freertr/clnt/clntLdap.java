package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.pack.packLdap;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGeneric;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * lightweight directory access protocol (rfc4511) client
 *
 * @author matecsaba
 */
public class clntLdap {

    /**
     * create instance
     *
     * @param prx proxy
     */
    public clntLdap(clntProxy prx) {
        proxy = prx;
    }

    /**
     * target server
     */
    public String server = null;

    /**
     * target port
     */
    public int port;

    /**
     * proxy to use
     */
    public clntProxy proxy;

    private packLdap ldaTx;

    private packLdap ldaRx;

    private String ldaUsr;

    private String ldaPwd;

    /**
     * do pap transaction
     *
     * @param user username
     * @param pass password
     * @return false on completion, true on error
     */
    public boolean doPap(String user, String pass) {
        ldaUsr = user;
        ldaPwd = pass;
        ldaTx = new packLdap();
        ldaTx.usr = user;
        ldaTx.pwd = pass;
        return doAuthenXchg();
    }

    private boolean doAuthenXchg() {
        if (server == null) {
            return true;
        }
        addrIP trg = clntDns.justResolv(server, 0);
        if (trg == null) {
            return true;
        }
        clntProxy prx = cfgAll.getClntPrx(proxy);
        if (prx == null) {
            return true;
        }
        if (port < 1) {
            port = packLdap.port;
        }
        pipeSide conn = prx.doConnect(servGeneric.protoTcp, trg, port, "ldap");
        if (conn == null) {
            return true;
        }
        String user = ldaTx.usr;
        String pass = ldaTx.pwd;
        conn.setTime(5000);
        ldaTx.seq = bits.randomD();
        ldaTx.pipe = conn;
        ldaTx.createBindReq();
        ldaTx.packSend();
        if (debugger.clntLdapTraf) {
            logger.debug("tx " + ldaTx.dump());
        }
        packLdap res = new packLdap();
        res.pipe = conn;
        if (res.packRecv()) {
            conn.setClose();
            return true;
        }
        if (res.parseBindRep()) {
            conn.setClose();
            return true;
        }
        conn.setClose();
        ldaRx = res;
        if (debugger.clntLdapTraf) {
            logger.debug("rx " + ldaRx.dump());
        }
        return false;
    }

    /**
     * check if access allowed
     *
     * @param par parent
     * @param priv privilege on success
     * @return result
     */
    public authResult checkAuthenResult(authGeneric par, int priv) {
        if (ldaRx == null) {
            return new authResult(par, authResult.authServerError, ldaUsr, ldaPwd);
        }
        if (ldaRx.cod != packLdap.cdSucc) {
            return new authResult(par, authResult.authBadUserPass, ldaUsr, ldaPwd);
        }
        authResult res = new authResult(par, authResult.authSuccessful, ldaUsr, ldaPwd);
        res.privilege = priv;
        return res;
    }

}

package net.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAll;
import net.freertr.pack.packTacacs;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.serv.servTacacs;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * terminal access controller access control system (rfc1492) client
 *
 * @author matecsaba
 */
public class clntTacacs {

    /**
     * create instance
     *
     * @param prx proxy
     */
    public clntTacacs(clntProxy prx) {
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

    private packTacacs tacTx;

    private packTacacs tacRx;

    private String tacUsr;

    private String tacPwd;

    /**
     * do pap transaction
     *
     * @param user username
     * @param pass password
     * @return false on completion, true on error
     */
    public boolean doPap(String user, String pass) {
        tacUsr = user;
        tacPwd = pass;
        tacTx = new packTacacs();
        tacTx.auty = packTacacs.autyAscii;
        tacTx.usr = user;
        tacTx.dat = pass;
        return doAuthenXchg();
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
        tacUsr = user;
        tacPwd = "";
        tacTx = new packTacacs();
        tacTx.auty = packTacacs.autyChap;
        tacTx.usr = user;
        byte[] buf = new byte[1];
        buf[0] = (byte) id;
        buf = bits.byteConcat(buf, chal);
        buf = bits.byteConcat(buf, resp);
        tacTx.dat = new String(buf);
        return doAuthenXchg();
    }

    private boolean doAuthenXchg() {
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
        pipeSide conn = prx.doConnect(servGeneric.protoTcp, trg, new servTacacs().srvPort(), "tacacs");
        if (conn == null) {
            return true;
        }
        String user = tacTx.usr;
        String pass = tacTx.dat;
        if (tacTx.auty == packTacacs.autyAscii) {
            tacTx.usr = "";
            tacTx.dat = "";
        }
        conn.setTime(5000);
        tacTx.ses = bits.randomD();
        tacTx.pipe = conn;
        tacTx.secret = secret;
        tacTx.act = packTacacs.actLogin;
        tacTx.priv = 1;
        tacTx.srv = packTacacs.srvLogin;
        tacTx.adr = "";
        tacTx.prt = "";
        tacTx.createAuthenStrt();
        tacTx.packSend();
        if (debugger.clntTacacsTraf) {
            logger.debug("tx " + tacTx.dump());
        }
        packTacacs res = new packTacacs();
        res.pipe = conn;
        res.secret = secret;
        if (res.packRecv()) {
            conn.setClose();
            return true;
        }
        if (tacTx.auty == packTacacs.autyAscii) {
            res.parseAuthenCont();
            if (debugger.clntTacacsTraf) {
                logger.debug("rx " + res.dump());
            }
            tacTx.act = 0;
            tacTx.usr = user;
            tacTx.seq = res.seq;
            tacTx.createAuthenCont();
            tacTx.packSend();
            if (debugger.clntTacacsTraf) {
                logger.debug("tx " + tacTx.dump());
            }
            if (res.packRecv()) {
                conn.setClose();
                return true;
            }
            res.parseAuthenCont();
            if (debugger.clntTacacsTraf) {
                logger.debug("rx " + res.dump());
            }
            tacTx.usr = pass;
            tacTx.seq = res.seq;
            tacTx.createAuthenCont();
            tacTx.packSend();
            if (debugger.clntTacacsTraf) {
                logger.debug("tx " + tacTx.dump());
            }
            if (res.packRecv()) {
                conn.setClose();
                return true;
            }
            tacTx.usr = user;
        }
        conn.setClose();
        if (res.parseAuthenRply()) {
            return true;
        }
        tacRx = res;
        if (debugger.clntTacacsTraf) {
            logger.debug("rx " + tacRx.dump());
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
        if (tacRx == null) {
            return new authResult(par, authResult.authServerError, tacUsr, tacPwd);
        }
        if (tacRx.act != packTacacs.sttPass) {
            return new authResult(par, authResult.authBadUserPass, tacUsr, tacPwd);
        }
        authResult res = new authResult(par, authResult.authSuccessful, tacUsr, tacPwd);
        res.privilege = priv;
        return res;
    }

    /**
     * do command transaction
     *
     * @param par parent
     * @param usr username
     * @param cmd command
     * @return result
     */
    public authResult doCmd(authGeneric par, String usr, String cmd) {
        tacTx = new packTacacs();
        tacTx.usr = usr;
        tacTx.ses = bits.randomD();
        tacTx.secret = secret;
        if (server == null) {
            return new authResult(par, authResult.authServerError, usr, cmd);
        }
        addrIP trg = userTerminal.justResolv(server, 0);
        if (trg == null) {
            return new authResult(par, authResult.authServerError, usr, cmd);
        }
        pipeSide conn = cfgAll.clntConnect(servGeneric.protoTcp, trg, new servTacacs().srvPort(), "tacacs");
        if (conn == null) {
            return new authResult(par, authResult.authServerError, usr, cmd);
        }
        tacTx.pipe = conn;
        tacTx.act = packTacacs.metNotset;
        tacTx.priv = 15;
        tacTx.auty = packTacacs.autyNotset;
        tacTx.prt = "";
        tacTx.adr = "";
        List<String> lst = new ArrayList<String>();
        lst.add("service=shell");
        cmds cm = new cmds("tac", cmd);
        lst.add("cmd=" + cm.word());
        for (;;) {
            String a = cm.word();
            if (a.length() < 1) {
                break;
            }
            lst.add("cmd-arg=" + a);
        }
        lst.add("cmd-arg=<cr>");
        tacTx.arg = new String[lst.size()];
        for (int i = 0; i < tacTx.arg.length; i++) {
            tacTx.arg[i] = lst.get(i);
        }
        if (debugger.clntTacacsTraf) {
            logger.debug("tx " + tacTx.dump());
        }
        tacTx.createAuthorReq();
        tacTx.packSend();
        tacRx = new packTacacs();
        tacRx.pipe = conn;
        tacRx.secret = secret;
        if (tacRx.packRecv()) {
            conn.setClose();
            return new authResult(par, authResult.authServerError, usr, cmd);
        }
        conn.setClose();
        if (tacRx.parseAuthorRep()) {
            return new authResult(par, authResult.authServerError, usr, cmd);
        }
        if (debugger.clntTacacsTraf) {
            logger.debug("rx " + tacRx.dump());
        }
        if ((tacRx.srv == packTacacs.staPassAdd) || (tacRx.srv == packTacacs.staPassRep)) {
            return new authResult(par, authResult.authSuccessful, usr, cmd);
        } else {
            return new authResult(par, authResult.authBadUserPass, usr, cmd);
        }
    }

}

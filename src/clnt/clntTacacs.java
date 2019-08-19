package clnt;

import pack.packTacacs;
import pipe.pipeSide;
import serv.servTacacs;
import user.userTerminal;
import util.bits;
import util.debugger;
import util.logger;
import addr.addrIP;
import auth.authGeneric;
import auth.authResult;
import cfg.cfgAll;
import java.util.ArrayList;
import java.util.List;
import serv.servGeneric;
import util.cmds;

/**
 * terminal access controller access control system (rfc1492) client
 *
 * @author matecsaba
 */
public class clntTacacs {

    /**
     * target server
     */
    public String server = null;

    /**
     * shared secret
     */
    public String secret = null;

    private packTacacs tacTx;

    private packTacacs tacRx;

    /**
     * do pap transaction
     *
     * @param user username
     * @param pass password
     * @return false on completion, true on error
     */
    public boolean doPap(String user, String pass) {
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
        pipeSide conn = cfgAll.clntConnect(servGeneric.protoTcp, trg, new servTacacs().srvPort(), "tacacs");
        if (conn == null) {
            return true;
        }
        String user = tacTx.usr;
        String pass = tacTx.dat;
        if (tacTx.auty == packTacacs.autyAscii) {
            tacTx.usr = "";
            tacTx.dat = "";
        }
        conn.timeout = 5000;
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
            return new authResult(par, authResult.authServerError, tacTx.usr);
        }
        if (tacRx.act != packTacacs.sttPass) {
            return new authResult(par, authResult.authBadUserPass, tacTx.usr);
        }
        authResult res = new authResult(par, authResult.authSuccessful, tacTx.usr);
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
            return new authResult(par, authResult.authServerError, tacTx.usr);
        }
        addrIP trg = userTerminal.justResolv(server, 0);
        if (trg == null) {
            return new authResult(par, authResult.authServerError, tacTx.usr);
        }
        pipeSide conn = cfgAll.clntConnect(servGeneric.protoTcp, trg, new servTacacs().srvPort(), "tacacs");
        if (conn == null) {
            return new authResult(par, authResult.authServerError, tacTx.usr);
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
            return new authResult(par, authResult.authServerError, tacTx.usr);
        }
        conn.setClose();
        if (tacRx.parseAuthorRep()) {
            return new authResult(par, authResult.authServerError, tacTx.usr);
        }
        if (debugger.clntTacacsTraf) {
            logger.debug("rx " + tacRx.dump());
        }
        if ((tacRx.srv == packTacacs.staPassAdd) || (tacRx.srv == packTacacs.staPassRep)) {
            return new authResult(par, authResult.authSuccessful, tacTx.usr);
        } else {
            return new authResult(par, authResult.authBadUserPass, tacTx.usr);
        }
    }

}

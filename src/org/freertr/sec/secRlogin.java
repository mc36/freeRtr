package org.freertr.sec;

import org.freertr.auth.authGeneric;
import org.freertr.auth.authResult;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * remote login (rfc1282) protocol
 *
 * @author matecsaba
 */
public class secRlogin implements Runnable {

    /**
     * lower layer session to use for encrypted communication
     */
    public final pipeSide lower;

    /**
     * user side of cleartext pipeline
     */
    public final pipeSide userC;

    /**
     * mode of operation, true=client, false=server
     */
    protected boolean client;

    /**
     * my side of cleartext pipeline
     */
    protected final pipeSide userS;

    /**
     * cleartext pipeline
     */
    protected final pipeLine userP;

    /**
     * server authentication list
     */
    protected authGeneric servAuth;

    /**
     * authenticated user
     */
    public authResult servUser;

    /**
     * client username
     */
    protected String clntUser;

    /**
     * client password
     */
    protected String clntPass;

    /**
     * start rlogin on a session
     *
     * @param session pipeside to use as lower layer
     * @param pipe pipeline to use on user side
     */
    public secRlogin(pipeSide session, pipeLine pipe) {
        lower = session;
        lower.setTime(120 * 1000);
        userP = pipe;
        userS = pipe.getSide();
        userC = pipe.getSide();
        userC.setTime(120 * 1000);
        userS.setTime(120 * 1000);
    }

    /**
     * get user side pipeline
     *
     * @return cleartext pipeline
     */
    public pipeSide getPipe() {
        return userC;
    }

    /**
     * start client connection
     *
     * @param user username
     * @param pass password
     */
    public void startClient(String user, String pass) {
        client = true;
        clntUser = user;
        clntPass = pass;
        workerStart();
    }

    /**
     * start server connection
     *
     * @param auth authentication list
     */
    public void startServer(authGeneric auth) {
        client = false;
        servAuth = auth;
        workerStart();
    }

    private void workerStart() {
        logger.startThread(this);
    }

    public void run() {
        try {
            if (client) {
                workerClient();
            } else {
                workerServer();
            }
            userS.setReady();
            pipeConnect.connect(lower, userS, true);
        } catch (Exception e) {
            logger.traceback(e);
            lower.setClose();
            userP.setClose();
            return;
        }
    }

    /**
     * send string
     *
     * @param p pipe to use
     * @param a string to send
     */
    public static void sendStr(pipeSide p, String a) {
        byte[] b = bits.byteConcat(a.getBytes(), new byte[1]);
        p.blockingPut(b, 0, b.length);
    }

    /**
     * read string
     *
     * @param p pipe to use
     * @return string read
     */
    public static String readStr(pipeSide p) {
        String a = "";
        byte[] buf = new byte[1];
        for (;;) {
            if (p.blockingGet(buf, 0, buf.length) != buf.length) {
                break;
            }
            if (buf[0] == 0) {
                break;
            }
            a += (char) buf[0];
        }
        return a;
    }

    private void workerClient() {
        sendStr(lower, "");
        sendStr(lower, clntUser);
        sendStr(lower, clntPass);
        sendStr(lower, "ansi");
        readStr(lower);
    }

    private void workerServer() {
        readStr(lower);
        String usr = readStr(lower);
        String pwd = readStr(lower);
        readStr(lower);
        sendStr(lower, "");
        servUser = servAuth.authUserPass(usr, pwd);
        if (servUser == null) {
            lower.setClose();
            userP.setClose();
            return;
        }
        if (servUser.result != authResult.authSuccessful) {
            lower.setClose();
            userP.setClose();
            return;
        }
        userS.settingsPut(pipeSetting.authed, servUser);
    }

}

package org.freertr.auth;

import org.freertr.pack.packHolder;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * authentication worker
 *
 * @author matecsaba
 */
public abstract class autherDoer implements Runnable {

    /**
     * create instance
     */
    public autherDoer() {
    }

    /**
     * get worker
     *
     * @param prnt parent
     * @param proto protocol id
     * @return worker
     */
    public static autherDoer getWorker(authenDown prnt, int proto) {
        switch (proto) {
            case autherEap.pppCtrl:
                return new autherEap(prnt);
            case autherChap.pppCtrl:
                return new autherChap(prnt);
            case autherPap.pppCtrl:
                return new autherPap(prnt);
        }
        return null;
    }

    /**
     * get name
     *
     * @param proto protocol id
     * @return name
     */
    public static String getName(int proto) {
        switch (proto) {
            case autherEap.pppCtrl:
                return autherEap.pppName;
            case autherChap.pppCtrl:
                return autherChap.pppName;
            case autherPap.pppCtrl:
                return autherPap.pppName;
            default:
                return "unknown=" + proto;
        }
    }

    /**
     * the parent
     */
    protected authenDown parent;

    /**
     * still working
     */
    public boolean working = true;

    /**
     * result
     */
    public authResult result = new authResult();

    /**
     * sent username
     */
    public String sentUser = null;

    /**
     * sent password
     */
    public String sentPass = null;

    /**
     * authentication list to use for remote
     */
    public authGeneric authenRem = null;

    private notifier notif = null;

    private packHolder pendPck = null;

    private int pendCod;

    private int pendId;

    public String toString() {
        return working + "|" + result;
    }

    /**
     * 9
     * received one packet
     *
     * @param pck packet received
     * @param code code
     * @param id identification
     */
    protected abstract void authenRecv(packHolder pck, int code, int id);

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    protected abstract void authenSend(packHolder pck);

    /**
     * test if this a client or server side
     *
     * @return true means client, false means server
     */
    public boolean isClient() {
        return authenRem == null;
    }

    /**
     * send one request
     */
    public void sendReq() {
        if ((notif != null) && (pendPck != null)) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        authenSend(pck);
    }

    /**
     * received one packet
     *
     * @param pck packet received
     * @param code code
     * @param id identification
     */
    public void recvPck(packHolder pck, int code, int id) {
        if (notif == null) {
            authenRecv(pck, code, id);
            return;
        }
        if (pendPck != null) {
            return;
        }
        pendCod = code;
        pendId = id;
        pendPck = pck.copyBytes(true, true);
        notif.wakeup();
    }

    /**
     * start thread
     */
    public void startThread() {
        if (notif != null) {
            return;
        }
        notif = new notifier();
        new Thread(this).start();
    }

    /**
     * start thread
     */
    public void stopThread() {
        notif = null;
    }

    private void doWork() {
        for (;;) {
            if (!working) {
                return;
            }
            if (notif == null) {
                return;
            }
            notif.sleep(1000);
            if (pendPck == null) {
                continue;
            }
            authenRecv(pendPck, pendCod, pendId);
            pendPck = null;
        }
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        working = false;
        notif = null;
    }

}

package auth;

import pack.packHolder;

/**
 * authentication worker
 *
 * @author matecsaba
 */
public abstract class autherDoer {

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
    public boolean succeed = false;

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

    /**
     * received one packet
     *
     * @param pck packet received
     * @param code code
     * @param id identification
     */
    public abstract void recvPck(packHolder pck, int code, int id);

    /**
     * send one packet
     *
     * @param pck packet to send
     * @return true to send packet, false to discard it
     */
    public abstract boolean sendPck(packHolder pck);

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
        packHolder pck = new packHolder(true, true);
        sendPck(pck);
    }

}

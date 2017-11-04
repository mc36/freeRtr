package auth;

import pack.packHolder;

/**
 * authentication sender
 *
 * @author matecsaba
 */
public interface authenDown {

    /**
     * send authentication packet
     *
     * @param pck packet to update
     * @param proto protocol number
     * @param code code number
     * @param id id value
     * @param msg message
     */
    public void sendAuthPack(packHolder pck, int proto, int code, int id, String msg);

    /**
     * received authentication packet
     *
     * @param msg message
     */
    public void recvAuthPack(String msg);

}

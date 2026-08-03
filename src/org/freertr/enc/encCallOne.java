package org.freertr.enc;

import org.freertr.pack.packHolder;

/**
 * one call leg
 *
 * @author matecsaba
 */
public interface encCallOne {

    /**
     * close this side
     */
    public void setClose();

    /**
     * test if the any side of pipe was closed
     *
     * @return 0=no, 1=this side, 1=other side, 3=both sides
     */
    public int isClosed();

    /**
     * send one packet
     *
     * @param pck packet holder
     */
    public void sendPack(packHolder pck);

    /**
     * receive one packet
     *
     * @param pck buffer to use
     * @param blocking blocking mode
     * @param enforce check type
     * @return bytes received
     */
    public int recvPack(packHolder pck, boolean blocking, boolean enforce);

}

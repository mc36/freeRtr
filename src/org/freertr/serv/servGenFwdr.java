package org.freertr.serv;

import org.freertr.pack.packHolder;

/**
 * one forwarder entity
 *
 * @author matecsaba
 */
public interface servGenFwdr {

    /**
     * get hardware forwarder info
     *
     * @return offload info
     */
    public abstract String getShGenOneLiner();

    /**
     * send a packet through the api
     *
     * @param cntr counter to use
     * @param ifcn interface to use
     * @param pck packet to send
     * @return true on error false on success
     */
    public abstract boolean send2apiPack(int cntr, int ifcn, packHolder pck);

}

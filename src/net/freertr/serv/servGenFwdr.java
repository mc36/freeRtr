package net.freertr.serv;

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

    //////// try to move send through api here
}

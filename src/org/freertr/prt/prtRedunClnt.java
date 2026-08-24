package org.freertr.prt;

import java.util.List;
import org.freertr.util.cmds;

/**
 * redundancy client
 *
 * @author matecsaba
 */
public interface prtRedunClnt {

    /**
     * get state information
     *
     * @param lst list to append
     */
    public abstract void routerStateGet(List<String> lst);

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public abstract boolean routerStateSet(cmds cmd);

}

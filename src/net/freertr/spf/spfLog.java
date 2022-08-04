package net.freertr.spf;

import net.freertr.cfg.cfgAll;
import net.freertr.util.bits;

/**
 * spf log
 *
 * @author matecsaba
 */
public class spfLog {

    /**
     * create instance
     */
    public spfLog() {
    }

    /**
     * timestamp
     */
    protected long when;

    /**
     * elapsed time
     */
    protected int tim;

    /**
     * topology summary
     */
    protected int topo;

    /**
     * unreachables
     */
    protected String unreach;

    public String toString() {
        return bits.time2str(cfgAll.timeZoneName, when + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(when) + "|" + tim + "|" + bits.toHexD(topo) + "|" + unreach;
    }

}

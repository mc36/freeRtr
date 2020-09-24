package util;

import cfg.cfgAll;

/**
 * spf log
 *
 * @author matecsaba
 */
public class shrtPthFrstLog {

    protected long when;

    protected int tim;

    protected int topo;

    protected String unreach;

    public String toString() {
        return bits.time2str(cfgAll.timeZoneName, when + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(when) + "|" + tim + "|" + bits.toHexD(topo) + "|" + unreach;
    }

}

package net.freertr.serv;

import java.util.Comparator;
import net.freertr.cfg.cfgAll;
import net.freertr.util.bits;

/**
 * one p4lang api statistic
 *
 * @author matecsaba
 */
public class servP4langMsg implements Comparator<servP4langMsg> {

    private final String msg;

    /**
     * hit count
     */
    public int cnt;

    /**
     * last time
     */
    public long lst;

    /**
     * create instance
     *
     * @param m message
     */
    public servP4langMsg(String m) {
        msg = m;
    }

    public String toString() {
        return msg + "|" + cnt + "|" + bits.time2str(cfgAll.timeZoneName, lst + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(lst);
    }

    public int compare(servP4langMsg o1, servP4langMsg o2) {
        return o1.msg.compareTo(o2.msg);
    }

}

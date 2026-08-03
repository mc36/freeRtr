package org.freertr.rtr;

import java.util.List;
import org.freertr.util.cmds;

/**
 * bgp4 template
 *
 * @author matecsaba
 */
public class rtrBgpTemp extends rtrBgpParam implements Comparable<rtrBgpTemp> {

    /**
     * name of template
     */
    public final String tempName;

    /**
     * create template
     *
     * @param parent bgp process
     * @param nam name of template
     */
    public rtrBgpTemp(rtrBgp parent, String nam) {
        super(parent, true);
        tempName = "" + nam;
    }

    public String toString() {
        return "" + tempName;
    }

    public int compareTo(rtrBgpTemp o) {
        return tempName.compareTo(o.tempName);
    }

    /**
     * configure
     *
     * @param cmd command
     * @param negated negated
     */
    public void doTempCfg(String cmd, boolean negated) {
        lower.templateConfig(this, cmd, negated);
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void getConfig(List<String> l, String beg, int filter) {
        l.addAll(getParamCfg(beg, "template " + tempName + " ", filter));
        l.add(beg + cmds.comment);
    }

}

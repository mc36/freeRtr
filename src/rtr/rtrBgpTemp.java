package rtr;

import java.util.Comparator;
import java.util.List;

/**
 * bgp4 template
 *
 * @author matecsaba
 */
public class rtrBgpTemp extends rtrBgpParam implements Comparator<rtrBgpTemp> {

    /**
     * name of template
     */
    public String tempName;

    /**
     * create template
     *
     * @param parent bgp process
     */
    public rtrBgpTemp(rtrBgp parent) {
        super(parent, true);
        tempName = "";
    }

    public int compare(rtrBgpTemp o1, rtrBgpTemp o2) {
        return o1.tempName.compareTo(o2.tempName);
    }

    /**
     * flap connection
     */
    public void flapBgpConn() {
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
    public void getConfig(List<String> l, String beg, boolean filter) {
        l.addAll(getParamCfg(beg, "template " + tempName + " ", false));
    }

}

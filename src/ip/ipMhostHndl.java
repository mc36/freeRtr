package ip;

import addr.addrIP;

/**
 * multicast host (igmp/mld) handler
 *
 * @author matecsaba
 */
public interface ipMhostHndl {

    /**
     * process query message
     *
     * @param ifc receiving interface
     * @param grp group queryed, null if generic
     * @param src source, null if generic
     */
    public abstract void gotQuery(Object ifc, addrIP grp, addrIP src);

    /**
     * process report message
     *
     * @param ifc receiving interface
     * @param grp group reported, null if generic
     * @param src source, null if generic
     * @param need true if report, false if leaved
     */
    public abstract void gotReport(Object ifc, addrIP grp, addrIP src, boolean need);

}

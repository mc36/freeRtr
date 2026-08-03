package org.freertr.ip;

import org.freertr.addr.addrIP;

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
    public void mhostQuery(Object ifc, addrIP grp, addrIP src);

    /**
     * process report message
     *
     * @param ifc receiving interface
     * @param grp group reported, null if generic
     * @param src source, null if generic
     * @param need true if report, false if leaved
     */
    public void mhostReport(Object ifc, addrIP grp, addrIP src, boolean need);

}

package ip;

import addr.addrIP;
import pack.packHolder;
import util.counter;

/**
 * icmp core protocol
 *
 * @author matecsaba
 */
public interface ipIcmp {

    /**
     * get icmp protocol number
     *
     * @return protocol number
     */
    public int getProtoNum();

    /**
     * get icmp header size
     *
     * @return header size
     */
    public int getHeadSize();

    /**
     * set ip forwarder
     *
     * @param ifw forwarder to use
     */
    public void setForwarder(ipFwd ifw);

    /**
     * parse icmp header
     *
     * @param pck packet to parse
     * @return false if successful, true if error happened
     */
    public boolean parseICMPheader(packHolder pck);

    /**
     * create icmp header the reserved dword should be filled in before calling
     *
     * @param pck packet to update
     */
    public void createICMPheader(packHolder pck);

    /**
     * update icmp header checksum
     *
     * @param pck packet to update
     */
    public void updateICMPheader(packHolder pck);

    /**
     * create error reporting code
     *
     * @param pck packet to update
     * @param reason reason code
     * @param ifip sending interface ip
     * @return false if successful, true on error
     */
    public boolean createError(packHolder pck, counter.reasons reason, addrIP ifip);

    /**
     * create echo request
     *
     * @param pck packet to update
     * @param src source ip
     * @param trg target ip
     * @param id sending id
     * @return false if successful, true on error
     */
    public boolean createEcho(packHolder pck, addrIP src, addrIP trg, int id);

    /**
     * convert icmp type code to string
     *
     * @param i type code to convert
     * @return string showing the code
     */
    public String icmp2string(int i);

}

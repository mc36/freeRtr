package org.freertr.enc;

import java.util.List;

/**
 * calls handler
 *
 * @author matecsaba
 */
public interface encCallHnd {

    /**
     * get number of in calls
     *
     * @param dir direction, true=in, false=out
     * @return number of calls
     */
    public int numCalls(boolean dir);

    /**
     * get number of out messages
     *
     * @param dir direction, true=in, false=out
     * @return number of messages
     */
    public int numMsgs(boolean dir);

    /**
     * get call list
     *
     * @param dir direction, true=in, false=out
     * @return list
     */
    public List<String> listCalls(boolean dir);

    /**
     * check if ready
     *
     * @return true if yes, false if no
     */
    public boolean isReady();

    /**
     * send message
     *
     * @param calling calling number
     * @param called called number
     * @param msg message
     * @return false on success, true on error
     */
    public boolean sendMsg(String calling, String called, List<String> msg);

    /**
     * make the call
     *
     * @param calling calling number
     * @param called called number
     * @return call id, null if error
     */
    public String makeCall(String calling, String called);

    /**
     * stop the call
     *
     * @param cid call id
     */
    public void stopCall(String cid);

    /**
     * get call
     *
     * @param cid call id
     * @return rtp
     */
    public encCallOne getCall(String cid);

    /**
     * stop work
     */
    public void stopWork();

}

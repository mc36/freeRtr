
/**
 * log entry
 *
 * @author matecsaba
 */
public class temperLog {

    /**
     * parent
     */
    public final temper lower;

    /**
     * time
     */
    public final long time;

    /**
     * peer
     */
    public final String peer;

    /**
     * code
     */
    public final int code;

    /**
     * create instance
     *
     * @param prnt parent
     * @param per peer
     * @param cod code
     */
    public temperLog(temper prnt, String per, int cod) {
        lower = prnt;
        time = temperUtil.getTime();
        peer = per;
        code = cod;
    }

    /**
     * get web line
     *
     * @param num my number
     * @param tim current time
     * @return string
     */
    protected String getMeas(int num, long tim) {
        num += lower.doorCnt;
        num -= lower.doorLog.size();
        num++;
        return "<tr><td>" + num + "</td><td>" + temperUtil.time2str(lower.tzdata, time) + "</td><td>" + temperUtil.timePast(tim, time) + "</td><td>" + code + "</td><td>" + peer + "</td></tr>";
    }

}


/**
 * door code
 *
 * @author matecsaba
 */
public class temperCode {

    /**
     * my number
     */
    protected final int myNum;

    /**
     * code
     */
    protected final String code;

    /**
     * temporary
     */
    protected final boolean temp;

    /**
     * create instance
     *
     * @param c code
     * @param t temporary
     */
    public temperCode(int n, String c, boolean t) {
        myNum = n;
        code = c;
        temp = t;
    }

}

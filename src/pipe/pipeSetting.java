package pipe;

/**
 * one setting of a pipeline
 *
 * @author matecsaba
 */
public class pipeSetting {

    /**
     * user origin
     */
    public final static int userFrom = 1;

    /**
     * user name
     */
    public final static int userName = 2;

    /**
     * terminal length
     */
    public final static int termLen = 3;

    /**
     * terminal width
     */
    public final static int termWid = 4;

    /**
     * name of the setting
     */
    protected final int name;

    /**
     * value of the object
     */
    protected Object value;

    /**
     * create instance
     *
     * @param nam name
     */
    public pipeSetting(int nam) {
        name = nam;
    }

}

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
     * terminal height (y)
     */
    public final static int termHei = 3;

    /**
     * terminal width (x)
     */
    public final static int termWid = 4;

    /**
     * timestamps
     */
    public final static int times = 5;

    /**
     * colorize
     */
    public final static int colors = 6;

    /**
     * spacetab
     */
    public final static int spacTab = 7;

    /**
     * logging
     */
    public final static int logging = 8;

    /**
     * table mode
     */
    public final static int tabMod = 9;

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

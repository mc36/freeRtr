package pipe;

/**
 * one setting of a pipeline
 *
 * @author matecsaba
 */
public class pipeSetting {

    /**
     * name of the setting
     */
    public final String name;

    /**
     * value of the object
     */
    public Object value;

    /**
     * create instance
     *
     * @param nam name
     */
    public pipeSetting(String nam) {
        name = nam;
    }

}

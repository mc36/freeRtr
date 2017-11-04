package util;

/**
 * xml entry
 *
 * @author matecsaba
 */
public class extMrkLngEntry {

    /**
     * name
     */
    public String name;

    /**
     * parameter
     */
    public String param;

    /**
     * value
     */
    public String value;

    /**
     * create instance
     *
     * @param n name
     * @param p parameter
     * @param v value
     */
    public extMrkLngEntry(String n, String p, String v) {
        name = n;
        param = p;
        value = v;
    }

    /**
     * create instance
     */
    public extMrkLngEntry() {
        clear();
    }

    /**
     * copy instance
     *
     * @return instance
     */
    public extMrkLngEntry copyBytes() {
        extMrkLngEntry ntry = new extMrkLngEntry();
        ntry.name = "" + name;
        ntry.param = "" + param;
        ntry.value = "" + value;
        return ntry;
    }

    /**
     * clear everything
     */
    public void clear() {
        name = "";
        param = "";
        value = "";
    }

    public String toString() {
        return name + "<" + param + ">" + value;
    }

    /**
     * get tag name
     *
     * @return name
     */
    public String getTag() {
        int i = name.lastIndexOf("/");
        if (i < 1) {
            return name;
        }
        return name.substring(i + 1, name.length());
    }

}

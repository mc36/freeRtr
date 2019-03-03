package util;

/**
 * json entry
 *
 * @author matecsaba
 */
public class jasOnEntry {

    /**
     * level
     */
    public int level;

    /**
     * value
     */
    public String value;

    /**
     * create instance
     *
     * @param n level
     * @param v value
     */
    public jasOnEntry(int n, String v) {
        level = n;
        value = v;
    }

    /**
     * create instance
     */
    public jasOnEntry() {
        clear();
    }

    /**
     * copy instance
     *
     * @return instance
     */
    public jasOnEntry copyBytes() {
        jasOnEntry ntry = new jasOnEntry();
        ntry.level = level;
        ntry.value = "" + value;
        return ntry;
    }

    /**
     * clear everything
     */
    public void clear() {
        level = 0;
        value = "";
    }

    public String toString() {
        return level + "=\"" + value + "\"";
    }

}

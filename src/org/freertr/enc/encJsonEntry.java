package org.freertr.enc;

/**
 * json entry
 *
 * @author matecsaba
 */
public class encJsonEntry {

    /**
     * level
     */
    public int level;

    /**
     * value
     */
    public String data;

    /**
     * create instance
     *
     * @param n level
     * @param v value
     */
    public encJsonEntry(int n, String v) {
        level = n;
        data = v;
    }

    /**
     * create instance
     */
    public encJsonEntry() {
        clear();
    }

    /**
     * copy instance
     *
     * @return instance
     */
    public encJsonEntry copyBytes() {
        encJsonEntry ntry = new encJsonEntry();
        ntry.level = level;
        ntry.data = "" + data;
        return ntry;
    }

    /**
     * clear everything
     */
    public void clear() {
        level = 0;
        data = "";
    }

    public String toString() {
        return level + "=\"" + data + "\"";
    }

}

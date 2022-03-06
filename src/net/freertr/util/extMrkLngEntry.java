package net.freertr.util;

/**
 * xml entry
 *
 * @author matecsaba
 */
public class extMrkLngEntry {

    /**
     * parent
     */
    public extMrkLngEntry parent;

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
     * @param pr parent
     * @param n name
     * @param p parameter
     * @param v value
     */
    public extMrkLngEntry(extMrkLngEntry pr, String n, String p, String v) {
        parent = pr;
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
        ntry.parent = parent;
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

    /**
     * get name equals value path
     *
     * @return string
     */
    public String getNamVal() {
        return name + "=" + value;
    }

    /**
     * get unescaped path
     *
     * @return path
     */
    public String getUnesc() {
        String s = "";
        extMrkLngEntry cur = this;
        for (; cur != null;) {
            String a = cur.getTag();
            if (a.startsWith(extMrkLng.value)) {
                a = cur.value;
            }
            cur = cur.parent;
            if (a.startsWith(extMrkLng.ignore)) {
                continue;
            }
            if (a.startsWith(extMrkLng.escape)) {
                a = a.substring(extMrkLng.escape.length(), a.length());
            }
            if (a.length() < 1) {
                continue;
            }
            if (s.length() < 1) {
                s = a;
                continue;
            }
            s = a + "/" + s;
        }
        return s;
    }

}

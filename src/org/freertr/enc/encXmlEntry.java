package org.freertr.enc;

/**
 * xml entry
 *
 * @author matecsaba
 */
public class encXmlEntry {

    /**
     * parent
     */
    public encXmlEntry parent;

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
    public String data;

    /**
     * create instance
     *
     * @param pr parent
     * @param n name
     * @param p parameter
     * @param v value
     */
    public encXmlEntry(encXmlEntry pr, String n, String p, String v) {
        parent = pr;
        name = n;
        param = p;
        data = v;
    }

    /**
     * create instance
     */
    public encXmlEntry() {
        clear();
    }

    /**
     * copy instance
     *
     * @return instance
     */
    public encXmlEntry copyBytes() {
        encXmlEntry ntry = new encXmlEntry();
        ntry.parent = parent;
        ntry.name = "" + name;
        ntry.param = "" + param;
        ntry.data = "" + data;
        return ntry;
    }

    /**
     * clear everything
     */
    public void clear() {
        name = "";
        param = "";
        data = "";
    }

    public String toString() {
        return name + "<" + param + ">" + data;
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
        return name + "=" + data;
    }

    /**
     * get unescaped path
     *
     * @return path
     */
    public String getUnesc() {
        String s = "";
        encXmlEntry cur = this;
        for (; cur != null;) {
            String a = cur.getTag();
            if (a.startsWith(encXml.content)) {
                a = cur.data;
            }
            cur = cur.parent;
            if (a.startsWith(encXml.ignore)) {
                continue;
            }
            if (a.startsWith(encXml.escape)) {
                a = a.substring(encXml.escape.length(), a.length());
                a = encXml.decodeQuoted(a);
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

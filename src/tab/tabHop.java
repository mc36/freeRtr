package tab;

import addr.addrIP;

/**
 * one explicit route hop
 *
 * @author matecsaba
 */
public class tabHop {

    /**
     * address of hop
     */
    public addrIP adr = new addrIP();

    /**
     * true=strict, false=loose
     */
    public boolean strict = true;

    public String toString() {
        String a;
        if (strict) {
            a = "S";
        } else {
            a = "L";
        }
        return a + "/" + adr;
    }

}

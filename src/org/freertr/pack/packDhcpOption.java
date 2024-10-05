package org.freertr.pack;

import java.util.ArrayList;
import java.util.List;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * dhcp option
 *
 * @author matecsaba
 */
public class packDhcpOption implements Comparable<packDhcpOption> {

    /**
     * create instance
     */
    public packDhcpOption() {
    }

    /**
     * option number
     */
    public int number;

    /**
     * option data
     */
    public byte[] buffer;

    public int compareTo(packDhcpOption o) {
        if (number < o.number) {
            return -1;
        }
        if (number > o.number) {
            return +1;
        }
        return 0;
    }

    /**
     * parse from string
     *
     * @param cmd string to read
     */
    public void fromString(cmds cmd) {
        number = bits.str2num(cmd.word());
        List<Integer> lst = new ArrayList<Integer>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            lst.add(bits.fromHex(a));
        }
        buffer = new byte[lst.size()];
        for (int i = 0; i < buffer.length; i++) {
            buffer[i] = (byte) (lst.get(i) & 0xff);
        }
    }

    public String toString() {
        String s = "";
        for (int i = 0; i < buffer.length; i++) {
            s += " " + bits.toHexB(buffer[i]);
        }
        return number + s;
    }

}

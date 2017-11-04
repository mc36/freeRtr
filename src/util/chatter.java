package util;

import java.util.ArrayList;
import java.util.List;

/**
 * chatter class
 *
 * @author matecsaba
 */
public class chatter {

    private final List<chatterLine> lines = new ArrayList<chatterLine>();

    private long pos = 0;

    /**
     * send one line
     *
     * @param usr username
     * @param msg message
     */
    public synchronized void send(String usr, String msg) {
        lines.add(new chatterLine(usr, msg));
        if (lines.size() > 64) {
            lines.remove(0);
        }
        pos++;
    }

    /**
     * read lines
     *
     * @param l line buffer
     * @param p old position
     * @param s max lines
     * @return new position
     */
    public synchronized long read(List<String> l, long p, int s) {
        if (p >= pos) {
            return pos;
        }
        long m = pos - lines.size();
        if (p < m) {
            l.add("..flood");
            p = m;
        }
        p = pos - p;
        if (p > s) {
            l.add("..small");
            p = s;
        }
        for (int i = (int) (lines.size() - p); i < lines.size(); i++) {
            l.add("" + lines.get(i));
        }
        return pos;
    }

}

class chatterLine {

    public long time;

    public String user;

    public String line;

    public chatterLine(String usr, String ln) {
        user = usr;
        line = ln;
        time = bits.getTime();
    }

    public String toString() {
        return user + ": " + line;
    }

}

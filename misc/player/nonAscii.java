
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * non ascii
 *
 * @author matecsaba
 */
public class nonAscii {

    /**
     * check if a string is usascii7bit or not
     *
     * @param a string to check
     * @return false if safe
     */
    public final static int doNonAscii(String a) {
        if (a == null) {
            return -2;
        }
        byte[] b = a.getBytes();
        for (int i = 0; i < b.length; i++) {
            int c = b[i];
            if (c < 0x20) {
                return i;
            }
            if (c >= 0x7e) {
                return i;
            }
        }
        return -1;
    }

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        String s = "./";
        if (args.length > 0) {
            s = args[0];
        }
        List<File> fl = findSongs.doFindDir(s);
        int err = 0;
        for (int i = 0; i < fl.size(); i++) {
            File ntry = fl.get(i);
            String a = ntry.getName();
            int o = doNonAscii(a);
            if (o < 0) {
                continue;
            }
            err++;
            playerUtil.put(a);
            playerUtil.put(" ".repeat(o) + "^");
        }
        if (err < 1) {
            return;
        }
        playerUtil.put("found " + err + " bad of " + fl.size());
    }

}

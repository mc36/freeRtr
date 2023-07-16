
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
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
    public static final boolean nonAscii(String a) {
        if (a == null) {
            return true;
        }
        byte[] b = a.getBytes();
        for (int i = 0; i < b.length; i++) {
            int c = b[i];
            if (c < 0x20) {
                return true;
            }
            if (c >= 0x7e) {
                return true;
            }
        }
        return false;
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
        playerUtil.put("scanning " + s);
        findSongs fs = new findSongs();
        findSongs na = new findSongs();
        fs.doFind(s);
        for (int i = 0; i < fs.lst.size(); i++) {
            playerSong ntry = fs.lst.get(i);
            String a = ntry.file;
            if (!nonAscii(a)) {
                continue;
            }
            na.lst.add(ntry);
        }
        for (int i = 0; i < fs.lst.size(); i++) {
            playerSong ntry = fs.lst.get(i);
            playerUtil.put("not ascii " + ntry.file);
        }
        playerUtil.put("found " + na.lst.size() + " non ascii");//////////////
    }

}

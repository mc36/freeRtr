
import java.io.File;
import java.io.RandomAccessFile;
import java.util.List;

/**
 * favorites collector
 *
 * @author matecsaba
 */
public class favorites {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        String path = null;
        String favf = args[0];
        if (args.length > 1) {
            path = args[1];
        }
        if (path == null) {
            playerUtil.put("checking " + favf + "'s...");
        } else {
            playerUtil.put("copying " + favf + "'s to " + path + "...");
        }
        List<playerSong> need = playerSong.txt2pls(null, playerUtil.readup(favf));
        playerSong.sort(need);
        playerUtil.saveas(playerSong.pls2txt(need), favf);
        for (int o = 0; o < need.size(); o++) {
            playerSong ntry = need.get(o);
            if (!new File(ntry.file).exists()) {
                playerUtil.put("missing: " + ntry.file);
                continue;
            }
            int i = playerSong.find(need, ntry);
            if (i != o) {
                playerUtil.put("duplicate: " + ntry.file);
            }
            if (path == null) {
                continue;
            }
            i = ntry.file.lastIndexOf(".");
            copyFile(ntry.file, path + ntry.title + ntry.file.substring(i, ntry.file.length()));
        }
    }

    /**
     * copy one file
     *
     * @param src source
     * @param trg target
     * @return true on error, false on success
     */
    public static boolean copyFile(String src, String trg) {
        playerUtil.put(src + "-->" + trg + "...");
        RandomAccessFile fs;
        RandomAccessFile ft;
        try {
            new File(trg).createNewFile();
        } catch (Exception e) {
            return true;
        }
        long siz;
        try {
            fs = new RandomAccessFile(new File(src), "r");
            siz = fs.length();
            if (new File(trg).length() == siz) {
                fs.close();
                return false;
            }
            ft = new RandomAccessFile(new File(trg), "rw");
            ft.setLength(0);
        } catch (Exception e) {
            return true;
        }
        long pos = 0;
        for (; pos < siz;) {
            final int max = 8 * 1024;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            pos += rndl;
            int rndi = (int) rndl;
            byte buf[] = new byte[rndi];
            try {
                fs.read(buf, 0, rndi);
            } catch (Exception e) {
                return true;
            }
            try {
                ft.write(buf, 0, rndi);
            } catch (Exception ex) {
                return true;
            }
        }
        try {
            fs.close();
        } catch (Exception e) {
        }
        try {
            ft.close();
        } catch (Exception e) {
        }
        return false;
    }

}

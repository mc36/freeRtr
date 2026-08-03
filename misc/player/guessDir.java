
/**
 * guess directory
 *
 * @author matecsaba
 */
public class guessDir {

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
        fs.doFindSongs(s);
        fs.doSort();
        fs.doDir();
        String a = fs.art + " - " + fs.dat + " - " + fs.alb;
        playerUtil.put(a);
        a = renamer.unidecode(a);
        playerUtil.put(a);
        a = renamer.guessName(a, false);
        playerUtil.put(a);
    }

}

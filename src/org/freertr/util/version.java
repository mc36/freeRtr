package org.freertr.util;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.pipe.pipeShell;
import org.freertr.serv.servOpenflow;
import org.freertr.serv.servP4lang;
import org.freertr.serv.servStack;
import org.freertr.user.userHelping;

/**
 * version utils
 *
 * @author matecsaba
 */
public class version {

    private version() {
    }

    /**
     * 9.1.1
     */
    public final static String verNum = verCore.year + "." + verCore.month + "." + verCore.day;

    /**
     * v9.1.1-rel
     */
    public final static String VerNam = "v" + verNum + verCore.state;

    /**
     * ros v9.1.1-rel
     */
    public final static String namVer = verCore.name + " " + VerNam;

    /**
     * ros/9.1.1-rel
     */
    public final static String usrAgnt = verCore.name + "/" + verNum + verCore.state;

    /**
     * ros v9.1.1-rel, done by me.
     */
    public final static String headLine = namVer + ", done by " + verCore.author + ".";

    /**
     * mimetype text
     */
    public final static String[] mimetypes = {
        // text
        "html   text/html",
        "htm    text/html",
        "css    text/css",
        "rtf    text/richtext",
        "text   text/plain",
        "txt    text/plain",
        "csv    text/csv",
        "md     text/markdown",
        "*      text/plain",
        // image
        "webp   image/webp",
        "gif    image/gif",
        "jpeg   image/jpeg",
        "jpg    image/jpeg",
        "tiff   image/tiff",
        "tif    image/tiff",
        "bmp    image/bmp",
        "png    image/png",
        "svg    image/svg+xml",
        "ico    image/x-icon",
        "pbm    image/x-portable-bitmap",
        "pgm    image/x-portable-graymap",
        "pnm    image/x-portable-anymap",
        "ppm    image/x-portable-pixmap",
        "xbm    image/x-xbitmap",
        "xpm    image/x-xpixmap",
        // video
        "webm   video/webm",
        "mjpeg  video/x-motion-jpeg",
        "avi    video/msvideo",
        "mov    video/quicktime",
        "qt     video/quicktime",
        "mpeg   video/mpeg",
        "mpg    video/mpeg",
        "mp4    video/mp4",
        "mkv    video/x-matroska",
        "3gp    video/3gpp",
        "3g2    video/3gpp2",
        "ogv    video/ogg",
        // audio
        "weba   audio/weba",
        "aif    audio/x-aiff",
        "aiff   audio/x-aiff",
        "wav    audio/wav",
        "midi   audio/midi",
        "mid    audio/midi",
        "rmi    audio/midi",
        "ram    audio/x-pn-realaudio",
        "rpm    audio/x-pn-realaudio-plugin",
        "ra     audio/x-realaudio",
        "rm     audio/x-pn-realaudio",
        "mp3    audio/mpeg",
        "oga    audio/ogg",
        "flac   audio/flac",
        "aac    audio/aac",
        // application
        "bin    application/octet-stream",
        "jar    application/java-archive",
        "doc    application/msword",
        "docx   application/msword",
        "dvi    application/x-dvi",
        "eps    application/postscript",
        "ps     application/postscript",
        "gz     application/x-gzip",
        "bz2    application/x-bzip2",
        "js     application/javascript",
        "latex  application/x-latex",
        "lzh    application/x-lzh",
        "pdf    application/pdf",
        "epub   application/epub+zip",
        "swf    application/x-shockwave-flash",
        "tar    application/tar",
        "tcl    application/x-tcl",
        "tex    application/x-tex",
        "tgz    application/x-gzip",
        "zip    application/zip",
        "xml    application/xml",
        "ogg    application/ogg",
        // wireless application
        "wml    text/vnd.wap.wml",
        "wbmp   image/vnd.wap.wbmp"
    };

    /**
     * get show logo text
     *
     * @param head needed extra lines
     * @return list
     */
    public static List<String> shLogo(int head) {
        List<String> sa = new ArrayList<String>();
        if ((head & 0x01) != 0) {
            sa.add("");
        }
        if ((head & 0x02) != 0) {
            sa.add(headLine);
        }
        if ((head & 0x04) != 0) {
            sa.add("");
        }
        if ((head & 0x08) != 0) {
            array2list(sa, verCore.logo);
        }
        if ((head & 0x10) != 0) {
            sa.add("");
        }
        if ((head & 0x20) != 0) {
            sa.add(headLine);
        }
        if ((head & 0x40) != 0) {
            sa.add("");
        }
        if ((head & 0x80) != 0) {
            array2list(sa, verCore.license);
        }
        if ((head & 0x100) != 0) {
            sa.add("");
        }
        if ((head & 0x200) != 0) {
            sa.add(verNum);
        }
        if ((head & 0x400) != 0) {
            sa.add(bits.time2str(cfgAll.timeZoneName, getFileDate(), 3));
        }
        if ((head & 0x800) != 0) {
            sa.add(bits.time2str(cfgAll.timeZoneName, getFileDate(), 4));
        }
        if ((head & 0x1000) != 0) {
            sa.add(usrAgnt);
        }
        if ((head & 0x2000) != 0) {
            sa.add(verCore.homeUrl);
        }
        if ((head & 0x4000) != 0) {
            array2list(sa, verCore.quotes);
        }
        return sa;
    }

    /**
     * put array to list
     *
     * @param trg target
     * @param src source
     */
    public final static void array2list(List<String> trg, String[] src) {
        for (int i = 0; i < src.length; i++) {
            trg.add(src[i]);
        }
    }

    /**
     * generate help list
     *
     * @param hl help to update
     * @param beg beginning
     */
    public static void genSecHelp(userHelping hl, int beg) {
        List<String> l = getSecList();
        for (int i = 0; i < l.size(); i++) {
            String a = l.get(i);
            hl.add(null, beg + " .  " + a + "   sh0w m30www s0m30www " + a);
        }
    }

    /**
     * find in secret list
     *
     * @param a string to find
     * @return found, -1 if nothing
     */
    public static int findSecret(String a) {
        List<String> lst = getSecList();
        for (int i = 0; i < lst.size(); i++) {
            String s = lst.get(i);
            if (s.equals(a)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * get show secret text
     *
     * @param typ type of secret
     * @return list
     */
    public static List<String> shSecret(int typ) {
        ArrayList<String> l = new ArrayList<String>();
        switch (typ) {
            case 0:
                l.add("");
                l.add("   /~~!~\\");
                l.add("  |      |_______");
                l.add("  | |<3Y |       |");
                l.add("   \\____/        |");
                l.add("");
                break;
            case 1:
                l.add("");
                l.add("   /~~\\   /~~\\");
                l.add("  |    \\_/    |");
                l.add("   \\         /");
                l.add("    \\  L0v3 /");
                l.add("     \\     /");
                l.add("      \\   /");
                l.add("       \\ /");
                l.add("        V");
                l.add("");
                break;
            case 2:
                l.add("");
                l.add("                 \\   /");
                l.add("                 .\\-/.");
                l.add("             /\\  @o o@  /\\");
                l.add("            /  \\ /~-~\\ /  \\");
                l.add("                y  Y  V");
                l.add("          ,-^-./   |   \\,-^-.");
                l.add("         /    {   BuG   }    \\");
                l.add("               \\   |   /");
                l.add("               /\\  A  /\\");
                l.add("              /  \\/ \\/  \\");
                l.add("             /           \\");
                l.add("");
                break;
            case 3:
                l.add("                   \\ /");
                l.add("                 -->*<--");
                l.add("                   /o\\");
                l.add("                  /_\\_\\");
                l.add("                 /_/_0_\\");
                l.add("                /_o_\\_\\_\\");
                l.add("               /_/_/_/_/o\\");
                l.add("              /@\\_\\_\\@\\_\\_\\");
                l.add("             /_/_/O/_/_/_/_\\");
                l.add("            /_\\_\\_\\_\\_\\o\\_\\_\\");
                l.add("           /_/0/_/_/_0_/_/@/_\\");
                l.add("          /_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\");
                l.add("         /_/o/_/_/@/_/_/o/_/0/_\\");
                l.add("            [___]");
                l.add("b3ar pspsps sh0w m0r3 r3sp3ct y0urs3lv3s");
                break;
            case 4:
                l.add("      /OO\\");
                l.add("      /||\\");
                l.add("       ||");
                l.add("       ||");
                l.add("       ||");
                l.add("       ||");
                l.add("       ||");
                l.add("       ||");
                l.add("      0||@");
                break;
            case 5:
                l.add("    _      _      _");
                l.add("  >(0)__ <(0)__ =(0)__");
                l.add("   (___/  (___/  (___/");
                break;
            case 6:
                l.add("   |~~~~~~~~|");
                l.add("   | |_@@_| |");
                l.add("   |   __   |");
                l.add("   |  (00)  |");
                l.add("   |        |");
                l.add("   |________|");
                break;
            case 7:
                l.add("                         c@t5eTAN|<");
                l.add("                                                     _..----.._  ");
                l.add("                                                    ]_.--._____[  ");
                l.add("                                                  ___|'--'__..|--._   ");
                l.add("                              __               \"\"\"    ;            :  ");
                l.add("                            ()_ \"\"\"\"---...__.'\"\"!\":  /    ___       :  ");
                l.add("                               \"\"\"---...__\\]..__] | /    [ 0 ]      :  ");
                l.add("                                          \"\"\"!--./ /      \"\"\"        :  ");
                l.add("                                   __  ...._____;\"\"'.__________..--..:_  ");
                l.add("                                  /  !\"''''''!''''''''''|''''/' ' ' ' \\\"--..__  __..  ");
                l.add("                                 /  /.--.    |          |  .'          \\' ' '.\"\"--.{'.  ");
                l.add("             _...__            >=7 //.-.:    |          |.'             \\ ._.__  ' '\"\"'. ");
                l.add("          .-' /    \"\"\"\"----..../ \"\">==7-.....:______    |                \\| |  \"\";.;-\"> \\  ");
                l.add("          \"\"\"\";           __..\"   .--\"/\"\"\"\"\"----....\"\"\"\"\"----.....H_______\\_!....'----\"\"\"\"]  ");
                l.add("        _..---|._ __..--\"\"       _!.-=_.            \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"                   ;\"\"\"  ");
                l.add("       /   .-\";-.'--...___     .\" .-\"\"; ';\"\"-\"\"-...^..__...-v.^___,  ,__v.__..--^\"--\"\"-v.^v,  ");
                l.add("      ;   ;   |'.         \"\"\"-/ ./;  ;   ;\\P.        ;   ;        \"\"\"\"____;  ;.--\"\"\"\"// '\"\"<,");
                l.add("      ;   ;   | 1            ;  ;  '.: .'  ;<   ___.-'._.'------\"\"\"\"\"\"____'..'.--\"\"\";;'  o ';  ");
                l.add("      '.   \\__:/__           ;  ;--\"\"()_   ;'  /___ .-\" ____---\"\"\"\"\"\"\" __.._ __._   '>.,  ,/;  ");
                l.add("        \\   \\    /\"\"\"<--...__;  '_.-'/; \"\"; ;.'.'  \"-..'    \"-.      /\"/    `__. '.   \"---\"; ");
                l.add("         '.  'v ; ;     ;;    \\  \\ .'  \\ ; // /    _.-\" \"-._   ;    : ;   .-'__ '. ;   .^\".'      ");
                l.add("           '.  '; '.   .'/     '. `-.__.' /;;;   .o__.---.__o. ;    : ;   '\"\";;\"\"' ;v^\" .^ ");
                l.add("             '-. '-.___.'<__v.^,v'.  '-.-' ;|:   '    :      ` ;v^v^'.'.    .;'.__/_..-'   ");
                l.add("                '-...__.___...---\"\"'-.   '-'.;\\     'WW\\     .'_____..>.\"^\"-\"\"\"\"\"\"\"\" ");
                l.add("                                      '--..__ '\"._..'  '\"-;;\"\"\" ");
                l.add("                                             \"\"\"---'\"\"\"\"\"\"  ");
                l.add("");
                break;
            case 8:
                l.add("      .____.");
                l.add("   xuu$``$$$uuu.");
                l.add(" . $``$  $$$`$$$");
                l.add("dP*$  $  $$$ $$$");
                l.add("?k $  $  $$$ $$$");
                l.add(" $ $  $  $$$ $$$");
                l.add(" \":$  $  $$$ $$$");
                l.add("  N$  $  $$$ $$$");
                l.add("  $$  $  $$$ $$$");
                l.add("   $  $  $$$ $$$");
                l.add("   $  $  $$$ $$$");
                l.add("   $  $  $$$ $$$");
                l.add("   $  $  $$$ $$$");
                l.add("   $  $  $$$ $$$");
                l.add("   $$#$  $$$ $$$");
                l.add("   $$'$  $$$ $$$");
                l.add("   $$`R  $$$ $$$");
                l.add("   $$$&  $$$ $$$");
                l.add("   $#*$  $$$ $$$");
                l.add("   $  $  $$$ @$$");
                l.add("   $  $  $$$ $$$");
                l.add("   $  $  $$$ $$$");
                l.add("   $  $  $B$ $$&.");
                l.add("   $  $  $D$ $$$$$muL.");
                l.add("   $  $  $Q$ $$$$$  `\"**mu..");
                l.add("   $  $  $R$ $$$$$    k  `$$*t");
                l.add("   $  @  $$$ $$$$$    k   $$!4");
                l.add("   $ x$uu@B8u$NB@$uuuu6...$$X?");
                l.add("   $ $(`RF`$`````R$ $$5`\"\"\"#\"R");
                l.add("   $ $\" M$ $     $$ $$$      ?");
                l.add("   $ $  ?$ $     T$ $$$      $");
                l.add("   $ $F H$ $     M$ $$K      $  ..");
                l.add("   $ $L $$ $     $$ $$R.     \"d$$$$Ns.");
                l.add("   $ $~ $$ $     N$ $$X      .\"    \"02h");
                l.add("   $ 4k f  $     *$ $$&      R       \"iN");
                l.add("   $ $$ %uz!     tuuR$$:     Buu      ?`:");
                l.add("   $ $F          $??$8B      | '*Ned*$~L$");
                l.add("   $ $k          $'@$$$      |$.suu+!' !$");
                l.add("   $ ?N          $'$$@$      $*`      d:\"");
                l.add("   $ dL..........M.$&$$      5       d\"P");
                l.add(" ..$.^\"*I$RR*$C\"\"??77*?      \"nu...n*L*");
                l.add("'$C\"R   ``\"\"!$*@#\"\"` .uor    bu8BUU+!`");
                l.add("'*@m@.       *d\"     *$Rouxxd\"```$");
                l.add("     R*@mu.           \"#$R *$    !");
                l.add("     *%x. \"*L               $     %.");
                l.add("        \"N  `%.      ...u.d!` ..ue$$$o..");
                l.add("         @    \".    $*\"\"\"\" .u$$$$$$$$$$$$beu...");
                l.add("        8  .mL %  :R`     x$$$$$$$$$$$$$$$$$$$$$$$$$$WmeemeeWc");
                l.add("       |$e!\" \"s:k 4      d$N\"`\"#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$>");
                l.add("       $$      \"N @      $?$    F$$$$$$$$$$$$$$$$$$$$$$$$$$$$>");
                l.add("       $@       ^%Uu..   R#8buu$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$>");
                l.add("                  ```\"\"*u$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$>");
                l.add("                         #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$>");
                l.add("                          \"5$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$>");
                l.add("                            `*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$>");
                l.add("                              ^#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$>");
                l.add("                                 \"*$$$$$$$$$$$$$$$$$$$$$$$$$$>");
                l.add("                                   `\"*$$$$$$$$$$$$$$$$$$$$$$$>");
                l.add("                                       ^!$$$$$$$$$$$$$$$$$$$$>");
                l.add("                                           `\"#+$$$$$$$$$$$$$$>");
                l.add("                                                 \"\"**$$$$$$$$>");
                l.add("                                                        ```\"\"");
                break;
            case 9:
                l.add(" r1ffl3 ");
                l.add(" ,________________________________   ");
                l.add("|__________,----------._ [____]  \"\"-,__  __...-----===\"");
                l.add("        (_(||||@@@@||||)___________/   \"\"             |");
                l.add("           `----------' Kr0gg@@[ ))\"-,                |");
                l.add("                                \"\"    `,  _,--...___  |");
                l.add("                                        `/          \"\"\"");
                break;
            case 10:
                l.add(" kn1f3 ");
                l.add("       .---.");
                l.add("       |---|");
                l.add("       |---|");
                l.add("       |---|");
                l.add("   .---^ - ^---.");
                l.add("   :___________:");
                l.add("      | 0|//|");
                l.add("      |0 |//|");
                l.add("      |@@|//|");
                l.add("      |@@|//|");
                l.add("      |@@|//|");
                l.add("      |0|//|");
                l.add("      |0 |.-|");
                l.add("      |0-'@@|");
                l.add("       \\@@/");
                l.add("        \\@/");
                l.add("         V");
                break;
            case 11:
                l.add("     ... th3 n3w m30wcr0n var1ant ...");
                l.add("                                        XX ");
                l.add("     XX   XX    XX                    XXXXX ");
                l.add("     XX   XX   XXX                   XX   XX");
                l.add("     XX   XX  XXXX                   XX    X");
                l.add("     XX   XX    XX   XX XXX  XX  XX  XX     ");
                l.add("     XX   XX    XX    XXX XX XX  XX   XXXXX ");
                l.add("     XX   XX    XX    XX  XX XX  XX       XX");
                l.add("     XX   XX    XX    XX     XX  XX       XX");
                l.add("      XX XX     XX    XX     XX  XX  X    XX");
                l.add("       XXX      XX    XX     XX  XX  XX   XX");
                l.add("        X     XXXXXX XXXX     XXX XX  XXXXX ");
                l.add("                                        XX ");
                l.add("");
                break;
            case 12:
                l.add("         ... TAN< u N0T ...");
                l.add("            .....        ...       ");
                l.add("     ::::::::::::::_____::::::::.");
                l.add("    .::::::::::::::::::::::::::::::::::::::::::::::::::. ");
                l.add(":::::::::::::::::::::::::::::::::::::::::::::::::::::::...");
                l.add(" :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::");
                l.add(" :::::::::::::::::::::::::::::::::::::::::::::::::::::::::");
                l.add(" :::::::::::::@:::::::::::::::::::::::::");
                l.add(" ::::::::::::@:@::::::::::::::::::::::::::::::::::");
                l.add("::::::::::::::@:::::::::::::::::::::::::::");
                l.add(" :::::::::::::X:::::::::::::::::::::::::: ");
                l.add(" .:::::::::::::0XXX::::::::::::::::::::'");
                l.add("::::::::::::::::::::::::::::::::");
                l.add("`::::::::::::::::::::::::::::::::::::::::");
                l.add(" '::::::::::::::::::::   ^^^^");
                l.add("          :::::::::::::");
                l.add("          :::::::::::::");
                l.add("          :::::::::::::::");
                l.add("          :::::::::::::::::");
                l.add("          ::::::::::::::::::.");
                l.add("         :::::::::::::::'");
                l.add("        :::::::::::::'");
                l.add("       ::::::::::'");
                l.add("");
                break;
            case 13:
                l.add(" d0 n0t b3ar w1th a m30www!");
                l.add("                  _         _");
                l.add(" .-\"\"-.          ( )-\"```\"-( )          .-\"\"-.");
                l.add("/ O O  \\          /         \\          /  O O \\");
                l.add("|O .-.  \\        /   0 _ 0   \\        /  .-. O|");
                l.add("\\ (   )  '.    _|     (_)     |     .'  (   ) /");
                l.add(" '.`-'     '-./ |             |`\\.-'     '-'.'");
                l.add("   \\         |  \\   \\     /   /  |         /");
                l.add("    \\        \\   '.  '._.'  .'   /        /");
                l.add("     \\        '.   `'-----'`   .'        /");
                l.add("      \\   .'    '-._        .-'\\   '.   /");
                l.add("       |/`          `'''''')    )    `\\|");
                l.add("       /                  (    (      ,\\");
                l.add("      ;                    \\    '-..-'/ ;");
                l.add("      |                     '.       /  |");
                l.add("      |                       `'---'`   |");
                l.add("      ;                                 ;");
                l.add("       \\                               /");
                l.add("        `.                           .'");
                l.add("          '-._                   _.-'");
                l.add("          __/`\"  '  - - -  ' \"`` \\__");
                l.add("         /`            /^\\           `\\");
                l.add("         \\(          .'   '.         )/");
                l.add("          '.(__(__.-'       '.__)__).'");
                l.add("");
                break;
            case 14:
                l.add("                    @@    00    00 ");
                l.add("              0000  @@000000@@00@@XXOO@@ ");
                l.add("          @@  OO0000@@000000000000@@@@OO ");
                l.add("          OO0000@@OOXXOO  @@  XXOOOO@@000000  ");
                l.add("      OO    00@@00XX  00  @@  OOXX  @@@@@@00  ");
                l.add("      0000@@00    00  @@  00  00  00XX  000000");
                l.add("        0000XX00  00  00  @@  @@  00  00OO@@00@@  ");
                l.add("    @@OO@@@@  XX@@  @@XXOO@@OO@@00XX00OO  XX@@@@O ");
                l.add("    OO0000XX00XXXX@@OOOO00@@00OO0000OOXX0000XX00@@0");
                l.add("      @@00    0000XX00000000000000OO0000XX    0000 ");
                l.add("  00000000@@00OOXX@@00@@@@00@@@@00@@XXXX00@@@@OO0000@");
                l.add("    OO00XX    OO00@@@@00XX  XX00@@00@@OOXX      @@00XX0");
                l.add("    00@@@@@@@@@@@@@@@@@@      0000@@@@@@@@@@@@@@@@00OO00");
                l.add("XX@@00@@        OO00@@00      000000OOXX        @@@@@@");
                l.add("      00OOOO@@00XXOO@@00000000000000XXOO@@00OOXX00XX  ");
                l.add("    000000XX  OO@@OO@@00@@00@@@@00OO00OO    XX@@00@@XX ");
                l.add("  0000@@00  @@OO  @@OO0000@@00OOOO00XXOO00OO  00@@00@@ ");
                l.add("      OO00@@    @@XX@@XX00@@00OO@@  @@XX  00@@0000 ");
                l.add("    XX00@@@@  @@XXOOOOOOXX@@  00XX00  00XX0000@@00OO");
                l.add("    XXXX  0000XX  @@  @@  @@  @@  00XX  0000@@    OO");
                l.add("        OO@@000000XX  00  @@  00XX  @@00@@00@@OO ");
                l.add("        @@  OO0000@@OOOO  @@  XX0000000000  XX@@ ");
                l.add("            0000@@00@@00@@@@@@@@@@00@@@@@@");
                l.add("            00XX0000@@@@@@00@@@@00@@OO  @@");
                l.add("        XX  000000@@@@0000  0000  00OO  ");
                break;
            case 15:
                l.add("... g0d sav3 th3 qu33n --- http://hacknasa.mp.ls/ ...");
                l.add("                                    00@@@@@@@@@@@@~~~");
                l.add("        88888b.  8888b. .d8888 @@8008b.");
                l.add("        888  88b     88 88K@@@@ 00   88b ");
                l.add("        888  888.d888888 Y888800.d888888 ");
                l.add("        888@@888888  888    0088 88  888 ");
                l.add("...@@@@@888  888 Y888888 88888P' Y888888 ");
                l.add("                        00");
                l.add("                      00");
                l.add("                    00");
                l.add("                  00");
                l.add("                00");
                l.add("              00");
                l.add("            00");
                l.add("          00");
                l.add("        00");
                l.add("      00");
                l.add("    00");
                l.add("  00");
                l.add("00");
                l.add("~");
                break;
            case 16:
                l.add(" http://c3rn.ch http://sw1t.ch");
                l.add("");
                l.add(" XXXXXXXXXXX   OO");
                l.add("XX         XX  OO");
                l.add("XX     @@@@@@@@@");
                l.add("XX    OO   XX  OO");
                l.add("XX    OO   XX  OO");
                l.add("XX    OO   XX  OO");
                l.add(" XXXXXOOXXXX@  OO");
                l.add(" @    OO     @ OO");
                l.add("  @   OO      @OO");
                l.add("     @ @@@@@@@@@");
                l.add("    @           @");
                break;
            case 17:
                l.add("----|------------|-----------|----");
                l.add("    |        --/ - \\--       |");
                l.add("   -|---------|  @  |--------|-");
                l.add("              /\\ _ /\\");
                l.add("           []/       \\[]");
                break;
            case 18:
                l.add(" us3d c0tt@n");
                l.add("      _   _");
                l.add("     ( `O' )");
                l.add("      ) \0 (");
                l.add("      |`@'|");
                l.add("      }-  {");
                l.add("      }-- {");
                l.add("      } - {");
                l.add("      |   |");
                l.add("      |___|");
                l.add("     (_____)");
                l.add("");
                break;
            case 19:
                l.add("... mrsPssyC4t5e - !!!Umrs4Z1Z!!! ...");
                l.add("");
                l.add("     ,/|         _.--''^``-...___.._.,;@0o");
                l.add("     /@ \\'.     _-'          ,--,,,--@00~");
                l.add("    { \\    `_-''       '    /}");
                l.add("     `;;'            ;   ; ;");
                l.add(" ._.--''     ._,,, _..'  .;.'");
                l.add("  (,_....----'''     (,..--''");
                break;
            case 20:
                l.add("    ... mrsB00B$Ssz ...");
                l.add("... 1d3a bY w4nd3rSh13ld...");
                l.add("");
                l.add("   (@)(0)");
                break;
            case 21:
                l.add(" sat3ll1t3                  http://www.rfc-editor.org/rfc/rfc9450");
                l.add("@         @");
                l.add("@          @0@");
                l.add("@            @0@@@");
                l.add("@             @   @@@@");
                l.add("@               @     @@@@");
                l.add("@                @        @@@");
                l.add("@                  @         @@@@");
                l.add("@ sat3ll1t3-bas3d   @            @@@");
                l.add("@   communications   @              0000@");
                l.add("@      satC0M (@)     @                  0@");
                l.add("@                      @                    a1rCraft");
                l.add("@                       @                 @         @");
                l.add("@                        @              @             @");
                l.add("@                         @           @     a1r-a1r     @");
                l.add("@                          @        @     commun1cat10ns   @");
                l.add("@                           @     @         LDACS A/A (@)    @");
                l.add("@                           @   @                              @");
                l.add("@                            a1rCraft.o@0@0@0@0@0@0@0@0@0@o.a1rCraft");
                l.add("@                                 0           a1r-gr0und           0");
                l.add("@                                 0         commun1cat10ns         0");
                l.add("@                                 0           LDACS a/g (0)        0");
                l.add("@      commun1cat10ns 1n          0                                0");
                l.add("@    and ar0und a1rp0rts          0                                0");
                l.add("@         a3r0MACS (-)            0                                0");
                l.add("@                                 0                                0");
                l.add("@         a1rCraft-------------+  0                                0");
                l.add("@                              0  0                                0");
                l.add("@                              0  0                                0");
                l.add("@         gr0und n3tw0rk       0  0         gr0und n3tw0rk         0");
                l.add("satC@M <---------------------> a1rp0rt <----------------------> LDACS");
                l.add("ground                          gr0und                         gr0und");
                l.add("transc31v3r                   transc31v3r                 transc31v3r");
                break;
            case 22:
                l.add("");
                l.add("     )  (  )  (");
                l.add("    (^)(^)(^)(^)");
                l.add("    _i__i__i__i_");
                l.add("   (____________)");
                l.add("   |@@@@|>o<|000|");
                l.add("   (____________)");
                break;
            case 23:
                l.add("        _,--',   _._.--._____");
                l.add(" .--.--';_'-.', \";_      _.,-'");
                l.add(".'--'.  _.'    {`'-;_ .-.>.'");
                l.add("      '-:_      )  / `' '=.");
                l.add("        ) >     {_/,     /~)");
                l.add("        |/               `^ .'");
                break;
            case 24:
                l.add("        _____");
                l.add("      .-'.  ':'-.");
                l.add("    .''::: .:    '.");
                l.add("   /   :::::'      \\");
                l.add("  ;.    ':' `       ;");
                l.add("  |       '..       |");
                l.add("  ; '      ::::.    ;");
                l.add("   \\       '::::   /");
                l.add("   '.      :::  .'");
                l.add("      '-.___'_.-'");
                break;
            case 25:
                l.add("                  \\       \\      `      /         /");
                l.add("                                             '");
                l.add("            \\      `    \\     `     '    /     /   `   '");
                l.add("    \\                       .  *  *  *  *  *  .     /");
                l.add("         `     \\    `    *  *  *  *  *  *  *  *  *       /");
                l.add("   \\                  * * * * * * * * * * * * * * *  '");
                l.add("       \\     `     @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @   /");
                l.add("\\  `             * * * * *             * * * * * * * * *   '  /");
                l.add("           `   @ @ @ @ *                 '@@@@@@@@@@@@@@@");
                l.add("      \\      * * * * *                     * * * * * * * *  '");
                l.add("  `        @ @ @ @ @                        @@@@@@@@@@@@@@@    /");
                l.add("          * * * **                            * * * * * * * *");
                l.add("\\   `   @ @ @ @ @                             @@@@@@@@@@@@@@@  '");
                l.add("      * * * * *                                * * * * * * * * /");
                l.add(" `   @ @ @ @ @               good luck          @@@@@@@@@@@@@@@ .");
                l.add("    * * * * *                                    * * * * * * * *.");
                l.add("\\  @ @ @ @ @                                     @@@@@@@@@@@@@@@@");
                l.add("  * * * * *                                         * * * * * * *");
                l.add(" @ @ @ @ @                                          @@@@@@@@@@@@@@@@");
                break;
            case 26:
                l.add("               |))    |))");
                l.add(" .             |  )) /   ))");
                l.add(" \\\\   ^ ^      |    /      ))");
                l.add("  \\\\(((  )))   |   /        ))");
                l.add("   / *    )))  |  /        ))");
                l.add("  |o  _)   ))) | /       )))");
                l.add("   --' |     ))`/      )))");
                l.add("    ___|              )))");
                l.add("   / __\\             ))))`()))");
                l.add("  /\\@   /             `(())))");
                l.add("  \\/   /  /`_______/\\   \\  ))))");
                l.add("       | |          \\ \\  |  )))");
                l.add("       | |           | | |   )))");
                l.add("       |_@           |_|_@    ))");
                l.add("      /_/           /_/_/");
                break;
            default:
                l.add("");
                l.add("   ... s0rr|/ b\\/tt th1s 0n3 1s _n0t_ _y33t_ ass1gnm3nt3d and when u enc0unt3t3r3d a r3al bug ...");
                break;
        }
        return l;
    }

    /**
     * get secret list
     *
     * @return list of secrets
     */
    public static List<String> getSecList() {
        List<String> res = new ArrayList<String>();
        res.add("key");
        res.add("love");
        res.add("bug");
        res.add("xmastree");
        res.add("dick");
        res.add("duck");
        res.add("disk");
        res.add("tank");
        res.add("revolver");
        res.add("riffle");
        res.add("knife");
        res.add("virus");
        res.add("girl");
        res.add("bear");
        res.add("gear");
        res.add("nasa");
        res.add("cern");
        res.add("plane");
        res.add("cotton");
        res.add("cat");
        res.add("boobs");
        res.add("airport");
        res.add("cake");
        res.add("map");
        res.add("earth");
        res.add("rainbow");
        res.add("unicorn");
        return res;
    }

    /**
     * get show platform text
     *
     * @return list
     */
    public static List<String> shPlat() {
        List<String> sa = new ArrayList<String>();
        sa.add(headLine);
        sa.add("");
        Runtime rt = Runtime.getRuntime();
        sa.add("name: " + cfgAll.hostName);
        sa.add("prnt: " + cfgInit.prntNam);
        sa.add("hwid: " + cfgInit.hwIdNum);
        sa.add("hwsn: " + cfgInit.hwSnNum);
        sa.add("hwfw: " + getHWfwd1liner());
        sa.add("pid: " + pipeShell.myProcessNum());
        sa.add("uptime: since " + bits.time2str(cfgAll.timeZoneName, cfgInit.started + cfgAll.timeServerOffset, 3) + ", for " + bits.timePast(cfgInit.started));
        sa.add("reload: " + bits.lst2str(bits.txt2buf(myReloadFile()), " "));
        sa.add("rwpath: " + getRWpath());
        sa.add("hwcfg: " + cfgInit.cfgFileHw);
        sa.add("swcfg: " + cfgInit.cfgFileSw);
        sa.add("cpu: " + getCPUname());
        sa.add("mem: free=" + bits.toUser(rt.freeMemory()) + ", max=" + bits.toUser(rt.maxMemory()) + ", used=" + bits.toUser(rt.totalMemory()));
        sa.add("hostos: " + getKernelName());
        long l = pipeShell.getKernelUptime();
        sa.add("hostup: since " + bits.time2str(cfgAll.timeZoneName, l + cfgAll.timeServerOffset, 3) + ", for " + bits.timePast(l));
        sa.add("java: " + getJavaVer("java") + " @ " + getProp("java.home"));
        sa.add("jspec: " + getJavaVer("java.specification"));
        sa.add("vm: " + getVMname());
        sa.add("vmspec: " + getJavaVer("java.vm.specification"));
        sa.add("class: v" + getProp("java.class.version") + " @ " + getFileName());
        return sa;
    }

    /**
     * get hardware forwarder
     *
     * @return offload info
     */
    public static String getHWfwd1liner() {
        servStack stk = cfgAll.dmnStack.get(0);
        if (stk != null) {
            return stk.getShGenOneLiner();
        }
        servP4lang p4l = cfgAll.dmnP4lang.get(0);
        if (p4l != null) {
            return p4l.getShGenOneLiner();
        }
        servOpenflow ovs = cfgAll.dmnOpenflow.get(0);
        if (ovs != null) {
            return ovs.getShGenOneLiner();
        }
        return "swonly";
    }

    /**
     * get java executable
     *
     * @return path of jvms
     */
    public static String getJvmExec() {
        try {
            return ProcessHandle.current().info().command().get();
        } catch (Exception e) {
            return getProp("java.home") + "/bin/java";
        }
    }

    private static String getJavaVer(String s) {
        String vnd = getProp(s + ".vendor");
        String nam = getProp(s + ".name");
        String ver = getProp(s + ".version");
        if (nam != null) {
            nam = " (" + nam + ")";
        } else {
            nam = "";
        }
        return vnd + nam + " v" + ver;
    }

    private static String getProp(String s) {
        try {
            return System.getProperty(s);
        } catch (Exception e) {
            return "?";
        }
    }

    /**
     * get archive date
     *
     * @return date of jar
     */
    public static long getFileDate() {
        return new File(getFileName()).lastModified();
    }

    /**
     * get archive name
     *
     * @return pathname jar filename
     */
    public static String getFileName() {
        return getProp("java.class.path");
    }

    /**
     * get archive path name
     *
     * @return filename without extension
     */
    public static String myPathName() {
        String s = getFileName();
        int i = s.lastIndexOf(".");
        int o = s.lastIndexOf("/");
        if (o < 0) {
            o = 0;
        }
        if (i < o) {
            return "rtr";
        }
        return s.substring(0, i);
    }

    /**
     * get read-write path name
     *
     * @return path
     */
    public static String getRWpath() {
        String a = cfgInit.rwPath;
        if (a == null) {
            a = cfgInit.cfgFileSw;
        }
        if (a == null) {
            a = cfgInit.cfgFileHw;
        }
        if (a == null) {
            a = "./";
        }
        int i = a.lastIndexOf("/");
        if (i < 0) {
            a = "./";
        } else {
            a = a.substring(0, i + 1);
        }
        return a;
    }

    /**
     * get reload file name
     *
     * @return filename without path
     */
    public static String myReloadFile() {
        return getRWpath() + "reload.log";
    }

    /**
     * get errors file name
     *
     * @return filename without path
     */
    public static String myErrorFile() {
        return getRWpath() + "errors.log";
    }

    /**
     * get memory info
     *
     * @return memory
     */
    public static String getMemoryInfo() {
        Runtime rt = Runtime.getRuntime();
        return bits.toUser(rt.totalMemory()) + "/" + bits.toUser(rt.maxMemory());
    }

    /**
     * get kernel name
     *
     * @return name of kernel
     */
    public static String getKernelName() {
        return getProp("os.name").trim() + " v" + getProp("os.version").trim();
    }

    /**
     * get vm name
     *
     * @return name of vm
     */
    public static String getVMname() {
        return getJavaVer("java.vm").trim();
    }

    /**
     * get cpu name
     *
     * @return name of cpu
     */
    public static String getCPUname() {
        return (Runtime.getRuntime().availableProcessors() + "*" + getProp("os.arch")).trim();
    }

}

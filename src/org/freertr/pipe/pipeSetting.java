package org.freertr.pipe;

import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.user.userFormat;
import org.freertr.user.userScreen;

/**
 * one setting of a pipeline
 *
 * @author matecsaba
 */
public class pipeSetting implements Comparable<pipeSetting> {

    /**
     * origin address
     */
    public final static int origin = 1;

    /**
     * authentication result
     */
    public final static int authed = 2;

    /**
     * terminal height (y)
     */
    public final static int height = 3;

    /**
     * terminal width (x)
     */
    public final static int width = 4;

    /**
     * timestamps
     */
    public final static int times = 5;

    /**
     * colorize
     */
    public final static int colors = 6;

    /**
     * spacetab
     */
    public final static int spacTab = 7;

    /**
     * logging
     */
    public final static int logging = 8;

    /**
     * table mode
     */
    public final static int tabMod = 9;

    /**
     * deactivation character
     */
    public final static int deactive = 10;

    /**
     * escape character
     */
    public final static int escape = 11;

    /**
     * routing table lines
     */
    public final static int riblines = 12;

    /**
     * capslock
     */
    public final static int capsLock = 13;

    /**
     * bells
     */
    public final static int termBells = 14;

    /**
     * ansi mode
     */
    public final static int ansiMode = 15;

    /**
     * password stars
     */
    public final static int passStar = 16;

    /**
     * normal color
     */
    public final static int colNormal = 17;

    /**
     * prompt color
     */
    public final static int colPrompt = 18;

    /**
     * header color
     */
    public final static int colHeader = 19;

    /**
     * name of the setting
     */
    protected final int name;

    /**
     * value of the object
     */
    protected Object data;

    /**
     * create instance
     *
     * @param nam name
     */
    public pipeSetting(int nam) {
        name = nam;
    }

    public int compareTo(pipeSetting o) {
        if (name < o.name) {
            return -1;
        }
        if (name > o.name) {
            return +1;
        }
        return 0;
    }

    /**
     * get info
     *
     * @param pip pipe
     * @return settings
     */
    public static userFormat getInfo(pipeSide pip) {
        userFormat l = new userFormat("|", "category|value");
        l.add("buggy|" + cfgAll.buggy);
        l.add("invdc|" + cfgAll.invdc);
        l.add("timeout|" + pip.getTime());
        l.add("evalvdc|" + cfgAll.evalVdcPrivs());
        l.add("authenticate|" + pip.settingsGet(pipeSetting.authed, new authResult()));
        l.add("monitor|" + pip.settingsGet(pipeSetting.logging, false));
        l.add("colorize|" + pip.settingsGet(pipeSetting.colors, userFormat.colorMode.normal));
        l.add("background|" + userScreen.color2string(pip.settingsGet(pipeSetting.colNormal, userScreen.colWhite) >>> 16));
        l.add("foreground|" + userScreen.color2string(pip.settingsGet(pipeSetting.colNormal, userScreen.colWhite)));
        l.add("prompt|" + userScreen.color2string(pip.settingsGet(pipeSetting.colPrompt, userScreen.colBrGreen)));
        l.add("header|" + userScreen.color2string(pip.settingsGet(pipeSetting.colHeader, userScreen.colBrYellow)));
        l.add("spacetab|" + pip.settingsGet(pipeSetting.spacTab, false));
        l.add("capslock|" + pip.settingsGet(pipeSetting.capsLock, false));
        l.add("bells|" + pip.settingsGet(pipeSetting.termBells, false));
        l.add("ansimode|" + pip.settingsGet(pipeSetting.ansiMode, userScreen.ansiMode.normal));
        l.add("timestamps|" + pip.settingsGet(pipeSetting.times, false));
        l.add("passwdstar|" + pip.settingsGet(pipeSetting.passStar, false));
        l.add("deactivate|" + pip.settingsGet(pipeSetting.deactive, 65536));
        l.add("escape|" + pip.settingsGet(pipeSetting.escape, 65536));
        l.add("table|" + pip.settingsGet(pipeSetting.tabMod, userFormat.tableMode.normal));
        l.add("height|" + pip.settingsGet(pipeSetting.height, 25));
        l.add("width|" + pip.settingsGet(pipeSetting.width, 80));
        l.add("riblines|" + pip.settingsGet(pipeSetting.riblines, 8192));
        l.add("origin|" + pip.settingsGet(pipeSetting.origin, "?"));
        return l;
    }

}

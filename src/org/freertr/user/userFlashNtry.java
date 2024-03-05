package org.freertr.user;

import java.io.File;
import java.util.Comparator;

/**
 * represents one file
 *
 * @author matecsaba
 */
public class userFlashNtry implements Comparator<userFlashNtry> {

    /**
     * file entry
     */
    protected final File f;

    /**
     * create instance
     *
     * @param fl file
     */
    protected userFlashNtry(File fl) {
        f = fl;
    }

    public int compare(userFlashNtry o1, userFlashNtry o2) {
        return o1.f.getName().compareTo(o2.f.getName());
    }

}

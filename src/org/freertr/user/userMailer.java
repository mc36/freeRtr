package org.freertr.user;

/**
 * email reader
 *
 * @author matecsaba
 */
public class userMailer {

    private final userScreen console;

    private String path;
    
    /**
     * create mailer
     *
     * @param pip console
     * @param p path
     */
    public userMailer(userScreen pip, String p) {
        console = pip;
        path = p;
    }


    /**
     * do work
     */
    public void doWork() {
        
    }
    
}

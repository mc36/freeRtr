package util;

/**
 * version information
 *
 * @author matecsaba
 */
public class verCore {

    private verCore() {
    }

    /**
     * os name
     */
    public final static String name = "freeRouter";

    /**
     * author
     */
    public final static String author = "matecsaba";

    /**
     * trusted certificate
     */
    public final static String pubKey = "MIHwMIGoBgcqhkjOOAQBMIGcAkEAiewXLBuBaGK6xToyWDHtAeG4rnIptP0Z7bufapDds85uKT2cLSvK5GDVs3B/bsIufNtgkfyuH5a3/mqcHFar5QIVAL2IomkVLGW/3/EcqDTmBDqr6eidAkB1nbx3Ip9rfYVUyqFeaSilWSRiuZMhiUOz3vUuDjj8VDOSorsnZk+emG6elhHgEWriW8kp9+Kab9WfVlk9hVLWA0MAAkB9Rt/iuZViZ0O9btydhXJ9KodwGSt4z4Z9xSwssDsqVieYrXRJbWThi19lYknQQy0qVN5kBniuCYz8j6+KiZfu";

    /**
     * compile year
     */
    public final static int year = 17;

    /**
     * compile month
     */
    public final static int month = 11;

    /**
     * compile day
     */
    public final static int day = 5;

    /**
     * statement of release
     */
    public final static String state = "-testing";

    /**
     * set true to hide experimental features
     */
    public static boolean release = false;

    /**
     * url of product
     */
    public final static String homeUrl = "http://freerouter.nop.hu/";

    /**
     * license text
     */
    public final static String license[] = {
        "place on the net: " + homeUrl,
        "license: http://creativecommons.org/licenses/by-sa/4.0/",
        "quote1: make the world better",
        "quote2: if a machine can learn the value of human life, maybe we can too",
        "quote3: be liberal in what you accept, and conservative in what you send",
        "quote4: the beer-ware license for selected group of people:",
        author + " wrote these files. as long as you retain this notice you",
        "can do whatever you want with this stuff. if we meet some day, and",
        "you think this stuff is worth it, you can buy me a beer in return"
    };

    /**
     * logo text
     */
    public final static String logo[] = {
        "#######                         ##################",
        " ##  ##                                 ##",
        " ##   # ## ###   #####   #####  ## ###  ## ## ###",
        " ## #    ### ## ##   ## ##   ##  ### ## ##  ### ##",
        " ####    ##  ## ####### #######  ##  ## ##  ##  ##",
        " ## #    ##     ##      ##       ##     ##  ##",
        " ##      ##     ##   ## ##   ##  ##     ##  ##",
        "####     ##      #####   #####   ##     ##  ##"
    };

    /**
     * interface names
     */
    public final static String ifaces[] = {"atm", "ethernet", "serial", "cellular"};

}

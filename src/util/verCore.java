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
    public final static String author = "cs@nop";

    /**
     * current public key
     */
    public final static String pubKeyC = "MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAr90aVMLfKy8vYts99XdypldSAAZRmbPPUd/x9RdSFVxE01qxD8Zco89m51uXCsK/dGgUB/CuAnNfFA0cX0B5htNWPNJ75GJh+DOWgw1Sb3NBil+0/Nd6R0MzbVTP4rvxRMlf66JhDH2dK/QfMBYSqiSi7k5paVbeFed5TbWtZGo6PHKMzjtz9LMOQhNtW6ZNnHsIBx4qvmSFrsaylvWMRFqAY1Sfjt/Xl2T422+RIhiGEyeztB3i7a0rfU/DGfBWAJaHLFiX0d3ybwzwzRR++kXKxoZFdYIhnG72iJQaoddjBMTUtawQGzlL8bcx7GCOeZ1Je3Z9VSL8EqFvjpP/jeiK8LG+LUDTkrPlWZJ1Zhmpm2IPGdhmGLC8ZMoHvEDyjJwMLS/dGKvIGTT0L27C1GkKeT9Q5AKRUFpOLeg2fERzAGsYZ1oZhLDwAECo/9MQoMESASN7CQCgib9ScJZaNq2TMQC8FOWcQpidFXHrc8kcSm37qeVAZabdECiPlbRSzAXpGbDpPX+wgeH8EgvbRIGUeR2uaPycUvH/0WOmaqt+R4YiEfNlPCDfO0vFwAgd54n4AKOOvpsFSqH5ScWVPbesiBjTF4fVbBK0wvR5arfmRg2f4/ISo9+tJy2fYhYDhLuVxEna/vnoHiBUQhJa5anOzXvs1DHMiLulSnOPyGsCAwEAAQ==";

    /**
     * old public key
     */
    public final static String pubKeyO = "MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAr90aVMLfKy8vYts99XdypldSAAZRmbPPUd/x9RdSFVxE01qxD8Zco89m51uXCsK/dGgUB/CuAnNfFA0cX0B5htNWPNJ75GJh+DOWgw1Sb3NBil+0/Nd6R0MzbVTP4rvxRMlf66JhDH2dK/QfMBYSqiSi7k5paVbeFed5TbWtZGo6PHKMzjtz9LMOQhNtW6ZNnHsIBx4qvmSFrsaylvWMRFqAY1Sfjt/Xl2T422+RIhiGEyeztB3i7a0rfU/DGfBWAJaHLFiX0d3ybwzwzRR++kXKxoZFdYIhnG72iJQaoddjBMTUtawQGzlL8bcx7GCOeZ1Je3Z9VSL8EqFvjpP/jeiK8LG+LUDTkrPlWZJ1Zhmpm2IPGdhmGLC8ZMoHvEDyjJwMLS/dGKvIGTT0L27C1GkKeT9Q5AKRUFpOLeg2fERzAGsYZ1oZhLDwAECo/9MQoMESASN7CQCgib9ScJZaNq2TMQC8FOWcQpidFXHrc8kcSm37qeVAZabdECiPlbRSzAXpGbDpPX+wgeH8EgvbRIGUeR2uaPycUvH/0WOmaqt+R4YiEfNlPCDfO0vFwAgd54n4AKOOvpsFSqH5ScWVPbesiBjTF4fVbBK0wvR5arfmRg2f4/ISo9+tJy2fYhYDhLuVxEna/vnoHiBUQhJa5anOzXvs1DHMiLulSnOPyGsCAwEAAQ==";

    /**
     * compile year
     */
    public final static int year = 19;

    /**
     * compile month
     */
    public final static int month = 8;

    /**
     * compile day
     */
    public final static int day = 29;

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
    public final static String ifaces[] = {"atm", "ethernet", "serial", "cellular", "wireless"};

}

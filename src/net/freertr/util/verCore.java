package net.freertr.util;

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
    public final static String pubKeyC = "MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEMSb9b6nWj7zdHnJcBGMAYwUUhwxTj4qmSeAL7cY3mZxFLk3y57G9nD61f9dqX12Ld95dKc+cyF7Zt0R/KOpCbe59tDxjhxwih8GceSLOypnpDKSY47PyrFdK78+fnZjZxDWBLm5PqhflJi4GapKSjIz6iYFYh4b/UYoto6u9RN6bn2sJC6BqgAfiXQODxztcKdHmNyQEs+/63aOuIcN6XQqIXokk4BPRz6gBDnx62mUXWK/v3u8BDA6dAQep7FXvL6h4ZmopnqRgoMA1nBN7vf5jZFB7yYR2UowuNEoK6WGcWz8hY5WAG+7/AFlizp16n6qfBG985SIrXxD30NPMzrhllhLAeewlMZzmvAtFOzI+yw288UE9bhj5bbpZKcYQ5ldXmQDCWhSipSsYGtqbi2OSlXuztIufiNr0OF4RIDYgQQeH3cX9imz/VTAd5eLe3/6JJY7el+WQzc5B7aROeUAokoHk9tce9ACb4MpCSHmWhYLbCD3RP68WCLX4C4RMQHILIudFao/23ce/RuFjVobpL0WsKy4Z1EE8zya/B9L2uGqM6bWd6RO7+8PERBqqegh3o5ggU52FEUW01priVy1uy/KrgmYFBahBntnJv41dO0mNbk/0EgGJiHUUrHV2yoZPnqRlCsu/of927aBrOpgrx9FmqYdE0UP8W3ENTBrUCAwEAAQ==";

    /**
     * old public key
     */
    public final static String pubKeyO = "MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEMSb9b6nWj7zdHnJcBGMAYwUUhwxTj4qmSeAL7cY3mZxFLk3y57G9nD61f9dqX12Ld95dKc+cyF7Zt0R/KOpCbe59tDxjhxwih8GceSLOypnpDKSY47PyrFdK78+fnZjZxDWBLm5PqhflJi4GapKSjIz6iYFYh4b/UYoto6u9RN6bn2sJC6BqgAfiXQODxztcKdHmNyQEs+/63aOuIcN6XQqIXokk4BPRz6gBDnx62mUXWK/v3u8BDA6dAQep7FXvL6h4ZmopnqRgoMA1nBN7vf5jZFB7yYR2UowuNEoK6WGcWz8hY5WAG+7/AFlizp16n6qfBG985SIrXxD30NPMzrhllhLAeewlMZzmvAtFOzI+yw288UE9bhj5bbpZKcYQ5ldXmQDCWhSipSsYGtqbi2OSlXuztIufiNr0OF4RIDYgQQeH3cX9imz/VTAd5eLe3/6JJY7el+WQzc5B7aROeUAokoHk9tce9ACb4MpCSHmWhYLbCD3RP68WCLX4C4RMQHILIudFao/23ce/RuFjVobpL0WsKy4Z1EE8zya/B9L2uGqM6bWd6RO7+8PERBqqegh3o5ggU52FEUW01priVy1uy/KrgmYFBahBntnJv41dO0mNbk/0EgGJiHUUrHV2yoZPnqRlCsu/of927aBrOpgrx9FmqYdE0UP8W3ENTBrUCAwEAAQ==";

    /**
     * compile year
     */
    public final static int year = 22;

    /**
     * compile month
     */
    public final static int month = 1;

    /**
     * compile day
     */
    public final static int day = 17;

    /**
     * statement of release
     */
    public final static String state = "-cur";

    /**
     * set true to hide experimental features
     */
    public static boolean release = false;

    /**
     * url of product
     */
    public final static String homeUrl = "http://www.freertr.net/";

    /**
     * license text
     */
    public final static String[] license = {
        "place on the web: " + homeUrl,
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
    public final static String[] logo = {
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
    public final static String[] ifaces = {"atm", "ethernet", "serial", "cellular", "wireless"};

}

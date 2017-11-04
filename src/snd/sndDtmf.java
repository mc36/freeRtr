package snd;

/**
 * dual tone multi frequency signaling
 *
 * @author matecsaba
 */
public class sndDtmf {

    private final static int[] dtmfCols = {1209, 1336, 1477, 1633};

    private final static int[] dtmfRows = {697, 770, 852, 941};

    private final static char[][] dtmfRowXcol = {{'1', '2', '3', 'a'}, {'4', '5', '6', 'b'}, {'7', '8', '9', 'c'},
    {'*', '0', '#', 'd'}};

    private final static int[] busy = {480, 620};

    private final static int[] ringback = {440, 480};

    private final static int[] dialTone = {350, 440};

}

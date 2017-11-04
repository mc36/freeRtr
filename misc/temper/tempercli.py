# sudo apt-get install python-usb
from __future__ import print_function
from temperusb import TemperHandler
import getopt, sys, os.path

def usage():
    print("%s [-p] [-q] [-h|--help]" % os.path.basename(sys.argv[0]))
    print("  -q    quiet: only output temperature as a number. Usefull for external program parsing")

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], ":hpcfq", ["help"])
    except getopt.GetoptError as err:
        print(str(err))
        usage()
        sys.exit(2)
    disp_ports = False
    quiet_output = False
    for o, a in opts:
        if o == "-p":
            disp_ports = True
        elif o == "-q":
            quiet_output = True
        elif o in ("-h", "--help"):
            usage()
            sys.exit()
        else:
            assert False, "unhandled option"

    th = TemperHandler()
    devs = th.get_devices()
    readings = []
    if not quiet_output:
        print("Found %i devices" % len(devs))

    for i, dev in enumerate(devs):
        readings.append({'device': i,
                         'temperature': dev.get_temperature(),
                         'ports': dev.get_ports(),
                         'bus': dev.get_bus()
                         })

    for reading in readings:
        if quiet_output:
            print('%0.4f'
                % reading['temperature'])
        else:
            if disp_ports:
                portinfo = " (bus %s - port %s)" % (reading['bus'],
                                                    reading['ports'])
            else:
                portinfo = ""
            print('Device #%i%s: %0.4f C' 
                  % (reading['device'],
                     portinfo,
                     reading['temperature']))

if __name__ == '__main__':
    main()

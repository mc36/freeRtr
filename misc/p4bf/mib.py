import os, io, mmap, struct, time

## Apparently, this is needed to make super() work with python 2.7
__metaclass__ = type

class SMIv2:
    def __init__(self, mib, offset):
        self.mib = mib
        self.offset = offset

class int_t(SMIv2):
    def __init__(self, mib, offset):
       super(int_t, self).__init__(mib, offset)

    def set(self, value):
        value = value % self.modulo
        struct.pack_into(self.fmt, self.mib.map, self.offset, value)

    def get(self):
        res = struct.unpack_from(self.fmt, self.mib.map, self.offset)
        return res[0]

class int32_t(int_t):
    def __init__(self, mib, offset):
        self.size = 4
        self.fmt = "@i"
        self.modulo = 2**31
        super(int32_t, self).__init__(mib, offset)

class uint32_t(int_t):
    def __init__(self, mib, offset):
        self.size = 4
        self.fmt = "@I"
        self.modulo = 2**32
        super(uint32_t, self).__init__(mib, offset)

class uint64_t(int_t):
    def __init__(self, mib, offset):
        self.size = 8
        self.fmt = "@Q"
        self.modulo = 2**64
        super(uint64_t, self).__init__(mib, offset)

class octetstr_t(SMIv2):
    def __init__(self, mib, offset, length):
        ## Account for length bytes
        self.size = 2 + length
        super(octetstr_t, self).__init__(mib, offset)

    def set(self, value):
        assert(isinstance(value, str))
        length = min(len(value), self.size-2)
        struct.pack_into("@H", self.mib.map, self.offset, length)
        struct.pack_into("{0:d}s".format(length),
                         self.mib.map, self.offset+2, value)

    def get(self):
        len = struct.unpack_from("@H", self.mib.map, self.offset)[0]
        res = struct.unpack_from("{0:d}s".format(len),
                                 self.mib.map, self.offset+2)
        return res[0]

types = {
    'Integer32': int32_t,
    'Unsigned32': uint32_t,
    'OctetStr' : octetstr_t,
    'Counter32': uint32_t,
    'Counter64': uint64_t,
    'Gauge32': uint32_t,
    'TimeTicks': uint32_t,
    ## Not yet implemented
    # 'Bits': octetstr_t
}

class MIB:
    def __init__(self, file):
        index_file = file + ".index"
        self.data_f = io.open(file, "w+b", buffering=0)
        self.index_f = io.open(index_file, "w")
        self.index_f.write(u'MIB:1\n')
        self.index_f.flush()

        self.map = None
        ## Current offset in the data segment where the next object
        ## will be allocated by register()
        self.offset = 0
        self.objects = {}

    def register(self, name, type, value = None, octet_str_len = None):
        assert(type in types.keys())
        assert(name not in self.objects.keys())

        if type == 'OctetStr':
            assert(octet_str_len is not None)
            obj = types[type](self, self.offset, octet_str_len)
        else:
            obj = types[type](self, self.offset)

        self.objects[name] = obj
        self.offset = self.offset + obj.size
        self.index_f.write(u"{0:s}: {1:d}\n".format(name, obj.size))
        self.index_f.flush()

        ## Grow the shared memory region to the new size
        fileno = self.data_f.fileno()
        os.ftruncate(fileno, self.offset)
        if self.map is not None:
            self.map.close()
        self.map = mmap.mmap(fileno, 0, mmap.MAP_SHARED, mmap.PROT_READ|mmap.PROT_WRITE)

        if value is not None:
            obj.set(value)

    def set(self, name, value):
        assert(name in self.objects.keys())
        self.objects[name].set(value)

    def get(self, name):
        assert(name in self.objects.keys())
        return self.objects[name].get()

class ifmib(MIB):
    def __init__(self, file, properties):
        super(ifmib, self).__init__(file)
        p = properties
        self.register('ifDescr', 'OctetStr', p['ifDescr'].encode('ascii'),
                      octet_str_len = 255)
        self.register('ifName', 'OctetStr', p['ifName'], octet_str_len = 255)
        self.register('ifAlias', 'OctetStr', p['ifAlias'],
                      octet_str_len = 64 ) # interface description
        self.register('ifType', 'Integer32', 6 ) # ethernetCsmacd
        self.register('ifMtu', 'Integer32', p['ifMtu'])

        if p['speed'] > 1000000000:
            self.register('ifSpeed', 'Gauge32', 4294967295) # RFC3635 sec. 3.2.8
        else:
            self.register('ifSpeed', 'Gauge32', p['speed'])
        self.register('ifHighSpeed', 'Gauge32', p['speed'] / 1000000)

        self.register('ifPhysAddress', 'OctetStr', octet_str_len = 0)
        self.register('ifAdminStatus', 'Integer32', 2) # down
        self.register('ifOperStatus', 'Integer32', 2) # down
        self.register('ifLastChange', 'TimeTicks', 0)
        self.register('_X_ifLastChange_TicksBase', 'Counter64',
                                           int(time.time()))
        self.register('ifInOctets', 'Counter32', 0)
        self.register('ifInUcastPkts', 'Counter32', 0)
        self.register('ifInDiscards', 'Counter32', 0)
        self.register('ifInErrors', 'Counter32', 0)
        self.register('ifInUnknownProtos', 'Counter32', 0)
        self.register('ifOutOctets', 'Counter32', 0)
        self.register('ifOutUcastPkts', 'Counter32', 0)
        self.register('ifOutDiscards', 'Counter32', 0)
        self.register('ifOutErrors', 'Counter32', 0)

        self.register('ifInMulticastPkts', 'Counter32', 0)
        self.register('ifInBroadcastPkts', 'Counter32', 0)
        self.register('ifOutMulticastPkts', 'Counter32', 0)
        self.register('ifOutBroadcastPkts', 'Counter32', 0)
        self.register('ifHCInOctets', 'Counter64', 0)
        self.register('ifHCInUcastPkts', 'Counter64', 0)
        self.register('ifHCInMulticastPkts', 'Counter64', 0)
        self.register('ifHCInBroadcastPkts', 'Counter64', 0)
        self.register('ifHCOutOctets', 'Counter64', 0)
        self.register('ifHCOutUcastPkts', 'Counter64', 0)
        self.register('ifHCOutMulticastPkts', 'Counter64', 0)
        self.register('ifHCOutBroadcastPkts', 'Counter64', 0)
        self.register('ifLinkUpDownTrapEnable', 'Integer32', 2) # disabled
        self.register('ifPromiscuousMode', 'Integer32', 2) # false
        ## This information is not available from the $PORT table
        self.register('ifConnectorPresent', 'Integer32', 1) # true
        self.register('ifCounterDiscontinuityTime', 'TimeTicks', 0)
        ## Discontinuities per RFC 2233 are not possible for
        ## this platform (interfaces indices are fixed)
        self.register('_X_ifCounterDiscontinuityTime', 'Counter64', 0)

    def update(self, port, stat):
        if port['$PORT_ENABLE']:
            self.set('ifAdminStatus', 1) # up
        else:
            self.set('ifAdminStatus', 2) # down
        old_oper_state = self.get('ifOperStatus')
        if port['$PORT_UP']:
            new_oper_state = 1 # up
        else:
            new_oper_state = 2 # down
        self.set('ifOperStatus', new_oper_state)
        if old_oper_state != new_oper_state:
            self.set('ifLastChange', 0)
            self.set('_X_ifLastChange_TicksBase', int(time.time()))

        self.set('ifInOctets', stat['$OctetsReceivedinGoodFrames'])
        self.set('ifInUcastPkts', stat['$FramesReceivedOK'])
        self.set('ifInDiscards', stat['$FramesDroppedBufferFull'])
        self.set('ifInErrors', stat['$FrameswithanyError'])
        self.set('ifOutOctets', stat['$OctetsTransmittedwithouterror'])
        self.set('ifOutUcastPkts', stat['$FramesTransmittedOK'])
        self.set('ifOutErrors', stat['$FramesTransmittedwithError'])
        self.set('ifInMulticastPkts', stat['$FramesReceivedwithMulticastAddresses'])
        self.set('ifInBroadcastPkts', stat['$FramesReceivedwithBroadcastAddresses'])
        self.set('ifOutMulticastPkts', stat['$FramesTransmittedMulticast'])
        self.set('ifOutBroadcastPkts', stat['$FramesTransmittedBroadcast'])

        self.set('ifHCInOctets', stat['$OctetsReceivedinGoodFrames'])
        self.set('ifHCInUcastPkts', stat['$FramesReceivedOK'])
        self.set('ifHCInMulticastPkts', stat['$FramesReceivedwithMulticastAddresses'])
        self.set('ifHCInBroadcastPkts', stat['$FramesReceivedwithBroadcastAddresses'])
        self.set('ifHCOutOctets', stat['$OctetsTransmittedwithouterror'])
        self.set('ifHCOutUcastPkts', stat['$FramesTransmittedOK'])
        self.set('ifHCOutMulticastPkts', stat['$FramesTransmittedMulticast'])
        self.set('ifHCOutBroadcastPkts', stat['$FramesTransmittedBroadcast'])

        ### Not available from stats
        #self.set('ifInUnknownProtos', 0)
        #self.set('ifOutDiscards', 0)

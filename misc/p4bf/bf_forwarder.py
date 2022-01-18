#!/usr/bin/env python3

###############################################################################
#
# Copyright 2019-present GEANT RARE project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed On an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
###############################################################################

from rare.bf_gbl_env.cst_env import *
from rare.bf_grpc_client import BfRuntimeGrpcClient
from rare.bf_ifstatus import BfIfStatus
from rare.bf_snmp_client import BfIfSnmpClient
from rare.bf_forwarder import BfForwarder
from rare.bf_forwarder.opt_parser import get_opt_parser
from salgrpcclient import SalGrpcClient

ALL_THREADS = []

class BfSubIfCounter(Thread):
    def __init__(self, threadID, name, bfgc, sck_file,pipe_name,subif_counter_interval,):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.file = sck_file
        self.die = False
        self.subif_counter_interval= subif_counter_interval
        self.pipe_name = pipe_name

    def run(self):
        try:
            logger.warning("%s - main" % (self.class_name))
            while not self.die:
                logger.debug("%s - loop" % (self.class_name))
                if len(ACTIVE_PORTS.keys())==0:
                    logger.debug("%s - No active ports" % (self.class_name))
                else:
                    logger.debug("%s - ACTIVE_PORTS%s" % (self.class_name, ACTIVE_PORTS.keys()))
                    self.getReadSwitchSubIfCounter()
                sleep(self.subif_counter_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning("%s - exited with code [%s]" % (self.class_name,_Exception()))
            self.tearDown()

    def getReadSwitchSubIfCounter(self):
        # for all subif
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_global2_path = "%s.eg_ctl" % self.pipe_name
        tbl_name_vlan_in = "%s.ig_ctl_vlan_in.tbl_vlan_in" % (tbl_global_path)
        tbl_vlan_in = self.bfgc.bfrt_info.table_get(tbl_name_vlan_in)
        tbl_name_in = "%s.ig_ctl_vlan_in.stats" % (tbl_global_path)
        tbl_stats_in = self.bfgc.bfrt_info.table_get(tbl_name_in)
        tbl_name_out = "%s.eg_ctl_vlan_out.stats" % (tbl_global2_path)
        tbl_stats_out = self.bfgc.bfrt_info.table_get(tbl_name_out)
        tbl_name_pkt_out = "%s.pkt_out_stats" % (tbl_global_path)
        tbl_stats_pkt_out = self.bfgc.bfrt_info.table_get(tbl_name_pkt_out)
        logger.debug("INGRESS STATS TABLE PATH: %s" % tbl_name_in)
        logger.debug("EGRESS STATS TABLE PATH: %s" % tbl_name_out)
        logger.debug("EGRESS PKT_OUT_STATS TABLE PATH: %s" % tbl_name_pkt_out)

        tbl_stats_in.operations_execute(self.bfgc.target, 'Sync')
        tbl_stats_out.operations_execute(self.bfgc.target, 'Sync')
        tbl_stats_pkt_out.operations_execute(self.bfgc.target, 'Sync')

        resp = tbl_vlan_in.entry_get(self.bfgc.target, [], {"from_hw": False})
        for d, k in resp:
            key_fields = k.to_dict()
            data_fields = d.to_dict()
            ACTIVE_SUBIFS[data_fields['src']]=[key_fields['ig_md.ingress_id']['value'],
                                  key_fields['hdr.vlan.vid']['value']]

        for port_id in ACTIVE_PORTS.keys():
            SUBIF_COUNTERS.update({port_id:[0,0,0,0]})
        for subif_if in ACTIVE_SUBIFS.keys():
            SUBIF_COUNTERS.update({subif_if:[0,0,0,0]})

        for key in SUBIF_COUNTERS.keys():
            logger.debug("SUBIF_COUNTERS[%s]=%s" % (key,
                                  SUBIF_COUNTERS[key]))

        for counter_id in SUBIF_COUNTERS.keys():
        # read counter from p4 switch via grpc client
            logger.debug("%s - reading counter_id[%s]" % (self.class_name,counter_id))
            # IN counters
            key_list=[tbl_stats_in.make_key([gc.KeyTuple("$COUNTER_INDEX", counter_id)])]
            data_list=None
            stats_in_entry = tbl_stats_in.entry_get(self.bfgc.target,
                                                        key_list,
                                                        {"from-hw": False},
                                                        data_list)

            stats_in = next(stats_in_entry)[0].to_dict()
            logger.debug("INGRESS STATS FOR SUBIF[%s]=%s" % (counter_id,stats_in))

            # OUT counters
            key_list=[tbl_stats_out.make_key([gc.KeyTuple("$COUNTER_INDEX", counter_id)])]
            data_list=None
            stats_out_entry = tbl_stats_out.entry_get(self.bfgc.target,
                                                        key_list,
                                                        {"from-hw": False},
                                                        data_list)
            stats_out = next(stats_out_entry)[0].to_dict()
            logger.debug("EGRESS STATS FOR SUBIF[%s]=%s" % (counter_id,stats_out))

            # PKT_OUT counters
            key_list=[tbl_stats_pkt_out.make_key([gc.KeyTuple("$COUNTER_INDEX", counter_id)])]
            data_list=None
            stats_pkt_out_entry = tbl_stats_pkt_out.entry_get(self.bfgc.target,
                                                        key_list,
                                                        {"from-hw": False},
                                                        data_list)
            stats_pkt_out = next(stats_pkt_out_entry)[0].to_dict()
            logger.debug("EGRESS PKT_OUT STATS FOR SUBIF[%s]=%s" % (counter_id,stats_pkt_out))

            SUBIF_COUNTERS.update({counter_id:[stats_in["$COUNTER_SPEC_PKTS"] - stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                               stats_in["$COUNTER_SPEC_BYTES"] - stats_pkt_out["$COUNTER_SPEC_BYTES"],
                                               stats_out["$COUNTER_SPEC_PKTS"] + stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                               stats_out["$COUNTER_SPEC_BYTES"] + stats_pkt_out["$COUNTER_SPEC_BYTES"]]})

            logger.debug("tx: ['counter','%s','%s','%s', '%s', '%s'\\n']"
                                % (counter_id,
                                stats_in["$COUNTER_SPEC_PKTS"] - stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                stats_in["$COUNTER_SPEC_BYTES"] - stats_pkt_out["$COUNTER_SPEC_BYTES"],
                                stats_out["$COUNTER_SPEC_PKTS"] + stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                stats_out["$COUNTER_SPEC_BYTES"] + stats_pkt_out["$COUNTER_SPEC_BYTES"] ))

            self.file.write("counter %s %s %s %s %s\n"
                                % (counter_id,
                                stats_in["$COUNTER_SPEC_PKTS"] - stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                stats_in["$COUNTER_SPEC_BYTES"] - stats_pkt_out["$COUNTER_SPEC_BYTES"],
                                stats_out["$COUNTER_SPEC_PKTS"] + stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                stats_out["$COUNTER_SPEC_BYTES"] + stats_pkt_out["$COUNTER_SPEC_BYTES"]))
            self.file.flush()

    def tearDown(self):
        os._exit(0)
        self.die=True

if __name__ == "__main__":
    
    args = get_opt_parser() 

    try:
        ACTIVE_PORTS = {}
        PORTS_OPER_STATUS = {}
        ACTIVE_SUBIFS = {}
        SUBIF_COUNTERS = {}
        SUBIF_COUNTERS_IN = {}
        SUBIF_COUNTERS_OUT = {}
        if (args.platform=="stordis_bf2556x_1t"):
            # start TOFINO via SAL GRPC client
            logger.warning('Starting TOFINO via SAL')
            sal_client = SalGrpcClient(args.sal_grpc_server_address)
            sal_client.TestConnection()
            sal_client.GetSwitchModel()
            sal_client.StartTofino()
            sal_client.StartGearBox()
        else:
            sal_client = None

        logger.warning("%s running on: %s" % (PROGRAM_NAME,args.platform.upper()))

        sck = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        while True:
            try:
                sck.connect((args.freerouter_address,
                             args.freerouter_port))
            except socket.error as e:
                logger.error("Failed to connect to control plane process: %s, retrying" % e)
                sleep(2)
                continue
            else:
                logger.warning("Connected to control plane %s:%s" % (args.freerouter_address, args.freerouter_port))
                break

        sckr_file = sck.makefile("r")
        sckw_file = sck.makefile("w")


        bf_client = BfRuntimeGrpcClient(args.bfruntime_address,
                                    args.p4_program_name,
                                    args.client_id,
                                    args.pipe_name,True)

        bf_ifstatus_client = BfRuntimeGrpcClient(args.bfruntime_address,
                                    args.p4_program_name,
                                    args.client_id+1,
                                    args.pipe_name,False)

        if args.snmp:
            bf_snmp = BfIfSnmpClient(1,
                             "bf_snmp",
                             bf_client,
                             args.ifmibs_dir,
                             args.stats_interval,
                             args.ifindex)
            bf_snmp.daemon=True
            bf_snmp.start()
            logger.warning("bf_switchd started with SNMP export")
        else:
            logger.warning("bf_switchd started with no SNMP export")

        bf_forwarder = BfForwarder(2,
                               "bf_forwarder",
                               args.platform,
                               bf_client,
                               sal_client,
                               sckr_file,
                               args.brdg,
                               args.mpls,
                               args.srv6,
                               args.nat,
                               args.pbr,
                               args.tun,
                               args.poe,
                               args.mcast,
                               args.polka,
                               args.nsh,
                               args.no_log_keepalive
                               )

        bf_forwarder.daemon=True
        bf_forwarder.start()

        bf_ifstatus = BfIfStatus(3,
                                "bf_ifstatus",
                                bf_ifstatus_client,
                                sckw_file,
                                1)

        bf_ifstatus.daemon=True
        bf_ifstatus.start()

        bf_if_counter = BfSubIfCounter(4,
                                "bf_if_counter",
                                bf_client,
                                sckw_file,
                                args.pipe_name,
                                5)

        bf_if_counter.daemon=True
        bf_if_counter.start()

        if args.snmp:
            ALL_THREADS = [bf_snmp,bf_forwarder,bf_ifstatus,bf_if_counter]
        else:
            ALL_THREADS = [bf_forwarder,bf_ifstatus,bf_if_counter]

        while is_any_thread_alive(ALL_THREADS):
            [t.join(1) for t in ALL_THREADS
                         if t is not None and t.is_alive()]

    except KeyboardInterrupt:
        logger.warning("\n%s - Received signal 2 ..." % PROGRAM_NAME)
        for t in ALL_THREADS:
            t.die = True
        logger.warning("%s - Quitting !" % PROGRAM_NAME)
        graceful_exit(bf_client,sck)

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
from rare.bf_ports import BfPorts
from rare.bf_natcounter import BfNatCounter
from rare.bf_bridgecounter import BfBridgeCounter
from rare.bf_inspectcounter import BfInspectCounter
from rare.bf_flowspeccounter import BfFlowspecCounter
from rare.bf_nshcounter import BfNshCounter
from rare.bf_aclcounter import BfAclCounter
from rare.bf_forwarder import BfForwarder
from rare.bf_forwarder.opt_parser import get_opt_parser

try:
    from salgrpcclient import SalGrpcClient
except ImportError:
    logger.warning("SAL import failed")

next_thread_id = 0
ALL_THREADS = []

if __name__ == "__main__":

    args = get_opt_parser()

    try:

        ## The beta version of the APS BSP for 9.9+ does not include
        ## the SAL. In that case, start bf_switchd regularly.
        if args.platform == "stordis_bf2556x_1t" and 'SalGrpcClient' in locals():
            # start TOFINO via SAL GRPC client
            logger.warning("Starting TOFINO via SAL")
            sal_client = SalGrpcClient(args.sal_grpc_server_address)
            sal_client.TestConnection()
            sal_client.GetSwitchModel()
            sal_client.StartTofino()
            sal_client.StartGearBox()
        else:
            sal_client = None

        logger.warning("%s running on: %s" % (PROGRAM_NAME, args.platform.upper()))

        sck = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        while True:
            try:
                sck.connect((args.freerouter_address, args.freerouter_port))
            except socket.error as e:
                logger.error(
                    "Failed to connect to control plane process: %s, retrying" % e
                )
                sleep(2)
                continue
            else:
                logger.warning(
                    "Connected to control plane %s:%s"
                    % (args.freerouter_address, args.freerouter_port)
                )
                break

        sckr_file = sck.makefile("r")
        sckw_file = sck.makefile("w")

        def newClient(bind = False):
            global next_thread_id
            thread_id = next_thread_id
            client = BfRuntimeGrpcClient(
                args.bfruntime_address,
                args.p4_program_name,
                args.client_id + thread_id,
                args.pipe_name,
                bind,
            )
            next_thread_id += 1
            return client, thread_id

        def startThread(thread):
            thread.daemon= True
            thread.start()
            ALL_THREADS.append(thread)

        bf_client, bf_client_id = newClient(bind = True)
        bf_ports_c, bf_ports_id = newClient()
        bf_forwarder = BfForwarder(
            bf_client_id,
            "bf_forwarder",
            args.platform,
            bf_client,
            sal_client,
            sckr_file,
            sckw_file,
            args.no_log_keepalive,
        )
        startThread(bf_forwarder)
        startThread(BfPorts(bf_ports_id, "bf_ports", args.platform,
                            bf_ports_c, sckw_file, args.pipe_name, 1, 5,
                            args.snmp, args.ifmibs_dir, args.ifindex))

        if bf_forwarder.dp_capabilities["nat"] == True:

            bf_natcounter_c, id = newClient()
            startThread(BfNatCounter(id, "bf_natcounter", bf_natcounter_c,
                                     sckw_file, args.pipe_name, 30))
        else:
            logging.warning("%s - nat not supported" % PROGRAM_NAME)

        if bf_forwarder.dp_capabilities["bridge"] == True:

            bf_bridgecounter_c, id = newClient()
            startThread(BfBridgeCounter(id, "bf_bridgecounter", bf_bridgecounter_c,
                                        sckw_file, args.pipe_name, 30))
        else:
            logging.warning("%s - bridge not supported" % PROGRAM_NAME)

        if (bf_forwarder.dp_capabilities["inspect_in"] == True
            or bf_forwarder.dp_capabilities["inspect_out"] == True
            ):

            bf_inspectcounter_c, id = newClient()
            startThread(BfInspectCounter(id, "bf_inspectcounter",
                                         bf_inspectcounter_c, sckw_file,
                                         args.pipe_name, 30))
        else:
            logging.warning("%s - inspect not supported" % PROGRAM_NAME)

        if bf_forwarder.dp_capabilities["flowspec"] == True:

            bf_flowspeccounter_c, id = newClient()
            startThread(BfFlowspecCounter(id, "bf_flowspeccounter",
                                          bf_flowspeccounter_c, sckw_file,
                                          args.pipe_name, 30))
        else:
            logging.warning("%s - flowspec not supported" % PROGRAM_NAME)

        if bf_forwarder.dp_capabilities["nsh"] == True:

            bf_nshcounter_c, id = newClient()
            startThread(BfNshCounter(id, "bf_nshcounter", bf_nshcounter_c,
                                   sckw_file, args.pipe_name, 30))
        else:
            logging.warning("%s - nsh not supported" % PROGRAM_NAME)

        if (bf_forwarder.dp_capabilities["inacl"] == True
            and bf_forwarder.dp_capabilities["outacl"] == True
            ):

            bf_aclcounter_c, id = newClient()
            startThread(BfAclCounter(id, "bf_aclcounter", bf_aclcounter_c,
                                     sckw_file, args.pipe_name, 30))
        else:
            logging.warning("%s - acl not supported" % PROGRAM_NAME)

        while is_any_thread_alive(ALL_THREADS):
            [t.join(1) for t in ALL_THREADS if t is not None and t.is_alive()]

    except KeyboardInterrupt:
        logger.warning("\n%s - Received signal 2 ..." % PROGRAM_NAME)
        for t in ALL_THREADS:
            t.die = True
        logger.warning("%s - Quitting !" % PROGRAM_NAME)
        graceful_exit(bf_client, sck)

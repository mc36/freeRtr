#! /usr/bin/env python3

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

import argparse, grpc, os, sys, socket, logging, mib, re, linecache, shutil, inspect
from time import sleep

SDE = os.environ.get("SDE", "~/bf-sde-9.7.0")
SDE_INSTALL = os.environ.get("SDE_INSTALL", SDE + "/install")
BF_RUNTIME_LIB = SDE_INSTALL + "/lib/python3.9/site-packages/tofino/"
BSP_FILE_PATH = SDE_INSTALL + "/lib/libpltfm_mgr.so"
P4_BASE="%s/share/tofinopd/" % SDE_INSTALL

# set our lib path
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "./", BF_RUNTIME_LIB))
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "./", BF_RUNTIME_LIB+"bfrt_grpc"))

import bfrt_grpc.bfruntime_pb2 as bfruntime_pb2
import bfrt_grpc.client as gc

PROGRAM_NAME = os.path.basename(sys.argv[0])

log_level = logging.WARNING

logger = logging.getLogger(PROGRAM_NAME)
if not len(logger.handlers):
    logger.addHandler(logging.StreamHandler())
    logger.setLevel(log_level)

logger.warn("SDE=%s" % SDE)

SINGLE = 1
DUAL = 2
QUAD = 4

def logException(location, e):
    logger.error("%s: %s error: %s at line number %s" % (location,
                                                         type(e).__name__,
                                                         e,
                                                         format(sys.exc_info()[-1].tb_lineno)))

class BfRuntimeGrpcClient:
    def __init__(self, grpc_addr, p4_base, p4_program_list, device_id, client_id, profile_name):
        self.class_name = type(self).__name__
        self.grpc_addr = grpc_addr # "localhost:50052"
        self.p4_base = p4_base
        self.p4_name_list = p4_program_list
        self.device_id = device_id
        self.client_id = client_id
        self.profile_name = profile_name
        self.notification = None
        self.is_master = True
        self.interface = None
        logger.warn("GRPC_ADDRESS: %s" % self.grpc_addr)
        logger.warn("P4_BASE: %s" % self.p4_base)
        logger.warn("P4_NAME_LIST: %s" % self.p4_name_list)
        logger.warn("DEVICE_ID: %s" % self.device_id)
        logger.warn("CLIENT_ID: %s" % self.client_id)
        logger.warn("PROFILE_NAME: %s" % self.profile_name)

        try:
            cfg_list = []
            self.setUpGrpcClient()
            cfg_list = self.setP4ProgramList()
            self.sendP4ProgramList(cfg_list)

            # Bind p4 program but I'm not sure if this is uselful
            # as we will tear down the grpc connection afterwards
            #self.interface.bind_pipeline_config(self.p4_name)
            #logger.warn("p4 program %s uploaded to %s", self.p4_name_list,self.grpc_addr)

            # Tearing down grpc connection disconnecting client from device
            # as we are only loading config (no p4 dataplane processing)

            self.tearDown()

        except Exception as e:
            logger.error("Error in %s. Is grpc server running at %s" % sys._getframe().f_code.co_name, self.grpc_addr)
            self.tearDown()
            sys.exit(1)

    def setUpGrpcClient(self):
        try:
            logger.warn("Binding client[%s] to device[%s]@%s with MASTER role", self.client_id, self.device_id, self.grpc_addr)
            self.interface = gc.ClientInterface(self.grpc_addr, client_id=self.client_id,
                             device_id=self.device_id, is_master=self.is_master, notifications=self.notification)
        except Exception as e:
            logger.error("Error in %s. Is grpc server running at %s ?" % (sys._getframe().f_code.co_name, self.grpc_addr))


    def setP4ProgramList(self):
        cfg_list = []
        # By default program is loaded on all TOFINO pipelines
        pipe_id_list = [0,1,2,3]

        try:
            if len(self.p4_name_list) == SINGLE:
                p4_name = self.p4_name_list[0]
                logger.warn("Setting config for: %s" % p4_name)
                cfg_list = [gc.ForwardingConfig(p4_name,
                             self.bfrt_fp(self.p4_base,p4_name),
                             [gc.ProfileInfo(self.profile_name,
                                             self.cxt_fp(self.p4_base, p4_name, self.profile_name),
                                             self.tna_fp(self.p4_base, p4_name, self.profile_name),
                                             pipe_id_list)]
                                             )]
                logger.warn("p4_base = %s" % self.p4_base)
            else:
                pipe_index = 0
                for p4_name in self.p4_name_list:
                    logger.warn("Setting config for: %s" % p4_name)
                    pfl_info_list = [gc.ProfileInfo(self.profile_name,
                                                    self.cxt_fp(self.p4_base, p4_name, self.profile_name),
                                                    self.tna_fp(self.p4_base, p4_name, self.profile_name),
                                                    [pipe_index])]

                    fwd_cfg = gc.ForwardingConfig(p4_name,
                                                  self.bfrt_fp(self.p4_base,p4_name),
                                                  pfl_info_list)

                    cfg_list.append(fwd_cfg)
                    pipe_index = pipe_index + 1

            return cfg_list

        except Exception as e:
            logException(sys._getframe().f_code.co_name,e)

    def sendP4ProgramList(self,cfg_list):

        try:
            logger.warn("Sending VERIFY, VERIFY_AND_WARM_INIT_BEGIN and WARM_INIT_END")

            action = bfruntime_pb2.SetForwardingPipelineConfigRequest.VERIFY_AND_WARM_INIT_BEGIN_AND_END

            success = self.interface.send_set_forwarding_pipeline_config_request(
                                action,
                                self.p4_base,
                                cfg_list)

            if not success:
                raise RuntimeError("Failed to load p4 configuration(s) to %s" % self.grpc_addr)

            logger.warn("Config sent for %s !" % self.p4_name_list)

        except Exception as e:
            logException(sys._getframe().f_code.co_name, e)

    def tearDown(self):
        if self.interface is not None:
            logger.warn("Tearing down gracefully grpc session with %s" % self.grpc_addr)
            self.interface._tear_down_stream()

    def bfrt_fp(self, base_path, p4_name):
        return base_path + "/" + p4_name + "/bf-rt.json"

    def cxt_fp(self, base_path, p4_name, profile_name):
        return base_path + "/" + p4_name + "/" + profile_name + "/context.json"

    def tna_fp(self, base_path, p4_name, profile_name):
        return base_path + "/" + p4_name + "/" + profile_name + "/" + "tofino" + ".bin"


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="BfRuntime controller")

    group = parser.add_mutually_exclusive_group()

    group.add_argument(
        "--single",
        help="Load single p4 program on the entire switch",
        action="store_true",
        required=False,
        default=True,
    )
    group.add_argument(
        "--multi",
        help="Number of pipe available (depends on TOFINO NPU model)",
        type=str,
        action="store",
        choices=["dual","quad"],
        required=False,
        default="",
    )
    parser.add_argument(
        "--bfruntime-address",
        help="BfRuntime address",
        type=str,
        action="store",
        required=False,
        default="localhost:50052",
    )
    parser.add_argument(
        "--p4-program",
        help="P4 program to load",
        type=str,
        action="store",
        required=False,
        default="",
    )
    parser.add_argument(
        "--p4-p0",
        help="P4 program to load on logical pipe 0",
        type=str,
        action="store",
        required=False,
        default="",
    )
    parser.add_argument(
        "--p4-p1",
        help="P4 program to load on logical pipe 1",
        type=str,
        action="store",
        required=False,
        default="",
    )
    parser.add_argument(
        "--p4-p2",
        help="P4 program to load on logical pipe 2",
        type=str,
        action="store",
        required=False,
        default="",
    )
    parser.add_argument(
        "--p4-p3",
        help="P4 program to load on logical pipe 3",
        type=str,
        action="store",
        required=False,
        default="",
    )
    parser.add_argument(
        "--device-id",
        help="p4 switch device-id",
        type=int,
        action="store",
        required=False,
        default=0,
    )
    parser.add_argument(
        "--client-id",
        help="grpc client-id",
        type=int,
        action="store",
        required=False,
        default=0,
    )
    parser.add_argument(
        "--profile-name",
        help="TOFINO profile name",
        type=str,
        action="store",
        required=False,
        default="pipe",
    )

    args = parser.parse_args()
    p4_program_list = []

    try:
        DUAL_PIPE = "dual"
        QUAD_PIPE = "quad"
        pipe_list = None


        if (args.multi == "dual") or (args.multi == "quad"):
            args.single = False
            logger.warn("%s running on NPU (%s pipe mode)" % (PROGRAM_NAME,args.multi))
        else:
            logger.warn("%s running on NPU (single pipe mode)" % (PROGRAM_NAME))

        logger.debug("args.single = %s" % args.single)
        logger.debug("args.multi = %s" % args.multi)

        if args.single is True:
            if args.p4_program != "":
                p4_program_list.append(args.p4_program)
            else:
                raise Exception("--p4-program name is mandatory")
        elif args.multi == DUAL_PIPE:
            if (args.p4_p0 != "") and (args.p4_p1 != ""):
                p4_program_list.append(args.p4_p0)
                p4_program_list.append(args.p4_p1)
            else:
                raise Exception("DUAL PIPE configuration requires --p4-p0 and --p4-p1 programs be allocated on per pipe id basis")
        elif args.multi == QUAD_PIPE:
            if (args.p4_p0 != "") and \
               (args.p4_p1 != "") and \
               (args.p4_p2 != "") and \
               (args.p4_p3 != ""):
                p4_program_list.append(args.p4_p0)
                p4_program_list.append(args.p4_p1)
                p4_program_list.append(args.p4_p2)
                p4_program_list.append(args.p4_p3)
            else:
                raise Exception("QUAD PIPES configuration requires --p4-p0, --p4-p1, --p4-p2 and --p4-p3 programs be allocated on per pipe id basis")
        else:
            raise Exception("More than 4 pipes !!!!")

        bf_client = BfRuntimeGrpcClient(args.bfruntime_address,
                                    P4_BASE,
                                    p4_program_list,
                                    args.device_id,
                                    args.client_id,
                                    args.profile_name)

    except Exception as e:
        logException(PROGRAM_NAME,e)
        #logger.error("%s: %s error: %s at line number %s" % (PROGRAM_NAME,type(e).__name__, e, format(sys.exc_info()[-1].tb_lineno)))
        sys.exit(1)


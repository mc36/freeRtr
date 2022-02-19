#! /usr/bin/python3

import argparse, os, sys, inspect, time, logging
import grpc

from sal_services_pb2 import (
    NoInput,
    PortId,
    PortInfo,
    PortConfig,
    SpeedInfo,
    FecInfo,
    FCInfo,
    ANInfo,
    EnableInfo,
    SFPInfo,
)
from sal_services_pb2 import (
    UNDEFINED_SPEED,
    SPEED_NONE,
    SPEED_1G,
    SPEED_2P5G,
    SPEED_5G,
    SPEED_10G,
    SPEED_25G,
    SPEED_40G,
    SPEED_50G,
    SPEED_100G,
)
from sal_services_pb2 import UNDEFINED_FEC, FEC_NONE, FEC_FC, FEC_RS
from sal_services_pb2 import UNDEFINED_FC, FC_NONE, FC_PAUSE, FC_PFC
from sal_services_pb2 import UNDEFINED_AN, AN_OFF, AN_ON

import sal_services_pb2_grpc


PROGRAM_NAME = os.path.basename(sys.argv[0])
log_level = logging.WARNING
logger = logging.getLogger(PROGRAM_NAME)      

SAL_PORT_ID = {
                0:1, 1:2, 2:3, 3:4, 4:5, 5:6, 6:7, 7:8,
                12:9, 13:10, 14:11, 15:12, 
                28:13, 29:14, 30:15, 31:16
                }

#class SalGrpcServer:

class SalGrpcClient:
    def __init__(self, grpc_addr):
        self.class_name = type(self).__name__

        self.grpc_addr = grpc_addr  # localhost:50053'
        self.channel = grpc.insecure_channel(self.grpc_addr)
        self.gb_stub = sal_services_pb2_grpc.SwitchServiceStub(self.channel)

    def TestConnection(self):
        method_name = inspect.currentframe().f_code.co_name
        while True:
            try:
                gb_srv_prp = self.gb_stub.TestConnection(NoInput())
                logger.warning(gb_srv_prp)

            except grpc.RpcError as e:
                logger.warning(
                    "%s.%s: Failed to connectd to SAL server at %s: %s, retrying"
                    % (self.class_name, method_name, self.grpc_addr, e)
                )
                time.sleep(2)
                continue
            else:
                logger.warning("Connected to SAL server")
                break

    def GetSwitchModel(self):
        method_name = inspect.currentframe().f_code.co_name
        try:
            gb_srv_prp = self.gb_stub.GetSwitchModel(NoInput())
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Cannot retrieve switch model"
                % (self.class_name, method_name)
            )

    def GetPortConfig(self, port_num=0, lane=0):
        method_name = inspect.currentframe().f_code.co_name
        port_id = PortId(portNum=port_num, lane=lane)
        try:
            gb_srv_prp = self.gb_stub.GetPortConfig(port_id)
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Error getting config for GearBox port:%s/%s"
                % (self.class_name, method_name, port_num, lane)
            )

    def StartTofino(self):
        method_name = inspect.currentframe().f_code.co_name
        try:
            gb_srv_prp = self.gb_stub.StartTofino(NoInput())
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Cannot start TOFINO BF_SWITCHD"
                % (self.class_name, method_name)
            )

    def StartGearBox(self):
        method_name = inspect.currentframe().f_code.co_name
        try:
            gb_srv_prp = self.gb_stub.StartGearBox(NoInput())
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Cannot start SAL"
                % (self.class_name, method_name)
            )

    def StartPTP(self):
        method_name = inspect.currentframe().f_code.co_name
        try:
            gb_srv_prp = self.gb_stub.StartPTP(NoInput())
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Cannot start PTP"
                % (self.class_name, method_name)
            )

    def AddPort(self, port_num=0, lane=0,
                      speed=SPEED_1G,
                      fec=FEC_NONE,
                      an=AN_OFF,
                      fc=FC_NONE,
                      enable=True,
                      up=True):
        method_name = inspect.currentframe().f_code.co_name
        port_id = PortId(portNum=port_num, lane=lane)
        port_config = PortConfig(
            speed=speed, fec=fec, an=an, fc=fc, enable=enable, up=up
        )
        port_info = PortInfo(portId=port_id, portConf=port_config)
        try:
            gb_srv_prp = self.gb_stub.AddPort(port_info)
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Error adding GearBox port:%s/%s"
                % (self.class_name, method_name, port_num, lane)
            )

    def DelPort(self, port_num=0, lane=0):
        method_name = inspect.currentframe().f_code.co_name
        port_id = PortId(portNum=port_num, lane=lane)
        try:
            gb_srv_prp = self.gb_stub.DelPort(port_id)
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Error deleting GearBox port:%s/%s"
                % (self.class_name, method_name, port_num, lane)
            )

    def SetSpeed(self, port_num=0, lane=0, new_speed=SPEED_1G):
        method_name = inspect.currentframe().f_code.co_name
        port_id = PortId(portNum=port_num, lane=lane)
        speed_info = SpeedInfo(portId=port_id, speed=new_speed)
        try:
            gb_srv_prp = self.gb_stub.SetSpeed(speed_info)
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Error setting speed for GearBox port:%s/%s to new_speed[%s]"
                % (
                    self.class_name,
                    method_name,
                    port_num,
                    lane,
                    new_speed,
                )
            )

    def SetFec(self, port_num=0, lane=0, new_fec=FEC_NONE):
        method_name = inspect.currentframe().f_code.co_name
        port_id = PortId(portNum=port_num, lane=lane)
        fec_info = FecInfo(portId=port_id, fec=new_fec)
        try:
            gb_srv_prp = self.gb_stub.SetFec(fec_info)
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Error setting FEC for GearBox port:%s/%s to new_fec[%s]"
                % (
                    self.class_name,
                    method_name,
                    port_num,
                    lane,
                    new_fec,
                )
            )

    def SetFc(self, port_num=0, lane=0, new_fc=FC_NONE):
        method_name = inspect.currentframe().f_code.co_name
        port_id = PortId(portNum=port_num, lane=lane)
        fc_info = FCInfo(portId=port_id, fc=new_fc)
        try:
            gb_srv_prp = self.gb_stub.SetFc(fc_info)
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Error setting FC for GearBox port:%s/%s to new_fc[%s]"
                % (self.class_name, method_name, port_num, lane, new_fc)
            )

    def SetAN(self, port_num=0, lane=0, new_an=AN_OFF):
        method_name = inspect.currentframe().f_code.co_name
        port_id = PortId(portNum=port_num, lane=lane)
        an_info = ANInfo(portId=port_id, an=new_an)
        try:
            gb_srv_prp = self.gb_stub.SetAN(an_info)
            logger.warning(gb_srv_prp)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Error setting AN for GearBox port:%s/%s to new_an[%s]"
                % (self.class_name, method_name, port_num, lane, new_an)
            )

    def EnablePort(self, port_num=0, lane=0, new_enb=True):
        method_name = inspect.currentframe().f_code.co_name
        port_id = PortId(portNum=port_num, lane=lane)
        enb_info = EnableInfo(portId=port_id, enb=new_enb)
        try:
            gb_srv_prp = self.gb_stub.EnablePort(enb_info)
            logger.warning(gb_srv_prp)
            return(0)

        except grpc.RpcError as e:
            logger.warning(
                "%s.%s: Error setting EnablePort for GearBox port:%s/%s to new_enb[%s]"
                % (
                    self.class_name,
                    method_name,
                    port_num,
                    lane,
                    new_enb,
                )
            )

    def GetSFPCInfo(self, port_num=0, lane=0):
        method_name = inspect.currentframe().f_code.co_name
        port_id = PortId(portNum=port_num, lane=lane)
        try:
            gb_srv_prp = self.gb_stub.GetSFPCInfo(port_id)
            logger.warning(gb_srv_prp)
            return(0)

        except grpc.RpcError as e:
            if e.code() == grpc.StatusCode.UNIMPLEMENTED:
                logger.warning("%s.%s: %s" % (self.class_name, method_name,
                        "UNIMPLEMENTED"))
            else:
                logger.warning(
                    "%s.%s: Error getting SFP Info from GearBox port:%s/%s"
                    % (self.class_name, method_name,
                        port_num, lane)
                )

    def speedTnaToSal(self,tna_speed):
        if tna_speed == 1:
            return(SPEED_1G)
        elif tna_speed == 2.5:
            return(SPEED_2P5G)
        elif tna_speed == 5:
            return(SPEED_5G)
        elif tna_speed == 10:
            return(SPEED_10G)
        elif tna_speed == 25:
            return(SPEED_25G)
        elif tna_speed == 40:
            return(SPEED_40G)
        elif tna_speed == 50:
            return(SPEED_50G)
        elif tna_speed == 100:
            return(SPEED_100G)
        elif tna_speed == 0:
            return(SPEED_NONE)
        else:
            return(UNDEFINED_SPEED)

    def tearDown(self):
        return
        # No teardown method on proto files
        # But we keep this because when teared down
        # all port with port_it #0-15 should be deleted

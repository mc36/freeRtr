from ..bf_gbl_env.cst_env import *


class BfRuntimeGrpcClient:
    def __init__(self, grpc_addr, p4_program_name, client_id, pipe_name, bind):
        self.class_name = type(self).__name__
        self.grpc_addr = grpc_addr  # "localhost:50052"
        self.p4_name = p4_program_name
        self.client_id = client_id
        self.pipe_name = pipe_name
        self.bind = bind
        logger.warning("GRPC_ADDRESS: %s" % self.grpc_addr)
        logger.warning("P4_NAME: %s" % self.p4_name)
        logger.warning("CLIENT_ID: %s" % self.client_id)

        while True:
            try:
                self.interface = gc.ClientInterface(
                    self.grpc_addr, client_id=self.client_id, device_id=0
                )
            except Exception as e:
                logger.error("Failed to connect to grpc server: %s, retrying" % e)
                ## Note: there appears to be a race condition inside
                ## gc.ClientInterface that can cause the call to throw
                ## an exception when in fact the socket was connected
                ## successfully.  In that case, this loop never
                ## terminates, because bf_switchd signals to the
                ## caller that a client is already connected (with
                ## this client being ourselves). This behaviour was
                ## seen when using a sleep duration of 2 seconds
                ## here. 4 seconds appears to be safe. This should be
                ## reported to Intel as a bug.
                sleep(4)
                continue
            else:
                logger.warning("Connected to grcp server")
                break

        # self.bfrt_info = self.interface.bfrt_info_get(self.p4_name)
        self.bfrt_info = self.interface.bfrt_info_get()
        self.port_table = self.bfrt_info.table_get("$PORT")
        self.port_str_info_table = self.bfrt_info.table_get("$PORT_STR_INFO")
        self.port_hdl_info_table = self.bfrt_info.table_get("$PORT_HDL_INFO")
        self.stat_table = self.bfrt_info.table_get("$PORT_STAT")
        self.metadata_table = self.bfrt_info.table_get("$PORT_METADATA")
        self.target = gc.Target(device_id=0, pipe_id=0xFFFF)
        self.client_id = int(client_id)
        if self.bind == True:
            self.interface.bind_pipeline_config(self.p4_name)

    def tearDown(self):
        self.interface.tear_down_stream()

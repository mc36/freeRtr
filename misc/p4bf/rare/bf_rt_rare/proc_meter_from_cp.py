from ..bf_gbl_env.cst_env import *


def _processMeterFromControlPlane(
    self,
    op_type,
    tbl_name,
    index,
    bytes,
    interval,
):
    if interval == 0:
        kbps = 0
    else:
        kbps = int(bytes * 8 / interval)
    try:
        tbl = self.bfgc.bfrt_info.table_get(tbl_name)
        key_field_list = [
            gc.KeyTuple("$METER_INDEX", index),
        ]
        data_field_list = [
            gc.DataTuple("$METER_SPEC_CIR_KBPS", kbps),
            gc.DataTuple("$METER_SPEC_PIR_KBPS", kbps),
            gc.DataTuple("$METER_SPEC_CBS_KBITS", 1),
            gc.DataTuple("$METER_SPEC_PBS_KBITS", 1),
        ]
        key_list = [tbl.make_key(key_field_list)]
        data_list = [tbl.make_data(data_field_list)]
    except KeyError as e:
        (str_key, str_data) = _formatErrMessage(key_field_list, data_field_list)
        err_msg = (
            "dataplane-say entry failed to prepare entry for table %s with Key[%s] Data[%s]\n"
            % (tbl_name, str_key, str_data)
        )
        logger.debug(err_msg)
        self.file_w.write(err_msg)
        self.file_w.flush()
        return

    try:

        tbl.entry_mod(self.bfgc.target, key_list, data_list)
        logger.debug(
            "Updating entry in table:%s keys:%s act_param:%s",
            tbl_name,
            key_list,
            data_list,
        )

    except gc.BfruntimeRpcException as e:
        (str_key, str_data) = _formatErrMessage(key_field_list, data_field_list)
        err_msg = (
            "dataplane-say entry failed to program entry for table %s with Key[%s] Data[%s]\n"
            % (tbl_name, str_key, str_data)
        )
        logger.debug(err_msg)
        self.file_w.write(err_msg)
        self.file_w.flush()

    except grpc.RpcError as e:
        (str_key, str_data) = _formatErrMessage(key_field_list, data_field_list)
        err_msg = (
            "dataplane-say entry failed to program entry via GRPC for table %s with Key[%s] Data[%s]\n"
            % (tbl_name, str_key, str_data)
        )
        logger.debug(err_msg)
        self.file_w.write(err_msg)
        self.file_w.flush()


def _formatErrMessage(key_field_list, data_field_list):
    str_key = ""
    for key_tuple in key_field_list:
        if str_key != "":
            str_key = (
                str_key
                + ","
                + str(key_tuple.name)
                + " "
                + str(key_tuple.value)
                + " "
                + str(key_tuple.mask)
                + " "
                + str(key_tuple.prefix_len)
                + " "
                + str(key_tuple.low)
                + " "
                + str(key_tuple.low)
                + " "
                + str(key_tuple.is_valid)
            )
        else:
            str_key = (
                str(key_tuple.name)
                + " "
                + str(key_tuple.value)
                + " "
                + str(key_tuple.mask)
                + " "
                + str(key_tuple.prefix_len)
                + " "
                + str(key_tuple.low)
                + " "
                + str(key_tuple.low)
                + " "
                + str(key_tuple.is_valid)
            )

    str_data = ""
    for data_tuple in data_field_list:
        if str_data != "":
            str_data = (
                str_data
                + ","
                + str(data_tuple.name)
                + " "
                + str(data_tuple.val)
                + " "
                + str(data_tuple.float_val)
                + " "
                + str(data_tuple.str_val)
                + " "
                + str(data_tuple.bool_val)
                + " "
                + str(data_tuple.int_arr_val)
                + " "
                + str(data_tuple.str_arr_val)
                + " "
                + str(data_tuple.container_arr_val)
            )
        else:
            str_data = (
                str(data_tuple.name)
                + " "
                + str(data_tuple.val)
                + " "
                + str(data_tuple.float_val)
                + " "
                + str(data_tuple.str_val)
                + " "
                + str(data_tuple.bool_val)
                + " "
                + str(data_tuple.int_arr_val)
                + " "
                + str(data_tuple.str_arr_val)
                + " "
                + str(data_tuple.container_arr_val)
            )
    return (str_key, str_data)

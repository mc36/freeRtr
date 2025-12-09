from ..bf_gbl_env.cst_env import *


def writeHairpinRules(self, op_type, myid, peerid):
    if op_type == WRITE:
        self.hairpins.append(myid)
    elif op_type == DELETE:
        self.hairpins.remove(myid)

    tbl_global_path = "eg_ctl.eg_ctl_hairpin"
    tbl_name = "%s.tbl_hairpin" % (tbl_global_path)
    tbl_action_name = "%s.act_set_recir" % (tbl_global_path)

    key_fields = [gc.KeyTuple("eg_md.output_id", myid)]
    data_fields = [gc.DataTuple("port", peerid)]
    key_annotation_fields = {}
    data_annotation_fields = {}

    self._processEntryFromControlPlane(
        op_type,
        tbl_name,
        key_fields,
        data_fields,
        tbl_action_name,
        key_annotation_fields,
        data_annotation_fields,
    )



    tbl_global_path = "ig_ctl.ig_ctl_bundle"
    tbl_apr_name = "%s.apr_bundle" % (tbl_global_path)
    tbl_ase_name = "%s.ase_bundle" % (tbl_global_path)
    tbl_nexthop_bundle_name = "%s.tbl_nexthop_bundle" % (tbl_global_path)

    max_grp_size = 120
    # actionprofile p4 objects are actually an object table
    tbl_apr_bundle = self.bfgc.bfrt_info.table_get(tbl_apr_name)
    # actionselector p4 objects are actually an object table
    tbl_ase_bundle = self.bfgc.bfrt_info.table_get(tbl_ase_name)
    # tbl_nexthop_bundle
    tbl_nexthop_bundle = self.bfgc.bfrt_info.table_get(tbl_nexthop_bundle_name)

    tbl_apr_bundle_key = [
        tbl_apr_bundle.make_key(
            [gc.KeyTuple("$ACTION_MEMBER_ID", peerid)]
        )
    ]
    tbl_apr_bundle_data = [
        tbl_apr_bundle.make_data(
            [gc.DataTuple("port", peerid)],
            "ig_ctl.ig_ctl_bundle.act_send_to_recir",
        )
    ]
    tbl_ase_bundle_key = [
        tbl_ase_bundle.make_key([gc.KeyTuple("$SELECTOR_GROUP_ID", myid)])
    ]

    tbl_ase_bundle_data = [
        tbl_ase_bundle.make_data(
            [
                gc.DataTuple("$MAX_GROUP_SIZE", max_grp_size),
                gc.DataTuple("$ACTION_MEMBER_ID", int_arr_val=[peerid]),
                gc.DataTuple(
                    "$ACTION_MEMBER_STATUS", bool_arr_val=[1]
                ),
            ]
        )
    ]


    tbl_nexthop_bundle_key = [
        tbl_nexthop_bundle.make_key([gc.KeyTuple("ig_md.output_id", myid)])
    ]

    tbl_nexthop_bundle_data = [
        tbl_nexthop_bundle.make_data(
            [gc.DataTuple("$SELECTOR_GROUP_ID", myid)]
        )
    ]

    if op_type == WRITE:
        tbl_apr_bundle.entry_add(
            self.bfgc.target, tbl_apr_bundle_key, tbl_apr_bundle_data
        )
        tbl_ase_bundle.entry_add(
            self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
        )
        tbl_nexthop_bundle.entry_add(
            self.bfgc.target, tbl_nexthop_bundle_key, tbl_nexthop_bundle_data
        )
    elif op_type == UPDATE:
        tbl_apr_bundle.entry_mod(
            self.bfgc.target, tbl_apr_bundle_key, tbl_apr_bundle_data
        )
        tbl_ase_bundle.entry_mod(
            self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
        )
        tbl_nexthop_bundle.entry_mod(
            self.bfgc.target, tbl_nexthop_bundle_key, tbl_nexthop_bundle_data
        )
    elif op_type == DELETE:
        tbl_apr_bundle.entry_del(
            self.bfgc.target, tbl_apr_bundle_key, tbl_apr_bundle_data
        )
        tbl_ase_bundle.entry_del(
            self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
        )
        tbl_nexthop_bundle.entry_del(
            self.bfgc.target, tbl_nexthop_bundle_key, tbl_nexthop_bundle_data
        )

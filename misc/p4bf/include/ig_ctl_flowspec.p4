/*
 * Copyright 2019-present GT RARE project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed On an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef _IG_CTL_Flowspec_P4_
#define _IG_CTL_Flowspec_P4_

#ifdef HAVE_FLOWSPEC

control IngressControlFlowspec(inout headers hdr, inout ingress_metadata_t ig_md,
                               in ingress_intrinsic_metadata_t ig_intr_md,
                               inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                               inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{

    Meter<SubIntId_t>((IPV4_FLOWSPEC_TABLE_SIZE), MeterType_t.BYTES) policer4;
    Meter<SubIntId_t>((IPV6_FLOWSPEC_TABLE_SIZE), MeterType_t.BYTES) policer6;
    DirectCounter< bit<64> > (CounterType_t.PACKETS_AND_BYTES) stats4;
    DirectCounter< bit<64> > (CounterType_t.PACKETS_AND_BYTES) stats6;

    action act4_deny() {
        stats4.count();
    }

    action act4_permit(SubIntId_t metid) {
        stats4.count();
        ig_md.flowspec_id = metid;
    }

    action act6_deny() {
        stats6.count();
    }

    action act6_permit(SubIntId_t metid) {
        stats6.count();
        ig_md.flowspec_id = metid;
    }


    table tbl_ipv4_flowspec {
        key = {
ig_md.vrf:
            exact;
hdr.ipv4.protocol:
            ternary;
hdr.ipv4.src_addr:
            ternary;
hdr.ipv4.dst_addr:
            ternary;
ig_md.layer4_srcprt:
            ternary;
ig_md.layer4_dstprt:
            ternary;
hdr.ipv4.diffserv:
            ternary;
hdr.ipv4.identification:
            ternary;
#ifdef HAVE_SGT
ig_md.sec_grp_id:
            ternary;
#endif
        }
        actions = {
            act4_permit;
            act4_deny;
            @defaultonly NoAction;
        }
        size = IPV4_FLOWSPEC_TABLE_SIZE;
        const default_action = NoAction();
        counters = stats4;
    }

    table tbl_ipv6_flowspec {
        key = {
ig_md.vrf:
            exact;
hdr.ipv6.next_hdr:
            ternary;
hdr.ipv6.src_addr:
            ternary;
hdr.ipv6.dst_addr:
            ternary;
ig_md.layer4_srcprt:
            ternary;
ig_md.layer4_dstprt:
            ternary;
hdr.ipv6.traffic_class:
            ternary;
hdr.ipv6.flow_label:
            ternary;
#ifdef HAVE_SGT
ig_md.sec_grp_id:
            ternary;
#endif
        }
        actions = {
            act6_permit;
            act6_deny;
            @defaultonly NoAction;
        }
        size = IPV6_FLOWSPEC_TABLE_SIZE;
        const default_action = NoAction();
        counters = stats6;
    }

    apply {
        if (ig_md.ipv4_valid==1)  {
            tbl_ipv4_flowspec.apply();
            ig_md.flowspec_res = policer4.execute(ig_md.flowspec_id);
        } else if (ig_md.ipv6_valid==1)  {
            tbl_ipv6_flowspec.apply();
            ig_md.flowspec_res = policer6.execute(ig_md.flowspec_id);
        }
        if ((ig_md.flowspec_id != 0) && (ig_md.flowspec_res != MeterColor_t.GREEN)) {
            ig_dprsr_md.drop_ctl = 1;
        }
    }
}

#endif

#endif // _IG_CTL_Flowspec_P4_


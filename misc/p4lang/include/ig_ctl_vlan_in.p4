#ifndef _IG_CTL_VLAN_IN_P4_  
#define _IG_CTL_VLAN_IN_P4_  
   
control IngressControlVlanIn(inout headers hdr,
                             inout ingress_metadata_t ig_md, 
                             in standard_metadata_t  ig_intr_md) { 

   action act_set_iface(SubIntId_t src) {
      ig_md.source_id = src;
      ig_md.ethertype = hdr.vlan.ethertype;
      ig_md.vlan_size = 4;
   }

   action act_set_def_iface() {
      ig_md.source_id = (SubIntId_t)ig_intr_md.ingress_port;
      ig_md.ethertype = hdr.ethernet.ethertype;
   }

   table tbl_vlan_in {
      key = {
         ig_intr_md.ingress_port: exact;  
         hdr.vlan.vid: exact;  
      }
      actions = {
         act_set_iface;
         act_set_def_iface;
      }
      size = 1024;
      default_action = act_set_def_iface();
   }

   apply {
         tbl_vlan_in.apply();
   }                               
}   

#endif // _IG_CTL_VLAN_IN_P4_
   

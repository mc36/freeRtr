from ..bf_gbl_env.var_env import *
from ..bf_rt_rare import BfRtRare  

class RareApi(BfRtRare):

    def __init__(self):
        self.name='RARE API'
        self.file_w=None

    from .bierlabel4 import writeBierLabelRules
    #from .bierlabel6 import 
    from .bierlabloc4 import writeBierLabLocRules
    #from .bierlabloc6 import 
    from .bridgelabel import writeBrlabRules
    from .bridgemac import writeBrmacRules
    from .bridgepckoudp4 import writePckoudp4rules
    from .bridgepckoudp6 import writePckoudp6rules
    from .bridgevpls import writeBrvplsRules
    from .bridgevxlan4 import writeVxlan4rules
    from .bridgevxlan6 import writeVxlan6rules
    from .bundlelist import setBundleAdmStatus
    from .bundlevlan import writeBunVlanRules
    from .copp4 import writeCopp4Rules
    from .copp6 import writeCopp6Rules
    from .cpulabel import writeCpuMplsRules
    from .duplabel4 import writeDupLabelRules
    #from .duplabel6 import 
    from .duplabloc4 import writeDupLabLocRules
    #from .duplabloc6 import 
    from .flowspec4 import writeFlowspec4Rules
    from .flowspec6 import writeFlowspec6Rules
    from .gre4 import writeGre4rules
    from .gre6 import writeGre6rules
    from .hairpin import writeHairpinRules
    from .sgttag import writeSgtTagRules
    from .sgtset import writeSgtSetRules
    from .inacl4 import writeInAcl4Rules
    from .inacl6 import writeInAcl6Rules
    from .inqos4 import writeInQos4Rules
    from .inqos6 import writeInQos6Rules
    from .inqos import writeInQosRules
    from .ipip4 import writeIpip4rules
    from .ipip6 import writeIpip6rules
    from .l2tp4 import writeL2tp4rules
    from .l2tp6 import writeL2tp6rules
    from .label4 import writeMplsRules
    #from .label6 import 
    from .labroute4 import writeGlobRules4
    from .labroute6 import writeGlobRules6
    from .mbierroute4 import writeMbierRouteRules
    #from .mbierroute6 import 
    from .mlabroute4 import writeMlabRouteRules
    #from .mlabroute6 import 
    from .mlocal4 import writeMlocal4rules
    from .mlocal6 import writeMlocal6rules
    from .mroute4 import writeMroute4rules
    from .mroute6 import writeMroute6rules
    from .mtu import setPortMTU
    from .myaddr4 import writeMyaddrRules4
    from .myaddr6 import writeMyaddrRules6
    from .mylabel4 import writeMyMplsRules
    #from .mylabel6 import 
    from .mysrv4 import writeMySrv4rules
    from .mysrv6 import writeMySrv6rules
    from .natcfg4 import writeNatCfgRules4
    from .natcfg6 import writeNatCfgRules6
    from .nattrns4 import writeNatTrnsRules4
    from .nattrns6 import writeNatTrnsRules6
    from .inspect4 import writeInspectRules4
    from .inspect6 import writeInspectRules6
    from .neigh4 import writeNexthopRules
    from .neigh4 import writeNeighborRules4
    from .neigh6 import writeNeighborRules6
    from .nhop2port import writeNhop2portRules
    from .nshfwd import writeNshFwdRules
    from .nshloc import writeNshLocRules
    from .outacl4 import writeOutAcl4Rules
    from .outacl6 import writeOutAcl6Rules
    from .outqos4 import writeOutQos4Rules
    from .outqos6 import writeOutQos6Rules
    from .outqos import writeOutQosRules
    from .pbr4hop import writePbrHopRules4
    from .pbr4lab import writePbrLabRules4
    from .pbr4norm import writePbrNormRules4
    from .pbr4vrf import writePbrVrfRules4
    from .pbr6hop import writePbrHopRules6
    from .pbr6lab import writePbrLabRules6
    from .pbr6norm import writePbrNormRules6
    from .pbr6vrf import writePbrVrfRules6
    from .polkaidx import writePolkaIndexRules
    from .polkaown import writePolkaOwnRules
    from .polkapoly import writePolkaPolyRules
    from .polroute4 import writePolkaRules4
    from .polroute6 import writePolkaRules6
    from .portbridge import writeBrprtRules
    from .portvlan import writeVlanRules
    from .portvrf import writeVrfRules
    from .pppoe import writePppoeRules
    from .route4 import writeForwardRules4
    from .route6 import writeForwardRules6
    from .routedmac import writeRoumacRules
    from .srvroute4 import writeSrvRules4
    from .srvroute6 import writeSrvRules6
    from .state import _setPortAdmStatusBF2556X1T
    from .state import _setPortAdmStatus
    from .unlabel4 import writeUnMplsRules
    #from .unlabel6 import 
    from .vpnroute4 import writeVpnRules4
    from .vpnroute6 import writeVpnRules6
    from .xconnect import writeXconnRules
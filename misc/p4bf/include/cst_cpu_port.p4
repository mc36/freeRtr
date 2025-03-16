/*
 * Copyright 2019-present GEANT RARE project
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

#ifndef _CPU_PORT_P4_
#define _CPU_PORT_P4_

/*
   Tofino1:

   The Physical port ID of the CPU PCIe port depends on the number of
   pipes (the Tofino ASIC comes in 2- and 4-pipe variants). Note that
   the CPU Eth port can't be used as interface to the control-plane on
   all platforms because it is not always wired to the main board. For
   example, on the WEDGE100BF-65X it is exposed as a port on the front
   panel. On the WEDGE100BF-32X, only two of the lanes are wired to
   the main boards (the others are not connected). Other platforms may
   use different setups.

   Tofino2:

   The documentation about CPU and recirculation ports is not yet
   available to us. We currently believe that the CPU PCIe port is
   always device port 0, i.e. attached to the (logical) pipe #0.

   In general, the "special" ports (i.e. CPU PCie, CPU Eth and
   recirculation) are always ports 0-7 on every pipe. We know for sure
   that on the BFN-T20-128Q, port 0 is the CPU PCIe port, ports
   2,3,4,5 are the CPU Eth port (of which only 4 and 5 are actually
   connected to the main CPU's onboard 10G interfaces) and ports 1,6,7
   are for recirculation. All 8 special ports on all other pipes are
   for recirculation.

   The logic below should be reviewed once the proper documentation is
   available.

   Note: recirculation ports are also pipe-specific. Port 68 is the
   recirculation port for pipe #0 on all Tofino1 variants. We mimick
   this by using port #1 on Tofino2.

 */

#if defined DUAL_PIPE || defined _WEDGE100BF32X_

#if __TARGET_TOFINO__ == 2

#define CPU_PORT 0
#define RECIR_PORT 1

#else

#define CPU_PORT 192
#define RECIR_PORT 68

#endif

#elif defined QUAD_PIPE || defined _WEDGE100BF65X_

#if __TARGET_TOFINO__ == 2

#define CPU_PORT 0
#define RECIR_PORT 1

#else

#define CPU_PORT 320
#define RECIR_PORT 68

#endif

#else

/* Fall-through for the Tofino software emulation (Tofino model). It
   doesn't emulate the CPU PCIe port, use the CPU Eth port instead. */

#if __TARGET_TOFINO__ == 2

#define CPU_PORT 2
#define RECIR_PORT 1

#else

#define CPU_PORT 64
#define RECIR_PORT 68

#endif

#endif

#endif // _CPU_PORT_P4_

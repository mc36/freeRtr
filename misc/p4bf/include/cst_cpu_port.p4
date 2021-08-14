/*
 * Copyright 2019-present GÉANT RARE project
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

/* The Physical port ID of the CPU PCIe port depends on the number of
   pipes (the Tofino ASIC comes in 2- and 4-pipe variants). Note that
   the CPU Eth port can't be used as interface to the control-plane on
   all platforms because it is not always wired to the main board. For
   example, on the WEDGE100BF-65X it is exposed as a port on the front
   panel. On the WEDGE100BF-32X, only two of the lanes are wired to
   the main boards (the others are not connected). Other platforms may
   use different setups. */

#if defined DUAL_PIPE || defined _WEDGE100BF32X_

#define CPU_PORT 192
#define RECIR_PORT 68

#elif defined QUAD_PIPE || defined _WEDGE100BF65X_

#define CPU_PORT 320
#define RECIR_PORT 68

#else

/* Fall-through for the Tofino software emulation (Tofino model). It
   doesn't emulate the CPU PCIe port, use the CPU Eth port instead. */

#define CPU_PORT 64
#define RECIR_PORT 68

#endif

#endif // _CPU_PORT_P4_

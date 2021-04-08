/*
 * Copyright 2020 ConsenSys AG.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.minima.system.network.base.peer;

import java.util.Optional;
import java.util.stream.Stream;
//import tech.pegasys.teku.infrastructure.unsigned.UInt64;
//import tech.pegasys.teku.spec.datastructures.networking.libp2p.rpc.GoodbyeMessage;

// from GoodbyeMessage.java
// public static final UInt64 REASON_CLIENT_SHUT_DOWN = UInt64.valueOf(1);
// public static final UInt64 REASON_IRRELEVANT_NETWORK = UInt64.valueOf(2);
// public static final UInt64 REASON_FAULT_ERROR = UInt64.valueOf(3);
// public static final UInt64 MIN_CUSTOM_REASON_CODE = UInt64.valueOf(128);

// // Custom reasons
// public static final UInt64 REASON_UNABLE_TO_VERIFY_NETWORK = UInt64.valueOf(128);
// public static final UInt64 REASON_TOO_MANY_PEERS = UInt64.valueOf(129);
// public static final UInt64 REASON_RATE_LIMITING = UInt64.valueOf(130)
public enum DisconnectReason {
  IRRELEVANT_NETWORK(2, true),
  UNABLE_TO_VERIFY_NETWORK(128, true),
  TOO_MANY_PEERS(129, false),
  REMOTE_FAULT(3, false),
  UNRESPONSIVE(3, false),
  SHUTTING_DOWN(1, false),
  RATE_LIMITING(130, false);

  private final long reasonCode;
  private final boolean isPermanent;

  DisconnectReason(final long reasonCode, final boolean isPermanent) {
    this.reasonCode = reasonCode;
    this.isPermanent = isPermanent;
  }

  public static Optional<DisconnectReason> fromReasonCode(final long reasonCode) {
    return Stream.of(values())
        .filter(reason -> reason.getReasonCode() == reasonCode)
        .findAny();
  }

  public long getReasonCode() {
    return reasonCode;
  }

  public boolean isPermanent() {
    return isPermanent;
  }
}

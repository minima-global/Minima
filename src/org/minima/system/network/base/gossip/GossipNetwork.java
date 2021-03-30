/*
 * Copyright 2019 ConsenSys AG.
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

package org.minima.system.network.base.gossip;

import java.util.Collection;
import java.util.Map;
//import org.apache.tuweni.bytes.Bytes;
import org.minima.system.network.base.SafeFuture;
import org.minima.system.network.base.gossip.config.GossipTopicsScoringConfig;
//import tech.pegasys.teku.networking.p2p.gossip.config.GossipTopicsScoringConfig;
//import tech.pegasys.teku.networking.p2p.peer.NodeId;
import org.minima.system.network.base.peer.NodeId;

public interface GossipNetwork {
  SafeFuture<?> gossip(String topic, byte[] data);

  TopicChannel subscribe(String topic, TopicHandler topicHandler);

  Map<String, Collection<NodeId>> getSubscribersByTopic();

  void updateGossipTopicScoring(final GossipTopicsScoringConfig config);
}

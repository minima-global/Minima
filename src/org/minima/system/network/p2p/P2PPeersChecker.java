package org.minima.system.network.p2p;

import org.minima.objects.Greeting;
import org.minima.system.Main;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOManager;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

import java.net.InetSocketAddress;
import java.util.HashSet;
import java.util.Set;

public class P2PPeersChecker extends MessageProcessor {

	/**
	 * Initialise the System
	 */
	public static final String PEERS_INIT 		= new String("PEERS_INIT");
	
	/**
	 * Add some Peers - or more function..
	 */
	public static final String PEERS_ADDPEERS 	= new String("PEERS_ADDPEERS");

	/**
	 * Check a Peer is contactable
	 */
	public static final String PEERS_CHECKPEERS 	= new String("PEERS_CHECKPEERS");

	/**
	 * Peers looper called every hour..
	 */
	public static final String PEERS_LOOP 		= new String("PEERS_LOOP");
	long PEERS_LOOP_TIMER = 1000 * 60 * 60;

	private final Set<InetSocketAddress> unverifiedPeers = new HashSet<>();

	private final Set<InetSocketAddress> verifiedPeers = new HashSet<>();

	private final P2PManager p2PManager;

	public P2PPeersChecker(P2PManager manager) {
		super("PEERS_CHECKER");
		p2PManager = manager;
		//FOR NOW - turm full logs on

		setFullLogging(false, "");
		
		//Do some Initialisation..
		PostMessage(PEERS_INIT);
		
		//First one happens after 5 mins..
		PostTimerMessage(new TimerMessage(1000 * 60 * 5, PEERS_LOOP));
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(PEERS_INIT)) {
			
			
		}else if(zMessage.getMessageType().equals(PEERS_ADDPEERS)) {
			// When a new peer address is added - check if the address is already in the verified
			// or unverified peers list. If it is not, add to the unverified list and request a check if it's contactable
			InetSocketAddress address = (InetSocketAddress) zMessage.getObject("address");
			if (!unverifiedPeers.contains(address) && !verifiedPeers.contains(address)) {
				unverifiedPeers.add(address);
				Message msg = new Message(PEERS_CHECKPEERS).addObject("address", address);
				PostMessage(msg);

			}
		}else if(zMessage.getMessageType().equals(PEERS_CHECKPEERS)) {
			InetSocketAddress address = (InetSocketAddress) zMessage.getObject("address");
			Greeting greet = NIOManager.sendPingMessage(address.getHostString(), address.getPort(), true);
			if (greet != null){
				unverifiedPeers.remove(address);
				if (verifiedPeers.size() < 250) {
					verifiedPeers.add(address);
					Message msg = new Message(P2PManager.P2P_ADD_PEER).addObject("address", address);
					p2PManager.PostMessage(msg);
				}
			} else {
				if (verifiedPeers.contains(address)){
					verifiedPeers.remove(address);
					unverifiedPeers.add(address);

					// Check the peer is still down in 30 minutes
					TimerMessage msg = new TimerMessage( 1000 * 60 * 30,PEERS_CHECKPEERS);
					msg.addObject("address", address);
					PostMessage(msg);
				} else {
					unverifiedPeers.remove(address);
				}
				Message msg = new Message(P2PManager.P2P_REMOVE_PEER).addObject("address", address);
				p2PManager.PostMessage(msg);
			}

		}else if(zMessage.getMessageType().equals(PEERS_LOOP)) {
			// Check all the verified Peers again
			for (InetSocketAddress address: verifiedPeers){
				Message msg = new Message(PEERS_CHECKPEERS).addObject("address", address);
				PostMessage(msg);
			}
			
			
			//Do it again ..
			PostTimerMessage(new TimerMessage(PEERS_LOOP_TIMER, PEERS_LOOP));
		}
		
	}

}

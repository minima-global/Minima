package org.minima.system.network.p2p;

import java.net.InetSocketAddress;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Greeting;
import org.minima.objects.base.MiniNumber;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class P2PPeersChecker extends MessageProcessor {

    /**
     * Initialise the System
     */
    public static final String PEERS_INIT = "PEERS_INIT";

    /**
     * Add some Peers - or more function..
     */
    public static final String PEERS_ADDPEERS = "PEERS_ADDPEERS";

    /**
     * Check a Peer is contactable
     */
    public static final String PEERS_CHECKPEERS = "PEERS_CHECKPEERS";

    /**
     * Peers looper called every 6 hours..
     */
    public static final String PEERS_LOOP = "PEERS_LOOP";
    long PEERS_LOOP_TIMER = 1000 * 60 * 60 * 6;

    /**
     * Check all your verified peers
     */
    public static final String PEERS_FORCEFULLCHECK = "PEERS_FORCEFULLCHECK";
    
    /**
     * Max number of Wanted Verified Peers
     */
    public static int MAX_VERIFIED_PEERS = 250;

    public Set<InetSocketAddress> getUnverifiedPeers() {
        return unverifiedPeers;
    }

    private final Set<InetSocketAddress> unverifiedPeers = new HashSet<>();

    public Set<InetSocketAddress> getVerifiedPeers() {
        return verifiedPeers;
    }

    private final Set<InetSocketAddress> verifiedPeers = new HashSet<>();

    private final P2PManager p2PManager;

    public P2PPeersChecker(P2PManager manager) {
        super("PEERS_CHECKER");
        p2PManager = manager;
        //FOR NOW - turm full logs on

        setFullLogging(false, "");

        //Do some Initialisation..
        PostMessage(PEERS_INIT);

        //First one happens after 2 hours
        //PostTimerMessage(new TimerMessage(1000 * 60 * 60 * 2, PEERS_LOOP));
    }

    /**
     * Do we have any peers
     */
    public boolean haveAnyPeers() {
    	if(verifiedPeers.size()>0 || unverifiedPeers.size()>0) {
    		return true;
    	}
    	
    	return false;
    }
    
    /**
     * Only add up to max peers in unverified list
     */
    public void checkUnverifiedPeer(InetSocketAddress zAddress) {
    	
    	//Do we know it
    	if (unverifiedPeers.contains(zAddress) || verifiedPeers.contains(zAddress)) {
    		return;
    	}
    	
    	//Check the limit
    	if(unverifiedPeers.size()<MAX_VERIFIED_PEERS) {
    		
    		//Do we have all the verified peers ?
    		if(verifiedPeers.size()>=MAX_VERIFIED_PEERS) {
    			
    			//Randomly choose if to add it.. 10% chance
    			int rand = new Random().nextInt(100);
    			if(rand<90) {
    				//MOST will not be added
    				return;
    			}
    		}
    		
    		//Add it to the list
    		unverifiedPeers.add(zAddress);
    		
    		//Send a message to check it..
    		Message msg = new Message(PEERS_CHECKPEERS).addObject("address", zAddress);
            PostMessage(msg);
            
    	}else {
    		//MAX reached..
    	}
    }
    
    @Override
    protected void processMessage(Message zMessage) throws Exception {
   	
//    	MinimaLogger.log("PEERSCHECKER:"+zMessage.toString());
    	
        if (zMessage.getMessageType().equals(PEERS_INIT)) {


        } else if (zMessage.getMessageType().equals(PEERS_ADDPEERS)) {
            
        	// When a new peer address is added - check if the address is already in the verified
            // or unverified peers list. If it is not, add to the unverified list and request a check if it's contactable
            InetSocketAddress address = (InetSocketAddress) zMessage.getObject("address");
            Set<String> localAddresses = P2PFunctions.getLocalAddresses();
            
            boolean islocal = P2PFunctions.isIPLocal(address.getHostString());
            boolean isipv6  = P2PFunctions.isIPv6(address.getHostString());
            if (GeneralParams.ALLOW_ALL_IP || (!islocal && !isipv6)) {
           
            	//Do we have room for more..
            	checkUnverifiedPeer(address);
           
            } else {
				P2PFunctions.log_debug("[-] Prevent node from adding localhost / ipv6 address to peers list "+address.getHostString());
			}

        } else if (zMessage.getMessageType().equals(PEERS_CHECKPEERS)) {

            InetSocketAddress address = (InetSocketAddress) zMessage.getObject("address");
            
            boolean force = false;
            if(zMessage.exists("force")) {
            	force = zMessage.getBoolean("force");
            }
            
            boolean forcelog = false;
            if(zMessage.exists("forcelog")) {
            	forcelog = zMessage.getBoolean("forcelog");
            }
            
            if (force || P2PFunctions.getAllConnectedConnections().size() > 0) {
                
            	//Get a Greeting if possible
            	Greeting greet = NIOManager.sendPingMessage(address.getHostString(), address.getPort(), true);
                
                //Check is the correct version..
                boolean validversion = false;
                if (greet != null) {
                
                	boolean testcheck = true;
                    String greetstr = greet.getVersion().toString();
                    if(GeneralParams.TEST_PARAMS && !greetstr.contains("TEST")) {
                        testcheck = false;
                    }else if(!GeneralParams.TEST_PARAMS && greetstr.contains("TEST")) {
                        testcheck = false;
                    } 
                    
                    //Is it correct
                    if(testcheck && greetstr.startsWith(GlobalParams.MINIMA_BASE_VERSION)) {
                    	validversion = true;
                    }
                    
                    //Check they are on the right chain..
                    if(validversion) {
                    	
                    	//Get the extra data..
                    	JSONObject extra = greet.getExtraData();
                    	
                    	//Get the block 50 back..
                    	String block 	 = extra.getString("50block","");
                    	String blockhash = extra.getString("50hash","");
                    	
                    	//Need both
                    	if(!block.equals("") && !blockhash.equals("")) {
                    	
                    		//What is our tip
                    		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
                    		
                    		//check it..
                    		if(tip != null) {
                    			
                    			//Are we below..
                    			if(tip.getTxPoW().getBlockNumber().isLess(new MiniNumber(block))) {
                    				if(forcelog || GeneralParams.PEERSCHECKER_lOG) {
                        				MinimaLogger.log("[-] Can't check peer as tip below check block tip:"+tip.getTxPoW().getBlockNumber().toString()+" check:"+block);
                        			}	
                    			}else {
                    			
	                    			//Get that node..
	                    			TxPoWTreeNode checknode = tip.getPastNode(new MiniNumber(block));
	                    			if(checknode != null) {
	                    			
	    	                			if(!checknode.getTxBlock().getTxPoW().getTxPoWID().equals(blockhash)) {
	    	                			
	    	                				if(forcelog || GeneralParams.PEERSCHECKER_lOG) {
	    	                					MinimaLogger.log("PEERS CHECKER incorrect chain! @ "+block+" "+address.toString()+" ");
	    	                				}
	    	                				
	    	                				//Wrong chain.. !
	    	                				validversion = false;
	    	                			}
	    	                			
	                    			}else {
	                    				if(forcelog || GeneralParams.PEERSCHECKER_lOG) {
	                    					MinimaLogger.log("PEERS CHECKER incorrect chain! ( null checknode ) @ "+block+" "+address.toString()+" ");
	                    				}
	                    				
	                    				//Wrong chain.. !
	                    				validversion = false;
	                    			}
                    			}
                    		} else {
                    			if(forcelog || GeneralParams.PEERSCHECKER_lOG) {
                    				MinimaLogger.log("[-] Can't check peer as we have no block data");
                    			}
                            }
                    	}else {
                    		if(forcelog || GeneralParams.PEERSCHECKER_lOG) {
                    			MinimaLogger.log("PEERS CHECKER no block data @ "+address.toString());
                    		}
                    		
                    		//Wrong chain.. !
            				validversion = false;
                    	}
                    	
                    	if(validversion) {
                    		if(forcelog) {
                    			MinimaLogger.log("FORCECHECK - VALID CHAIN "+address.toString());
                    		}
                    	}
                    }
                }else {
                	if(forcelog) {
            			MinimaLogger.log("NULL greeting from Peer "+address.toString());
            		}
                }
                
                //What to do now..
                if (validversion) {
                    unverifiedPeers.remove(address);
                    
                    //Are we at capacity
                    if (verifiedPeers.size() >= MAX_VERIFIED_PEERS) {
                    	InetSocketAddress removed =  removeRandomItem(verifiedPeers);
                    	
                    	//Remove from our list
                    	if(removed != null) {
	                    	Message msg = new Message(P2PManager.P2P_REMOVE_PEER).addObject("address", removed);
	                        p2PManager.PostMessage(msg);
                    	}
                    }

                    //Add to our List (only one allowed)
                    verifiedPeers.remove(address);
                    verifiedPeers.add(address);
                    Message msg = new Message(P2PManager.P2P_ADD_PEER).addObject("address", address);
                    p2PManager.PostMessage(msg);
                    
                } else {
                	if (verifiedPeers.contains(address)) {
                        verifiedPeers.remove(address);
                        if (verifiedPeers.size() == 0) {
                            P2PFunctions.log_node_runner("[-] All addresses removed from verified peers list - Check node has internet connection");
                        }
                        unverifiedPeers.add(address);

                        // Check the peer is still down in 30 mins time
                        TimerMessage msg = new TimerMessage(1000 * 60 * 30, PEERS_CHECKPEERS);
                        msg.addObject("address", address);
                        PostTimerMessage(msg);
                    } else {
                        unverifiedPeers.remove(address);
                    }
                    
                    Message msg = new Message(P2PManager.P2P_REMOVE_PEER).addObject("address", address);
                    p2PManager.PostMessage(msg);
                }
                
            } else {
            	
//              	P2PFunctions.log_debug("[!] P2P not connected to internet - try again in 60 seconds");
                
            	//Check again in 60 seconds
                TimerMessage tmsg = new TimerMessage(60_000, PEERS_CHECKPEERS);
                tmsg.addObject("address", address);
                PostTimerMessage(tmsg);
            }

        } else if (zMessage.getMessageType().equals(PEERS_LOOP)) {
        	
        	//Check we have a net connection
        	if(P2PFunctions.isNetAvailable()) {
        		// Check all the verified Peers again
                for (InetSocketAddress address : verifiedPeers) {
                    Message msg = new Message(PEERS_CHECKPEERS).addObject("address", address);
                    PostMessage(msg);
                }
        	}
        	
            //Do it again ..
            PostTimerMessage(new TimerMessage(PEERS_LOOP_TIMER, PEERS_LOOP));
        
        } else if (zMessage.getMessageType().equals(PEERS_FORCEFULLCHECK)) {
        	
        	//Check we have a net connection
        	if(P2PFunctions.isNetAvailable()) {
        		// Check all the verified Peers again
                for (InetSocketAddress address : verifiedPeers) {
                    Message msg = new Message(PEERS_CHECKPEERS)
                    		.addObject("address", address)
                    		.addBoolean("forcelog", true);
                    PostMessage(msg);
                }
        	}else {
        		MinimaLogger.log("No Network connection - cannot perform peers check");
        	}
        }

    }

    /**
     * Remove 1 random element from this set
     */
    private InetSocketAddress removeRandomItem(Set<InetSocketAddress> zSet) {
    	int size = zSet.size();
    	if(size == 0) {
    		return  null;
    	}
    	
    	int item = new Random().nextInt(size); // In real life, the Random object should be rather more shared than this
    	Object chosen = null; 
    	int i = 0;
    	for(Object obj : zSet){
    	    if (i == item) {
    	    	chosen = obj;
    	    	break;
    	    }
    	    i++;
    	}
    	
    	//And remove this..
    	if(chosen != null) {
    		zSet.remove(chosen);
    		return (InetSocketAddress) chosen;
    	}
    	
    	return null;
    }
    
}
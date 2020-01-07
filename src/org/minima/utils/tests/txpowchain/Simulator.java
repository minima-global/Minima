package org.minima.utils.tests.txpowchain;

import java.util.ArrayList;

import org.minima.utils.MinimaLogger;

/**
 * Simulate multiple users sending tx pow messages 
 * across a network and building a chain from that data.
 * Play with the time delay, since users will be mining 
 * for 10s to send a transaction, and propagation 
 * speeds of messages across the network.
 * 
 * 
 * @author spartacus
 *
 */
public class Simulator {
	
	/*
	 * How many users
	 */
	public static final int NUM_USERS 	 = 50;
	
	/**
	 * Desired Blocktime in secs
	 */
	public static final double BLOCKTIME = 100;
	
	/**
	 * How long should the test go on for in secs (simulated)
	 */
	public static final double DURATION = 100000;
	
	/**
	 * Message reliability - sometimes a message doesn't get through or is delayed..
	 * 1 = 100%
	 */
	public static final double TXN_RELIABILITY 		= 1.0;
	public static final double RELIABILITY_DOWNTIME = 20;
	
	//Run it..
	public static void main(String[] zArgs) {
	
		//The Users
		ArrayList<User> users = new ArrayList<>();
		
		for(int i=0;i<NUM_USERS;i++) {
			users.add(new User(i+1));
		}
		
		log("Starting simulation..\n");
		long timenow = System.currentTimeMillis();
		
		//Run each second of the simulation as a discrete unit of time
		int currentBlock= 0;
		int totalstales = 0;
		int totaltxn    = 0;
		for(double timesecs = 0;timesecs<DURATION;timesecs++) {
			
			//Each second each user is updated..
			for(User user : users) {
				int blockFound = user.Update(timesecs, currentBlock);
			
				if(blockFound == -1) {
					//Nothing..
					
				}else if(blockFound == 0) {
					//Just a TXN
					totaltxn++;
					
				}else {
					totaltxn++;
					
					//They found a block.. ! (is it the right one ?)
					if(blockFound <= currentBlock) {
//						log("ORPHAN!");
						totalstales++;
					}else {
						currentBlock = blockFound;
					}
				}
				
			}
			
			
		}
		
		long timediff = System.currentTimeMillis() - timenow;
		log("\nFinished simulation in "+timediff+" milli secs");
		
		double totblock = (DURATION / BLOCKTIME);
		
		log("\nTotal Blocks       : "+currentBlock+ " / "+ totblock);
		log("Total Transactions : "+totaltxn+ " / "+ (NUM_USERS * User.TXN_PER_SEC * DURATION));
		log("Total Stales       : "+totalstales+ " .... "+((totalstales/totblock)*100)+"%");
		
		
	}

	public static void log(String zLog) {
		MinimaLogger.log(""+zLog);
	}
}

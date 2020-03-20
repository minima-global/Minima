package org.minima.system.input.functions;

import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class minetrans extends CommandFunction{

	public minetrans() {
		super("minetrans");
		
		setHelp("(number of txns)", 
				"Mine a blank transaction","1 sec pause between multiple transactions. Useful when debugging and MINIMA_ZERO_DIFF_BLK set to true");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		int num = 1;
		if(zInput.length>1) {
			num = Integer.parseInt(zInput[1]);
		}
	
		//Create a blank transaction
		Message newtrans = getResponseMessage(ConsensusHandler.CONSENSUS_SENDTRANS)
				.addObject("transaction", new Transaction())
				.addObject("witness", new Witness());

		if(num>1) {
			newtrans = new Message(ConsensusHandler.CONSENSUS_SENDTRANS)
					.addObject("transaction", new Transaction())
					.addObject("witness", new Witness());
		}
		
		//Send that many transactions
		for(int i=0;i<num;i++) {
			//Send it to the miner..
			getMainHandler().getConsensusHandler().PostMessage(newtrans);
		
			if(i<num-1) {
				//Pause for a second
				Thread.sleep(1000);
			}
		}
		
//		if(zInput.length>1) {
//			boolean stress = false;
//			if(zInput.length>2) {
//				if(!zInput[2].equals("stress")) {
//					System.out.println("Incorrect function format.");	
//					return;
//				}
//				
//				//Must be stress..
//				stress = true;			
//			}
//			
//			if(zInput[1].equalsIgnoreCase("auto")) {
//				getMainHandler().setSimulator(true,-1,stress);
//				System.out.println("AUTO transaction mining ON stress:"+stress);
//			}else if(zInput[1].equalsIgnoreCase("off")) {
//				getMainHandler().setSimulator(false,0,stress);
//				System.out.println("AUTO transaction mining OFF stress:"+stress);
//			}else {
//				int count = Integer.parseInt(zInput[1]);
//				getMainHandler().setSimulator(true,count,stress);
//				
//				System.out.println(count+" transactions added to mining stack..");
//			}
//			
//		}else {
//			getMainHandler().setSimulator(true,1,false);
//			System.out.println("1 off-chain transaction added to mining stack..");
//		}
		
		
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new minetrans();
	}
}

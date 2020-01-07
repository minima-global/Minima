package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;

public class minetrans extends CommandFunction{

	public minetrans() {
		super("minetrans");
		
		setHelp("[number|auto|off] {stress}", 
				"Mine a transaction and only publish if a block is found. Or stress to mine a published transaction.", 
				"This function simulates a User sending a random transaction. "
				+ "If not stress then this will only publish if a block is found : an off-chain transaction. "
				+ "If you use auto it will fire a transaction every 200 millisecs.");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		if(zInput.length>1) {
			boolean stress = false;
			if(zInput.length>2) {
				if(!zInput[2].equals("stress")) {
					System.out.println("Incorrect function format.");	
					return;
				}
				
				//Must be stress..
				stress = true;			
			}
			
			if(zInput[1].equalsIgnoreCase("auto")) {
				getMainHandler().setSimulator(true,-1,stress);
				System.out.println("AUTO transaction mining ON stress:"+stress);
			}else if(zInput[1].equalsIgnoreCase("off")) {
				getMainHandler().setSimulator(false,0,stress);
				System.out.println("AUTO transaction mining OFF stress:"+stress);
			}else {
				int count = Integer.parseInt(zInput[1]);
				getMainHandler().setSimulator(true,count,stress);
				
				System.out.println(count+" transactions added to mining stack..");
			}
			
		}else {
			getMainHandler().setSimulator(true,1,false);
			System.out.println("1 off-chain transaction added to mining stack..");
		}
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new minetrans();
	}
}

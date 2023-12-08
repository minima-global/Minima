package org.minima.system.network.minima;

import java.util.ArrayList;

import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.TxPoW;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;

public class RelayPolicy {

	/**
	 * Do we exceed the maximium number of coin outputs
	 */
	public static boolean checkMaxCoinOutputNumber(TxPoW zTxPow) {
		
		//Check Total Outputs..
		int outsize = zTxPow.getTransaction().getAllOutputs().size();
		if(outsize>GeneralParams.MAX_RELAY_OUTPUTCOINS) {
			MinimaLogger.log("MAX Coin outpouts in Transaction @ "+zTxPow.getTxPoWID());
			return false;
		}
		
		outsize = zTxPow.getBurnTransaction().getAllOutputs().size();
		if(outsize>GeneralParams.MAX_RELAY_OUTPUTCOINS) {
			MinimaLogger.log("MAX Coin outpouts in Burn Transaction @ "+zTxPow.getTxPoWID());
			return false;
		}
		
		return true;
	}

	public static boolean checkMaxStateStoreSize(TxPoW zTxPow, long zMaxSize) {
		
		//How many outputs store the state
		int counter=0;
		
		//How many output coins store the state
		ArrayList<Coin> outputs = zTxPow.getTransaction().getAllOutputs();
		for(Coin out : outputs) {
			if(out.storeState()) {
				counter++;
			}
		}
		
		//And for the burn
		outputs = zTxPow.getBurnTransaction().getAllOutputs();
		for(Coin out : outputs) {
			if(out.storeState()) {
				counter++;
			}
		}
		
		//Get the size of the state
		long statesize = zTxPow.getTransaction().calculateStateSize();
		
		//Multiply by number of coins that store the state..
		long total = statesize * counter;
		
		//Now check..
		if(total > zMaxSize) {
			MinimaLogger.log("TXPoW exceeds maximum state store for relay policy "
					+ "coins:"+counter+" statesize:"+statesize
					+" for "+total+" / "+GeneralParams.MAX_RELAY_STORESTATESIZE);
			
			return false;
		}
		
		return true;
	}
	
	/**
	 * Check All the relay policies..
	 */
	public static boolean checkAllPolicies(TxPoW zTxPow) {
		
		if(!checkMaxCoinOutputNumber(zTxPow)) {
			return false;
		}
		
		if(!checkMaxStateStoreSize(zTxPow,GeneralParams.MAX_RELAY_STORESTATESIZE)) {
			return false;
		}
		
		return true;
	}

}

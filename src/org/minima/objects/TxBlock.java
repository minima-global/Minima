package org.minima.objects;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMREntryNumber;
import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class TxBlock implements Streamable {

	/**
	 * The main TxPoW block
	 */
	TxPoW mTxPoW;

	/**
	 * The MMR Peaks from the previous block
	 */
	ArrayList<MMREntry> mPreviousPeaks = new ArrayList<>();
	
	/**
	 * The Proofs of all the input-spent coins - unspent as of the last block
	 */
	ArrayList<CoinProof> mSpentCoins = new ArrayList<>();
	
	/**
	 * A list of all the newly created coins
	 */
	ArrayList<Coin> mNewCoins = new ArrayList<>();
	
	private TxBlock() {}
	
	//For Tests
	public TxBlock(TxPoW zTxPoW) {
		//Main Block
		mTxPoW = zTxPoW;
	}
	
	public TxBlock(MMR zParentMMR, TxPoW zTxPoW, ArrayList<TxPoW> zAllTrans) {
		//Main Block
		mTxPoW = zTxPoW;
		
		//Get the Previous Peaks..
		mPreviousPeaks = zParentMMR.getPeaks();
		
		//Make a new child MMR that you can play with..
		MMR copymmr = new MMR(zParentMMR);
		
		//Cycle through the Main Block TxPoW
		calculateCoins(copymmr,zTxPoW);
	
		//Now cycle through the txns in the block MUST BE THE CORRECT ORDER for MMR root
		ArrayList<MiniData> txns = zTxPoW.getBlockTransactions();
		for(MiniData txn : txns) {
			
			//Get the correct txpow - the order could be wrong in ther function param
			TxPoW txp = getTxpoWFromList(txn, zAllTrans);
			if(txp == null) {
				throw new IllegalArgumentException("SERIOUS ERROR : TxBlock creation with missing txns.. "+zTxPoW.getTxPoWID()+" missing "+txn); 
			}
			
			//And now process that
			calculateCoins(copymmr,txp);
		}
	}
	
	/**
	 * Get the transactions in the correct order
	 */
	private TxPoW getTxpoWFromList(MiniData zTxPoWID, ArrayList<TxPoW> zAllTrans) {
		for(TxPoW txp : zAllTrans) {
			if(txp.getTxPoWIDData().isEqual(zTxPoWID)) {
				return txp;
			}
		}
		
		return null;
	}
	
//	private void calculateCoins(MMR zPreviousMMR, TxPoW zTxPoW) {
//		
//		//Needs to be a transaction
//		if(zTxPoW.isTransaction()) {
//			
//			//Get all the input coins
//			ArrayList<CoinProof> coinspent = zTxPoW.getWitness().getAllCoinProofs();
//			
//			//And now get all the proofs pointing to the previous block
//			for(CoinProof csp : coinspent) {
//				//Get the Coin
//				Coin coin = csp.getCoin();
//				
//				//Get the ENTRY NUmber..
//				MMREntryNumber entry = coin.getMMREntryNumber();
//			
//				//Add this to the MMR - so we can get a proof..
//				zPreviousMMR.updateEntry(entry, csp.getMMRProof(), csp.getMMRData());
//				
//				//The Proof - from the previous block
//				MMRProof proof = zPreviousMMR.getProofToPeak(entry);
//			
//				//Construct the CoinProof
//				CoinProof cp = new CoinProof(coin, proof);
//				
//				//Add to the list..
//				mSpentCoins.add(cp);
//			}
//			
//			//The state of this Txn
//			ArrayList<StateVariable> txnstate = zTxPoW.getTransaction().getCompleteState();
//			
//			//Create the state all outputs keep..
//			ArrayList<StateVariable> newstate = new ArrayList<>();
//			for(StateVariable sv : txnstate) {
//				if(sv.isKeepMMR()) {
//					newstate.add(sv);
//				}
//			}
//			
//			//All the new coins
//			ArrayList<Coin> outputs = zTxPoW.getTransaction().getAllOutputs();
//			int num=0;
//			for(Coin newoutput : outputs) {
//				
//				//Set the correct state variables
//				if(newoutput.storeState()) {
//					newoutput.setState(newstate);
//				}
//				
//				//Calculate the Correct CoinID for this coin.. TransactionID already calculated
//				MiniData coinid = zTxPoW.getTransaction().calculateCoinID(num);
//				
//				//Create a new coin with correct coinid
//				Coin correctcoin = newoutput.getSameCoinWithCoinID(coinid);
//				
//				//Is this a create token output..
//				if(newoutput.getTokenID().isEqual(Token.TOKENID_CREATE)) {
//					
//					//Get the Create token details..
//					Token creator = newoutput.getToken();
//					
//					//Get the details..
//					Token newtoken = new Token(	coinid, 
//												creator.getScale(), 
//												newoutput.getAmount(), 
//												creator.getName(),
//												creator.getTokenScript() ); 
//					
//					//Set it..
//					correctcoin.resetTokenID(newtoken.getTokenID());
//					
//					//And set that as the token..
//					correctcoin.setToken(newtoken);
//				}
//				
//				//Add to our list
//				mNewCoins.add(correctcoin);
//				
//				//Next coin down
//				num++;
//			}
//		}
//	}
	
	/**
	 * Calculate the MMR for both the main and burn transactions
	 */
	private void calculateCoins(MMR zPreviousMMR, TxPoW zTxPoW) {
		//First check the main transaction
		calculateCoins(zPreviousMMR, zTxPoW.getTransaction(), zTxPoW.getWitness());
		
		//And now the Burn Transaction
		calculateCoins(zPreviousMMR, zTxPoW.getBurnTransaction(), zTxPoW.getBurnWitness());
	}
	
	private void calculateCoins(MMR zPreviousMMR, Transaction zTransaction, Witness zWitness) {
			
		//Get all the input coins
		ArrayList<CoinProof> coinspent = zWitness.getAllCoinProofs();
		
		//And now get all the proofs pointing to the previous block
		for(CoinProof csp : coinspent) {
			//Get the Coin
			Coin coin = csp.getCoin();
			
			//Get the ENTRY NUmber..
			MMREntryNumber entry = coin.getMMREntryNumber();
		
			//Add this to the MMR - so we can get a proof..
			zPreviousMMR.updateEntry(entry, csp.getMMRProof(), csp.getMMRData());
			
			//The Proof - from the previous block
			MMRProof proof = zPreviousMMR.getProofToPeak(entry);
		
			//Construct the CoinProof
			CoinProof cp = new CoinProof(coin, proof);
			
			//Add to the list..
			mSpentCoins.add(cp);
		}
		
		//The state of this Txn
		ArrayList<StateVariable> txnstate = zTransaction.getCompleteState();
		
		//Create the state all outputs keep..
		ArrayList<StateVariable> newstate = new ArrayList<>();
		for(StateVariable sv : txnstate) {
			if(sv.isKeepMMR()) {
				newstate.add(sv);
			}
		}
		
		//All the new coins
		ArrayList<Coin> outputs = zTransaction.getAllOutputs();
		int num=0;
		for(Coin newoutput : outputs) {
			
			//Set the correct state variables
			if(newoutput.storeState()) {
				newoutput.setState(newstate);
			}
			
			//Calculate the Correct CoinID for this coin.. TransactionID already calculated
			MiniData coinid = zTransaction.calculateCoinID(num);
			
			//Create a new coin with correct coinid
			Coin correctcoin = newoutput.getSameCoinWithCoinID(coinid);
			
			//Is this a create token output..
			if(newoutput.getTokenID().isEqual(Token.TOKENID_CREATE)) {
				
				//Get the Create token details..
				Token creator = newoutput.getToken();
				
				//Get the details..
				Token newtoken = new Token(	coinid, 
											creator.getScale(), 
											newoutput.getAmount(), 
											creator.getName(),
											creator.getTokenScript() ); 
				
				//Set it..
				correctcoin.resetTokenID(newtoken.getTokenID());
				
				//And set that as the token..
				correctcoin.setToken(newtoken);
			}
			
			//Add to our list
			mNewCoins.add(correctcoin);
			
			//Next coin down
			num++;
		}
	}
	
	public TxPoW getTxPoW() {
		return mTxPoW;
	}
	
	public ArrayList<MMREntry> getPreviousPeaks(){
		return mPreviousPeaks;
	}
	
	public ArrayList<CoinProof> getInputCoinProofs(){
		return mSpentCoins;
	}
	
	public ArrayList<Coin> getOutputCoins(){
		return mNewCoins;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mTxPoW.writeDataStream(zOut);
		
		MiniNumber.WriteToStream(zOut, mPreviousPeaks.size());
		for(MMREntry entry : mPreviousPeaks) {
			entry.writeDataStream(zOut);
		}
		
		MiniNumber.WriteToStream(zOut, mSpentCoins.size());
		for(CoinProof cp : mSpentCoins) {
			cp.writeDataStream(zOut);
		}
		
		MiniNumber.WriteToStream(zOut, mNewCoins.size());
		for(Coin cc : mNewCoins) {
			cc.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mPreviousPeaks 	= new ArrayList<>();
		mSpentCoins		= new ArrayList<>();
		mNewCoins		= new ArrayList<>();
		
		mTxPoW 			= TxPoW.ReadFromStream(zIn);
		
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mPreviousPeaks.add(MMREntry.ReadFromStream(zIn));
		}
		
		len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mSpentCoins.add(CoinProof.ReadFromStream(zIn));
		}
		
		len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mNewCoins.add(Coin.ReadFromStream(zIn));
		}
	}
	
	public static TxBlock ReadFromStream(DataInputStream zIn) throws IOException {
		TxBlock sb = new TxBlock();
		sb.readDataStream(zIn);
		return sb;
	}
	
	/**
	 * Convert a MiniData version into a TxBlock
	 */
	public static TxBlock convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		TxBlock sync = null;
		
		try {
			//Convert data into a TxPoW
			sync = TxBlock.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return sync;
	}
}

package org.minima.system.network.maxima.message;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;

import org.minima.database.MinimaDB;
import org.minima.objects.Magic;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWMiner;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;

public class MaxTxPoW implements Streamable {

	/**
	 * The version
	 */
	MiniString mVersion = new MiniString("1.0");
	
	/**
	 * The Maxima Message
	 */
	MaximaPackage mMaxima;
	
	/**
	 * The Payment - A mined TxPoW with the MaximaPackage hash as the PRNG 
	 */
	TxPoW mTxPoW;
	
	private MaxTxPoW() {}
	
	public MaxTxPoW(MaximaPackage zMaxima, TxPoW zTxPoW) {
		mMaxima = zMaxima;
		mTxPoW 	= zTxPoW;
	}

	public MiniString getVersion() {
		return mVersion;
	}
		
	public MaximaPackage getMaximaPackage() {
		return mMaxima;
	}
	
	public TxPoW getTxPoW() {
		return mTxPoW;
	}
	
	public boolean checkValidTxPoW() {
		
		//What is the hash of the MaximaPackage
		MiniData msghash = Crypto.getInstance().hashObject(mMaxima);
		
		//Check the custom hash of the TxPoW..
		return mTxPoW.getCustomHash().isEqual(msghash);
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mVersion.writeDataStream(zOut);
		mMaxima.writeDataStream(zOut);
		mTxPoW.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mVersion = MiniString.ReadFromStream(zIn);
		mMaxima  = MaximaPackage.ReadFromStream(zIn);
		mTxPoW	 = TxPoW.ReadFromStream(zIn);
	}
	
	public static MaxTxPoW ReadFromStream(DataInputStream zIn) throws IOException {
		MaxTxPoW mp = new MaxTxPoW();
		mp.readDataStream(zIn);
		return mp;
	}
	
	public static MaxTxPoW createMaxTxPoW(MaximaPackage zMaxima) {		
	
		//What is the hash of this message
		MiniData msghash = Crypto.getInstance().hashObject(zMaxima);
				
		//Create a TXPOW unit around this package
		TxPoW txpow = TxPoWGenerator.generateTxPoW(new Transaction(), new Witness());
		
		//Now set the custom hash
		txpow.setCustomHash(msghash);
		
		//Get the Minimum allowed work..
		Magic magic = MinimaDB.getDB().getTxPoWTree().getTip().getTxPoW().getMagic();
		
		//Min Difficulty
		MiniData minwork = magic.getMinTxPowWork();
		
		//Add 10%.. to give yourself some space
		BigDecimal hashes 	= minwork.getDataValueDecimal();
		hashes 				= hashes.divide(new BigDecimal("1.1"), MathContext.DECIMAL64);
		MiniData minhash 	= new MiniData(hashes.toBigInteger());
		txpow.setTxDifficulty(minhash);
		
		//Now Mine it..
		Main.getInstance().getTxPoWMiner().MineTxPoW(txpow);
		
		//Now create a MaxTxPoW complete unit
		return new MaxTxPoW(zMaxima, txpow);
	}
	
}

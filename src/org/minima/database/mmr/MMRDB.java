package org.minima.database.mmr;

import java.util.ArrayList;

import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;

public class MMRDB {
	
	/**
	 * The Very bottom MMRset - this is where the proofs you want to keep are stored
	 */
	MMRSet mBaseSet;
	
	/**
	 * The Parent of the BottomSet
	 */
	MMRSet mRootSet;
	
	public MMRDB() {
		mBaseSet = new MMRSet();
	}
	
	public void initSets() {
		//All the proofs that need to be kept and updated..
		mBaseSet = new MMRSet();
		
		//No Parent..
		mRootSet = new MMRSet();
	}
	
	public MMRSet getCurrentMMR() {
		return mRootSet;
	}
	
	public static MMRSet mSet1 = new MMRSet();
	public static MMRSet mSet2 = new MMRSet();
	
	public static MMREntry mLast1;
	public static MMREntry mLast2;
	
	public static void addUnspent(MMRData zData) {
		mLast1 = mSet1.addUnspentCoin(zData);
		mLast2 = mSet2.addUnspentCoin(zData);
	}
	
	public static void spendCoin(MMRProof zProof) {
		mLast1 = mSet1.updateSpentCoin(zProof);
		mLast2 = mSet2.updateSpentCoin(zProof);
	}
	
	public static void newBlock() {
		mSet1 = new MMRSet(mSet1);
		mSet2 = new MMRSet(mSet2);
	}
	
//	public static void cascadeKeepers() {
//		mSet1.cascadeKeeperBlock();
//		mSet2.cascadeKeeperBlock();
//	}
	
	public static void printSets() {
		System.out.println();
		
		System.out.println("SET 1");
		MMRPrint.Print(mSet1);
		
		System.out.println("SET 2");
		MMRPrint.Print(mSet2);
	}
	
	public static MMRData getRandomCoin() {
		//New coin
		MiniData32 coin = new MiniData32(MiniData.getRandomData(32).getData());
		MiniData32 address  = new MiniData32("0xABBA");
		MiniNumber amount   = new MiniNumber("100");
		MiniData32 tokenid  = new MiniData32("0x00");
		
		Coin cc = new Coin( coin, address, amount, tokenid);
		
		return new MMRData(MiniByte.FALSE, cc, MiniNumber.ZERO, new ArrayList<StateVariable>()); 
	}
	
	public static void main(String[] zArgs) {
		
		MMREntry entry    = null;
		MMRProof genproof = null;
		MMRSet set        = new MMRSet();
		
		//Prime
		MMREntry prime    = set.addUnspentCoin(getRandomCoin());
		MMRPrint.Print(set);
		
		//Start
		set     = new MMRSet(set);
		
		entry    = set.addUnspentCoin(getRandomCoin());
		MiniNumber spec = entry.getEntry();
		genproof = set.getProof(spec);
		MMRPrint.Print(set,false);
		System.out.println("Proof #1 : "+genproof+" "+set.checkProof(genproof));
		
		entry    = set.addUnspentCoin(getRandomCoin());
		entry    = set.addUnspentCoin(getRandomCoin());
		entry    = set.addUnspentCoin(getRandomCoin());
		entry    = set.addUnspentCoin(getRandomCoin());
		entry    = set.addUnspentCoin(getRandomCoin());
		entry    = set.addUnspentCoin(getRandomCoin());
		entry    = set.addUnspentCoin(getRandomCoin());
		entry    = set.addUnspentCoin(getRandomCoin());
		entry    = set.addUnspentCoin(getRandomCoin());
		
//		genproof = set.getProof(entry.getEntry());
		genproof = set.getProof(spec);
		MMRPrint.Print(set,false);
		System.out.println("Proof #2 : "+genproof+" "+set.checkProof(genproof));
		
		entry    = set.updateSpentCoin(genproof);
		genproof = set.getProof(entry.getEntry());
		MMRPrint.Print(set,false);
		System.out.println("Proof #3 : "+genproof+" "+set.checkProof(genproof));
		
		if(true) {
			System.exit(0);
		}
		
		set = new MMRSet(set);
		
		//Basic..
//		MMRPrint.Print(set);
		
		entry = set.addUnspentCoin(getRandomCoin());
		
		genproof = set.getProof(entry.getEntry());
//		System.out.println("Proof : "+genproof);
		
		set = new MMRSet(set);
//		MMRPrint.Print(set);
		
		set.addUnspentCoin(getRandomCoin());
		set.addUnspentCoin(getRandomCoin());
		
		set.addUnspentCoin(getRandomCoin());
		set.addUnspentCoin(getRandomCoin());
		
		set = new MMRSet(set);
//		MMRPrint.Print(set);
		
		set = new MMRSet(set);
	
//		System.out.println("Proof : "+genproof+" check:"+set.checkProof(genproof));
		
		//This broken..
		set.updateSpentCoin(genproof);
		
//		MMRPrint.Print(set);

		genproof = set.getProof(entry.getEntry());
//		System.out.println("Proof to peak : "+genproof);
		
		set.addUnspentCoin(getRandomCoin());
		set.addUnspentCoin(getRandomCoin());
		
		MMRPrint.Print(set);
		
		set = new MMRSet(set);
		
//		genproof = set.getProof(entry.getEntry());
//		System.out.println("Proof    : "+genproof);

		entry = set.addUnspentCoin(getRandomCoin());
		genproof = set.getProof(entry.getEntry());
		System.out.println("Proof #1 : "+genproof);
		MMRPrint.Print(set,false);
		
		entry = set.updateSpentCoin(genproof);
		genproof = set.getProof(entry.getEntry());
		System.out.println("Proof #2 : "+genproof);
		
		
		
//		//Now spend..
//		set.updateSpentCoin(genproof);
//		set.addUnspentCoin(getRandomCoin());
//		set = new MMRSet(set);
//		
//		MMRPrint.Print(set);
//		genproof = set.getFullProofToRoot(entry.getEntry());
//		System.out.println("Proof : "+genproof);
//
//		set.addUnspentCoin(getRandomCoin());
//		set.addUnspentCoin(getRandomCoin());
//		set.addUnspentCoin(getRandomCoin());
//		set.addUnspentCoin(getRandomCoin());
//		set = new MMRSet(set);
//		
//		MMRPrint.Print(set);
//		genproof = set.getFullProofToRoot(entry.getEntry());
//		System.out.println("Proof : "+genproof);
		
	}
}

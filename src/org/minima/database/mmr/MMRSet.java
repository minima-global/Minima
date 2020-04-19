package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.objects.Coin;
import org.minima.objects.base.MMRSumNumber;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.Proof.ProofChunk;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class MMRSet implements Streamable {
	
	/**
	 * What Block time does this MMR represent. Each represents 1 block.
	 */
	MiniNumber mBlockTime = new MiniNumber(0);
	
	/**
	 * The parent MMRData..
	 * 
	 * If you don't have it, ask your parent
	 */
	MMRSet mParent = null;
	
	/**
	 * What is the current entry number..
	 */
	public MiniInteger mEntryNumber = new MiniInteger(0);
	
	/**
	 * All the entries in this set 
	 */
	ArrayList<MMREntry> mEntries;

	/**
	 * The maximum row used in this Set
	 */
	int mMaxRow = 0;
	
	/**
	 * The Max entries per row..
	 */
	MMREntry mMaxEntries[];
	
	/**
	 * Which Entries do we keep
	 */
	ArrayList<MiniInteger> mKeepers;
	
	/**
	 * Has the set been Finalized (Peaks / Root )
	 * No more changes after this.
	 */
	boolean mFinalized;
	
	MMRData mFinalizedRoot;
	ArrayList<MMREntry> mFinalizedPeaks;
	ArrayList<MMREntry> mFinalizedZeroRow;
	
	//HASH Function bit length..
	int MMR_HASH_BITS=512;
	
	/**
	 * Main Constructor
	 */
	public MMRSet() {
		this(null, 512);
	}
	
	public MMRSet(int zBitLength) {
		this(null, zBitLength);
	}
	
	public MMRSet(MMRSet zParent) {
		this(zParent, 512);
	}
	
	public MMRSet(MMRSet zParent, int zBitLength) {
		//All the Entries in this set
		mEntries    = new ArrayList<>();
		
		//The Maximum Rows and entries
		mMaxEntries = new MMREntry[256];
		mMaxRow     = 0;
		
		//Parent MMRSet
		mParent = zParent;
	
		//A list of entries to keep track of
		mKeepers = new ArrayList<>();
		
		//Not Finalized..
		mFinalized = false;
		
		//What HASH strength - ALL MMR database is 512
		MMR_HASH_BITS = zBitLength;
		
		//Now add the peaks..
		if(mParent != null) {
			if(!mParent.isFinalized()) {
				//Finalize the parent..
				mParent.finalizeSet();
			}
			
			//Set the Time.. 1 more than parent
			setBlockTime(mParent.getBlockTime().add(MiniNumber.ONE));
			
			//Calculate total entries..
			BigInteger tot = BigInteger.ZERO;
			BigInteger two = new BigInteger("2");
			
			ArrayList<MMREntry> peaks = mParent.getMMRPeaks(); 
			for(MMREntry peak : peaks) {
				//Add the peak
				setEntry(peak.getRow(), peak.getEntry(), peak.getData());
			
				//Add to the total entries.. the peaks are the binary value
				tot = tot.add(two.pow(peak.getRow()));
			}
			
			//Set the Entry Number
			mEntryNumber = new MiniInteger(tot);
			
			//Check!
			if(!mEntryNumber.isEqual(mParent.mEntryNumber)) {
				MinimaLogger.log("SERIOUS ERROR - Entry Number Mismatch! "+mEntryNumber+"/"+mParent.mEntryNumber);
			}
		}
	}
	
	public void setParent(MMRSet zParent) {
		mParent = zParent;
	}
	
	public void finalizeSet() {
		//Reset
		mFinalized = false;
				
		//The peaks..
		mFinalizedPeaks = getMMRPeaks();
		
		//Create the final values..
		mFinalizedRoot = getMMRRoot();
		
		//get the zero row
		mFinalizedZeroRow = getRow(0);
		
		//We are now Finalized..
		mFinalized = true;
	}
	
	public boolean isFinalized() {
		return mFinalized;
	}
	
	private void setBlockTime(MiniNumber zTime) {
		mBlockTime = zTime;
	}
	
	public MiniNumber getBlockTime() {
		return mBlockTime;
	}
	
	public MMRSet getParent() {
		return mParent;
	}
	
	private void incrementEntryNumber() {
		mEntryNumber = mEntryNumber.increment();
	}
	
	public int getMaxRow() {
		return getMaxRow(false);
	}
	
	public int getMaxRow(boolean zCheckParent) {
		int max = mMaxRow;
		
		if(zCheckParent && mParent!=null) {
			int pmax = mParent.getMaxRow(zCheckParent);
			if(pmax>max) {
				max = pmax;
			}
		}
		
		return max;
	}
	
	public ArrayList<MMREntry> getRow(int zRow){
		ArrayList<MMREntry> row = new ArrayList<>();
		
		for(MMREntry entry : mEntries) {
			if(entry.getRow() == zRow) {
				row.add(entry);
			}
		}
		
		return row;
	}
	
	public ArrayList<MMREntry> getZeroRow(){
		if(mFinalized) {
			return mFinalizedZeroRow;
		}else {
			return getRow(0);
		}
	}
	
	/**
	 * Search for the first valid unspent Address and Tokenid with AT LEAST Amount coin
	 * @param zCoinID
	 * @return
	 */
	public MMREntry searchAddress(MiniData zAddress, MiniNumber zAmount, MiniData zTokenID) {
		//Get the zero row - no parents..
		ArrayList<MMREntry> zero=getZeroRow();
		
		for(MMREntry entry : zero) {
			if(!entry.getData().isHashOnly()) {
				Coin cc = entry.getData().getCoin();
				
				boolean notspent  = !entry.getData().isSpent();
				boolean addr      = cc.getAddress().isEqual(zAddress);
				boolean amount    = cc.getAmount().isMoreEqual(zAmount);
				boolean tok       = cc.getTokenID().isEqual(zTokenID);
				
				if(addr && amount && tok && notspent){
					return entry;
				}
			}
		}
			
		//Cycle up the parents.. 
		if(mParent!=null) {
			return mParent.searchAddress(zAddress, zAmount, zTokenID);
		}
		
		return null;
	}
	
	/**
	 * Find an entry
	 * @param zCoinID
	 * @return
	 */
	public MMREntry findEntry(MiniData zCoinID) {
		//Get the zero row - no parents..
		ArrayList<MMREntry> zero=getZeroRow();
		
		for(MMREntry entry : zero) {
			if(!entry.getData().isHashOnly()) {
				if(entry.getData().getCoin().getCoinID().isEqual(zCoinID)){
					return entry;
				}
			}
		}
			
		if(mParent!=null) {
			return mParent.findEntry(zCoinID);
		}
	
		return null;
	}
	
	/**
	 * Sets the Entry value in THIS SET ONLY. Does not affect parents.
	 * @param zRow
	 * @param zEntry
	 * @param zData
	 * @return
	 */
	private MMREntry setEntry(int zRow, MiniInteger zEntry, MMRData zData) {
		//Store the Maximum
		if(zRow>mMaxRow) {
			mMaxRow = zRow;
		}
		
		//Check if already added..
		MMREntry entry = null;
		for(MMREntry ent : mEntries) {
			if(ent.checkPosition(zRow, zEntry)) {
				entry = ent;
				break;
			}
		}
		
		//Create and add if not found
		if(entry == null) {
			entry = new MMREntry(zRow, zEntry);
			entry.setBlockTime(getBlockTime());
			mEntries.add(entry);
		}
		
		//Set the correct data
		entry.setData(zData);
		
		//Is it a MAX
		if(mMaxEntries[zRow] == null) {
			mMaxEntries[zRow] = entry;
		}else if(mMaxEntries[zRow].getEntry().isLess(zEntry)) {
			mMaxEntries[zRow] = entry;
		}
		
		//Return
		return entry;
	}
	
	private MMREntry getEntry(int zRow, MiniInteger zEntry) {
		//Check if already added..
		for(MMREntry ent : mEntries) {
			if(ent.checkPosition(zRow, zEntry)) {
				return ent;
			}
		}
		
		//Check the parent Set
		if(mParent!=null) {
			MMREntry entry = mParent.getEntry(zRow, zEntry);
			if(!entry.isEmpty()) {
				return entry;
			}
		}
		
		//If all else fails.. return empty entry..
		MMREntry entry = new MMREntry(zRow, zEntry);
		entry.setBlockTime(getBlockTime());
		
		return entry;
	}
	
	/**
	 * Add data - an UNSPENT coin
	 */
	public MMREntry addUnspentCoin(MMRData zData) {
		//Create a new entry
		MMREntry entry = setEntry(0, mEntryNumber, zData);
		MMREntry ret   = entry;
		
		//1 more entry
		incrementEntryNumber();
		
		//Now go up the tree..
		while(entry.isRight()) {
			//Get the Sibling.. will be the left
			MMREntry sibling = getEntry(entry.getRow(), entry.getLeftSibling());
			
			//Create the new row - hash LEFT + RIGHT
			MiniData combined = Crypto.getInstance().hashObjects(sibling.getHashValue(), entry.getHashValue(), MMR_HASH_BITS);
			
			//Combine the Values..
			MMRSumNumber sumvalue = entry.getData().getValueSum().add(sibling.getData().getValueSum());
			
			//The New MMRData
			MMRData data = new MMRData(combined,sumvalue);
			
			//Set the Parent Entry
			entry = setEntry(entry.getParentRow(),entry.getParentEntry(),data);
		}
		
		return ret;
	}
	
	/**
	 * Add data - an UNSPENT coin - Must be added to the correct mmrset
	 */
	public MMREntry addExternalUnspentCoin(MMRProof zProof) {
		//The Details
		MiniInteger entrynum = zProof.getEntryNumber();
		MMRData proofdata    = zProof.getMMRData();
		
		//Do we already have this Entry..
		MMREntry entry = getEntry(0, entrynum);
		if(!entry.isEmpty() && !entry.getData().isHashOnly()) {
			//Make sure its a keeper
			addKeeper(entrynum);
			
			//We have it..
			return entry;
		}
		
		//Create a new entry
		entry = setEntry(0, entrynum, proofdata);
		MMREntry ret = entry;
		
		//Now go up the tree..
		int prooflen = zProof.getProofLen();
		int proofnum = 0;
		while(proofnum < prooflen) {
			MMREntry sibling = getEntry(entry.getRow(), entry.getSibling());
			
			//Do we add our own..
			ProofChunk chunk = zProof.getProofChunk(proofnum++);
			MMRData pdata = new MMRData(chunk.getHash(), chunk.getValue());
			if(sibling.isEmpty()) {
				//Set the data
				sibling = setEntry(sibling.getRow(), sibling.getEntry(), pdata);
				
			}else {
				//Check the value is what we expect it to be
				if(!sibling.getData().getFinalHash().isEqual(pdata.getFinalHash())) {
					//Hmm..
					System.out.println("Sibling Inconsistency!! in MMR @ "+entrynum+" when hard adding proof");
					
					return null;
				}else {
					//We have all this allready!
					break;
				}
			}
			
			//Create the new combined value..
			MiniData combined = null;
			if(entry.isLeft()) {
				combined = Crypto.getInstance().hashObjects(entry.getHashValue(),sibling.getHashValue(), MMR_HASH_BITS);
			}else {
				combined = Crypto.getInstance().hashObjects(sibling.getHashValue(), entry.getHashValue(), MMR_HASH_BITS);
			}
			
			//Combine the Values..
			MMRSumNumber sumvalue = entry.getData().getValueSum().add(sibling.getData().getValueSum());
			
			//CCreate a new data proof
			MMRData data = new MMRData(combined,sumvalue);
			
			//Check if we have it..
			MMREntry parent = getEntry(entry.getParentRow(),entry.getParentEntry());  
			if(!parent.isEmpty()) {
				if(!parent.getData().getFinalHash().isEqual(combined)) {
					//Hmm..
					System.out.println("Parent Inconsistency!! in MMR @ "+entrynum+" when hard adding proof");
					
					return null;
				}else {
					//We have this..!
					break;
				}
			}
			
			//Set the Parent Entry
			entry = setEntry(entry.getParentRow(),entry.getParentEntry(),data);
		}
		
		//Its a keeper..
		addKeeper(entrynum);
		
		return ret;
	}
	
	/**
	 * Set entry to SPENT
	 * 
	 * @param zProof
	 * @return
	 */
	public MMREntry updateSpentCoin(MMRProof zProof) {
		//The original MMRData..
		MMRData original = zProof.getMMRData();
		
		//The NEW spent MMRData..
		MMRData spentmmr = new MMRData(MiniByte.TRUE, 
										original.getCoin(),
										original.getInBlock(),
										original.getPrevState());
		
		//Get the current peaks..
		ArrayList<MMREntry> peaks=getMMRPeaks();
		
		//Create a new entry
		MMREntry entry = setEntry(0, zProof.getEntryNumber(), spentmmr);
		MMREntry ret   = entry;
		
		//Now update the tree - Get the Sibling.. 
		MMREntry sibling = getEntry(0, entry.getSibling());
		
		//Is this a peak..
		int prooflen = zProof.getProofLen();
		if(sibling.isEmpty() && prooflen==0) {
			return ret;
		}
		
		//Are there any proof chunks
		ProofChunk chunk  = null;
		MiniData phash    = null;
		MMRSumNumber pval = null;
		int pcount   = 0;
		if(prooflen>0) {
			chunk = zProof.getProofChunk(pcount++);
			phash = chunk.getHash();
			pval  = chunk.getValue();
			
			//Do we need to fill it in..
			if(sibling.isEmpty()) {
//				System.out.println("EMPTY SIBLING");
				sibling = setEntry(sibling.getRow(), sibling.getEntry(), new MMRData(phash, pval));
			}else if(sibling.getBlockTime().isLessEqual(zProof.getBlockTime())) {
				//Is it the original.. has all the micro details.. internal nodes are just the hash anyway
				MiniData orighash = sibling.getData().getFinalHash();
				if(!orighash.isEqual(phash)) {
					System.out.println("SIBLING DIFFERENT HASH");
					sibling = setEntry(sibling.getRow(), sibling.getEntry(), new MMRData(phash, pval));
				}
			}
			
			//Set the Sibling in this MMRSET!.. this way the MMR peaks still work.. (as the max in a row MUST be on the left to be a peak ))
			setEntry(sibling.getRow(), sibling.getEntry(),sibling.getData());
		}
		
		//Now go up the tree..
		while(!sibling.isEmpty()) {
			//Create the new parent
			MiniData combined = null;
			if(entry.isLeft()) {
				combined = Crypto.getInstance().hashObjects(entry.getHashValue(),sibling.getHashValue(), MMR_HASH_BITS);
			}else {
				combined = Crypto.getInstance().hashObjects(sibling.getHashValue(), entry.getHashValue(), MMR_HASH_BITS);
			}
			
			//Combine the Values..
			MMRSumNumber sumvalue = entry.getData().getValueSum().add(sibling.getData().getValueSum());
			
			//Create the new MMR Data
			MMRData data = new MMRData(combined,sumvalue);
			
			//Set the Sibling in this MMRSET!.. this way the MMR peaks still work.. (as the max in a row MUST be on the left to be a peak ))
			setEntry(sibling.getRow(), sibling.getEntry(),sibling.getData());
			
			//Set the Parent
			entry = setEntry(entry.getParentRow(), entry.getParentEntry(), data);
			
			//Is this is a Peak ? - if so, go no further..
			for(MMREntry peak : peaks) {
				if(entry.checkPosition(peak)) {
					return ret;
				}
			}
			
			//Get the Sibling..
			sibling = getEntry(entry.getRow(), entry.getSibling());
			
			//Check for a valid sibling
			if(pcount < prooflen) {
				chunk = zProof.getProofChunk(pcount++);
				phash = chunk.getHash();
				pval  = chunk.getValue();
				if(sibling.isEmpty()) {
//					System.out.println("EMPTY SIBLING 2");
					sibling = setEntry(sibling.getRow(), sibling.getEntry(), new MMRData(phash,pval));		
				}else if(sibling.getBlockTime().isLessEqual(zProof.getBlockTime())) {
					//Is it the original.. has all the micro details.. internal nodes are just the hash anyway
					MiniData orighash = sibling.getData().getFinalHash();
					if(!orighash.isEqual(phash)) {
						System.out.println("SIBLING DIFFERENT HASH 2");
						sibling = setEntry(sibling.getRow(), sibling.getEntry(), new MMRData(phash,pval));	
					}
				}
			}
		}
		
		return ret;
	}
	
	/**
	 * Get An MMR Proof
	 */
	public MMRProof getProof(MiniInteger zEntryNumber) {
		//First get the initial Entry.. check parents aswell..
		MMREntry entry = getEntry(0, zEntryNumber);
		
		//Now get all the hashes in the tree to a peak..
		MMRProof proof = new MMRProof(zEntryNumber, entry.getData(), mBlockTime);
		proof.setHashBitLength(MMR_HASH_BITS);
		
		//Go up to the MMR Peak..
		MMREntry sibling = getEntry(entry.getRow(), entry.getSibling());
		while(!sibling.isEmpty()) {
			//Add to our Proof..
			proof.addProofChunk(new MiniByte(sibling.isLeft()), sibling.getHashValue(), sibling.getData().getValueSum());	
			
			//Now get the Parent.. just need a reference even if is empty. To find the sibling.
			MMREntry parent = new MMREntry( sibling.getParentRow(), sibling.getParentEntry() );
			
			//And get the Sibling of the Parent..
			sibling = getEntry(parent.getRow(), parent.getSibling());
		}
		
		return proof;
	}
	
	/**
	 * Get Proof to ROOT
	 */
	private MMRProof getPeakToRoot(MiniData zPeak) {
		//Sum of all the initial proofs...
		MMRProof totalproof = new MMRProof();
		totalproof.setHashBitLength(MMR_HASH_BITS);
		
		//Get the Peaks..
		ArrayList<MMREntry> peaks = getMMRPeaks();
		
		//Now take all those values and put THEM in an MMR..
		MMREntry keeper      = null;
		MiniData keepvalue = zPeak;
		while(peaks.size() > 1) {
			//Create a new MMR
			MMRSet newmmr = new MMRSet(MMR_HASH_BITS);
			
			//Add all the peaks to it..
			for(MMREntry peak : peaks) {
				//Create the new input..
				MMRData data = new MMRData(peak.getHashValue(), peak.getData().getValueSum());
				
				//Is this the one to follow..
				if(peak.getHashValue().isEqual(keepvalue)) {
					keeper = newmmr.addUnspentCoin(data);	
				}else {
					newmmr.addUnspentCoin(data);	
				}	
			}
			
			//Now get the keeper proof..
			MMRProof proof = newmmr.getProof(keeper.getEntry());
			
			//Now add that to the total proof..
			int len = proof.getProofLen();
			for(int i=0;i<len;i++) {
				ProofChunk chunk = proof.getProofChunk(i);
				totalproof.addProofChunk(chunk.getLeft(), chunk.getHash(), chunk.getValue());
			}
			
			//Now get the peaks.. repeat..
			peaks = newmmr.getMMRPeaks();
		}
		
		return totalproof;
	}
	
	/**
	 * Get the full proof to the root of the MMR
	 * 
	 * @param zEntry
	 * @return
	 */
	public MMRProof getFullProofToRoot(MiniInteger zEntry) {
		//Get the Basic Proof..
		MMRProof proof = getProof(zEntry);
		
		//Now get the peak this points to..
		MiniData peak = proof.getFinalHash();
		
		//Now find the path to root for this peak
		MMRProof rootproof = getPeakToRoot(peak);
		
		//Now add the two..
		int len = rootproof.getProofLen();
		for(int i=0;i<len;i++) {
			ProofChunk chunk = rootproof.getProofChunk(i);
			proof.addProofChunk(chunk.getLeft(), chunk.getHash(), chunk.getValue());
		}
		
		return proof;
	}
	
	
	/**
	 * Check this is a valid UNSPENT output.
	 * 
	 * The Proof can point to a previous block. But must 
	 * be within the range that everyone store.. 
	 * 
	 * @return
	 */
	public boolean checkProof(MMRProof zProof) {
		//Hmm.. this is not good..
		if(zProof.getMMRData().isHashOnly()) {
			System.out.println("Invalid PROOF check HASHONLY! : "+zProof);
			return false;
		}
		
		//Check is not spent.. 
		if(zProof.getMMRData().isSpent()) {
			System.out.println("Invalid PROOF is SPENT! : "+zProof);
			return false;
		}
		
		//Get the MMRSet at the time this proof was made.. must be a recent proof..
		MMRSet proofset = getParentAtTime(zProof.getBlockTime());
		
		//The proof is it too old.. we can't check it. It's invalid.
		if(proofset == null) {
			System.out.println("ERROR Proof too Old "+zProof);
			return false;
		}
		
		//Check the proof point to the MMR ROOT..
		ArrayList<MMREntry> peaks = proofset.getMMRPeaks();
		
		//Calculate the proof..
		MiniData proofpeak = zProof.getFinalHash();
		
		//Is this is a Peak ? - if so, go no further..
		boolean found = false;
		MMRSumNumber peakvalue = null;
		for(MMREntry peak : peaks) {
			if(proofpeak.isEqual(peak.getHashValue())) {
				found     = true;
				peakvalue = peak.getData().getValueSum();
				break;
			}
		}
		
		//Was it one of the peaks ?
		if(!found) {
			System.out.println("ERROR Proof No Peak Found "+zProof);
			return false;
		}
		
		//So the proof was valid at that time.. if it has been SPENT, it will have been AFTER this block - and in our MMR
		MMREntry entry = getEntry(0, zProof.getEntryNumber());
		
		//Is it there ?
		if(!entry.isEmpty() && !entry.getData().isHashOnly()) {
			//Get the DATA - could be the original UNSPENT or the SPENT
			if(entry.getData().isSpent()) {
				System.out.println("ERROR Proof Spent! "+zProof);
				return false;
			}
		}
	
		//Check the SUMTREE - we've checked the HASH tree already..
		int proofnum        = 0;
		int prooflen        = zProof.getProofLen();
		MMRSumNumber totval = zProof.getMMRData().getValueSum();
		
		if(!entry.isEmpty()) {
			if(!totval.isEqual(entry.getData().getValueSum())) {
				System.out.println("ERROR MMR Sum Tree different "+totval+" "+entry.getData().getValueSum());
				return false;
			}
		}
		
		while(proofnum < prooflen) {
			MMREntry sibling = proofset.getEntry(entry.getRow(), entry.getSibling());
			
			//Do we add our own..
			ProofChunk chunk   = zProof.getProofChunk(proofnum++);
			MMRSumNumber value = chunk.getValue();
			if(!sibling.isEmpty()) {
				if(!value.isEqual(sibling.getData().getValueSum())) {
					System.out.println("ERROR 2 MMR Sum Tree different "+value+" "+sibling.getData().getValueSum());
					return false;
				}
			}
			
			//Create the new combined value..
			totval = totval.add(value);
			
			//Get the Parent - can be empty..
			entry = proofset.getEntry(entry.getParentRow(),entry.getParentEntry());
		}
		
		//Now check that value..
		if(!totval.isEqual(peakvalue)) {
			System.out.println("ERROR 3 MMR Sum Tree different "+totval+" "+peakvalue);
			return false;
		}
		
		//It was valid at the parent.. there is NO SPEND since.. so it's Valid!
		return true;
	}
	
	/**
	 * Get the MMR peaks of this Set
	 * @return
	 */
	public ArrayList<MMREntry> getMMRPeaks(){
		//Are we final 
		if(mFinalized) {
			return mFinalizedPeaks;
		}
		
		//Create from scratch
		ArrayList<MMREntry> peaks = new ArrayList<>();
		for(int i=mMaxRow;i>=0;i--) {
			//Get the MAX entry for the row..
			MMREntry max = mMaxEntries[i];
				
			//Is there an Entry..
			if(max != null) {
				//Is it a peak ? - ALL peaks are LEFT siblings..
				if(max.isLeft()) {
					peaks.add(max);
				}
			}
		}
			
		return peaks;
	}
	
	/**
	 * Get the ROOT of the Whole MMRSet
	 * 
	 * @return
	 */
	public MMRData getMMRRoot() {
		//Are we final
		if(mFinalized) {
			return mFinalizedRoot;
		}
		
		//Get the Peaks..
		ArrayList<MMREntry> peaks = getMMRPeaks();
		
		//Now take all those values and put THEM in an MMR..
		while(peaks.size() > 1) {
		
			//Create a new MMR
			MMRSet newmmr = new MMRSet(MMR_HASH_BITS);
			
			//Add all the peaks to it..
			for(MMREntry peak : peaks) {
				newmmr.addUnspentCoin(new MMRData(peak.getHashValue(), peak.getData().getValueSum()));	
			}
			
			//Now get the peaks.. repeat..
			peaks = newmmr.getMMRPeaks();
		}
		
		return peaks.get(0).getData();
	}
	
	/**
	 * Add a Keeper - once only..
	 * @param zEntry
	 */
	public boolean addKeeper(MiniInteger zEntry) {
		if(!isKeptAllready(zEntry)) {
			mKeepers.add(zEntry);
			return true;
		}
		
		return false;
	}
	
	/**
	 * Get the Keeper
	 */
	public ArrayList<MiniInteger> getKeepers() {
		return mKeepers;
	}
	
	/**
	 * Do we already keep this entry..
	 * 
	 * @param zNumber
	 * @return
	 */
	public boolean isKeptAllready(MiniInteger zNumber) {
		for(MiniInteger keep : mKeepers) {
			if(keep.isEqual(zNumber)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Used when Pruning the MMR tree..
	 * 
	 * All the Keepers are moved Up one level..
	 */
	public void copyParentKeepers() {
		//First get the Keepers..
		ArrayList<MiniInteger> parentkeepers = new ArrayList<>();
		if(mParent!=null) {
			parentkeepers = mParent.getKeepers();
		}
		
		//Cycle through the current crop..
		ArrayList<MiniInteger> newkeepers = new ArrayList<>();
		for(MiniInteger keep : mKeepers) {
			//Get that LATEST entry and all the entries it uses on the way up..
			MMREntry entry = getEntry(0, keep);
			if(!entry.getData().isSpent()) {
				newkeepers.add(keep);
			}
		}
		
		//Reset
		mKeepers = newkeepers;
		
		//Cycle through the Keepers..
		for(MiniInteger keep : parentkeepers) {
			//Get that LATEST entry and all the entries it uses on the way up..
			MMREntry entry = getEntry(0, keep);
			
			//Check valid.. SHOULD NOT HAPPEN
			if(entry.isEmpty() || entry.getData().isHashOnly()) {
				System.out.println("copyKeepers on NULL Keeper Entry! "+keep);
				continue;
			}
			
			//If it's spent we don't keep it..
			if(entry.getData().isSpent()) {
				continue;
			}
			
			//Keep it..
			boolean added = addKeeper(keep);
			
			//Has it already been added..
//			if(added) {
				//Add it.. to THIS set.. not the parent..
				entry = setEntry(0, keep, entry.getData());
				
				//And now go go up the tree..
				MMREntry sibling = getEntry(entry.getRow(), entry.getSibling());
				while(!sibling.isEmpty()) {
					//Add to our Set..
					setEntry(sibling.getRow(), sibling.getEntry(), sibling.getData());
					
					//Now get the Parent.. just need a reference even if is empty. To find the sibling.
					MMREntry parent = new MMREntry( sibling.getParentRow(), sibling.getParentEntry() );
					
					//And get the Sibling of the Parent..
					sibling = getEntry(parent.getRow(), parent.getSibling());
				}
//			}
		}
		
		//Now we have all the data stored for the keeper coins.. We can remove the parent..		
		mParent = null;
		
		//Re-finalise..
		finalizeSet();
	}
	
	/**
	 * Drill down and get the last but one parent..
	 * We will be pruning it.. 
	 * @return
	 */
	public MMRSet getRootParent() {
		if(mParent == null) {
			return this;
		}
		
		return mParent.getRootParent();
	}
	
	/**
	 * Get a Parent block at a certain time..
	 */
	public MMRSet getParentAtTime(MiniNumber zTime) {
		if(mBlockTime.isEqual(zTime)) {
			return this;
		}
		
		if(mParent != null) {
			return mParent.getParentAtTime(zTime);
		}
		
		return null;
	}
	
	public MMRSet getPenultimateParent() {
		if(mParent != null) {
			if(mParent.getParent() == null) {
				return this;
			}
			
			return mParent.getPenultimateParent();
		}
		
		return null;
	}
	
	public int getParentLength() {
		if(mParent != null) {
			return 1+mParent.getParentLength();	
		}
		
		return 1;
	}

	/**
	 * Write out this MMR set
	 * 
	 * When a user syncs to the network a section of these will bootstrap the MMR DB.
	 */
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Write the Block Time.
		mBlockTime.writeDataStream(zOut);
		
		//EntryNumber..
		mEntryNumber.writeDataStream(zOut);
		
		//How many..
		int len = mEntries.size();
		zOut.writeInt(len);
		
		//Now write out each row..
		for(MMREntry entry : mEntries) {
			entry.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mBlockTime   = MiniNumber.ReadFromStream(zIn);
		mEntryNumber = MiniInteger.ReadFromStream(zIn);
		
		//Now the Entries..
		mEntries = new ArrayList<>();
		mMaxEntries = new MMREntry[256];
		mMaxRow = 0;
		int len = zIn.readInt();
		for(int i=0;i<len;i++) {
			MMREntry entry = new MMREntry(0, null);
			entry.readDataStream(zIn);
			entry.setBlockTime(mBlockTime);
			
			if(!entry.isEmpty()) {
				//Now do the max..
				int row = entry.getRow();
				if(row > mMaxRow) {
					mMaxRow = row;	
				}
				
				if(mMaxEntries[row] == null) {
					mMaxEntries[row] = entry;
				}else if(mMaxEntries[row].getEntry().isLess(entry.getEntry())) {
					mMaxEntries[row] = entry;
				}
				
				//And add..
				mEntries.add(entry);
			}
		}
		
		//Finalize..
		finalizeSet();
	}
	
}

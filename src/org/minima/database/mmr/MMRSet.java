package org.minima.database.mmr;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.Proof.ProofChunk;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ObjectStack;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class MMRSet implements Streamable {
	
	/**
	 * Maximum number of rows in this set.. 2^128 trx max.. 
	 * Can be set higher - but takes more memory
	 */
	static int MAXROWS = 160;
	
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
	MiniNumber mEntryNumber = new MiniNumber(0);
	
	/**
	 * All the entries in this set 
	 */
	public Hashtable<String, MMREntry> mSetEntries;
	
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
	ArrayList<MiniNumber> mKeepers;
	
	/**
	 * Has the set been Finalized (Peaks / Root )
	 * No more changes after this.
	 */
	boolean mFinalized;
	
	MMRData mFinalizedRoot;
	ArrayList<MMREntry> mFinalizedPeaks;
	ArrayList<MMREntry> mFinalizedZeroRow;
	
	//HASH Function bit length.. ALWAYS 512 except when used in chainsha function
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
		mSetEntries       = new Hashtable<>();
		
		//The Maximum Rows and entries
		mMaxEntries = new MMREntry[MAXROWS];
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
				setEntry(peak.getRow(), peak.getEntryNumber(), peak.getData());
			
				//Add to the total entries.. the peaks are the binary value
				tot = tot.add(two.pow(peak.getRow()));
			}
			
			//Set the Entry Number
			mEntryNumber = new MiniNumber(tot);
			
			//Check!
			if(!mEntryNumber.isEqual(mParent.mEntryNumber)) {
				MinimaLogger.log("SERIOUS ERROR - Entry Number Mismatch! "+mEntryNumber+"/"+mParent.mEntryNumber);
			}
		}
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("block", mBlockTime);
		ret.put("entrynumber", mEntryNumber);

		JSONArray jentry = new JSONArray();
		Enumeration<MMREntry> entries = mSetEntries.elements();
		while(entries.hasMoreElements()) {
			MMREntry entry = entries.nextElement();
			jentry.add(entry.toJSON());
		}
		ret.put("entries", jentry);
		ret.put("maxrow", mMaxRow);
		
		JSONArray maxentry = new JSONArray();
		for(MMREntry entry : mMaxEntries) {
			if(entry != null) {
				maxentry.add(entry.getRow()+":"+entry.getEntryNumber().toString());
			}
		}
		ret.put("maxentries", maxentry);
		
		JSONArray keepers = new JSONArray();
		for(MiniNumber keeper : mKeepers) {
			keepers.add(keeper.toString());
		}
		ret.put("keepers", keepers);
		
		return ret;
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
	
	public MiniNumber getEntryNumber() {
		return mEntryNumber;
	}
	
	public MMRSet getParent() {
		return mParent;
	}
	
	private void incrementEntryNumber() {
		mEntryNumber = mEntryNumber.increment();
	}
	
	private String getHashTableEntry(int zRow, MiniNumber zEntry) {
		return zRow+":"+zEntry.toString();
	}
	
	private void addHashTableEntry(MMREntry zEntry) {
		String name = getHashTableEntry(zEntry.getRow(), zEntry.getEntryNumber());
		mSetEntries.put(name, zEntry);
	}
	
	public ArrayList<MMREntry> getRow(int zRow){
		ArrayList<MMREntry> row = new ArrayList<>();
		
		Enumeration<MMREntry> entries = mSetEntries.elements();
		while(entries.hasMoreElements()) {
			MMREntry entry = entries.nextElement();
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
	 * Search for unspent coins
	 * @return
	 */
	public ArrayList<MMREntry> searchAllRelevantCoins() {
		return searchCoins(true, 
				false, new MiniData(), 
				false, new MiniNumber(),
				false, new MiniData());
	}
	
	public ArrayList<MMREntry> searchAllCoins() {
		return searchCoins(false, 
				false, new MiniData(), 
				false, new MiniNumber(),
				false, new MiniData());
	}
	
	public ArrayList<MMREntry> searchCoins(
			boolean zKeeper, 
			boolean zSearchAddress, MiniData zAddress, 
			boolean zSearchAmount, MiniNumber zAmount, 
			boolean zSearchTokenid,MiniData zTokenID) {
		
		//Return structure
		ArrayList<MMREntry> ret = new ArrayList<>();
		
		//If you find a spent coin don't add it later as unspent..
		ArrayList<String> addedcoins = new ArrayList<>();
		
		//Loop through all
		MMRSet current = this;
		
		//Cycle through them..
		while(current != null) {
			//The list of entries to search in the Set
			ArrayList<MMREntry> entries = new ArrayList<>();
			
			//Only search relevant coins.. ?
			if(zKeeper) {
				for(MiniNumber keep : current.getKeepers()) {
					entries.add( current.getEntry(0, keep) );	
				}
			}else {
				entries = current.getZeroRow();
			}
				
			//Now cycle through these entries
			for(MMREntry entry : entries) {
				if(!entry.getData().isHashOnly()) {
					Coin cc = entry.getData().getCoin();
					
					//Is it spent
					boolean spent  		= entry.getData().isSpent();
					
					//Does it match what we are looking for
					boolean addr      	= true;
					boolean amount    	= true;
					boolean tok       	= true;
					
					if(zSearchAddress) {
						addr      		= cc.getAddress().isEqual(zAddress);
					}
					
					if(zSearchAmount) {
						amount    		= cc.getAmount().isEqual(zAmount);
					}
					
					if(zSearchTokenid) {
						tok       		= cc.getTokenID().isEqual(zTokenID);
					}
					
					//It matches..
					if(addr && amount && tok){
						String coinid = cc.getCoinID().to0xString();
						
						//Have we already added it..
						if(!addedcoins.contains(coinid)) {
							addedcoins.add(coinid);
							if(!spent) {
								ret.add(entry);
							}
						}
					}
				}
			}
				
			//Search the parent..
			current = current.getParent();
		}
		
		return ret;
	}
	
	/**
	 * Find an entry
	 * @param zCoinID
	 * @return
	 */
	public MMREntry findEntry(MiniData zCoinID) {
		//Loop through all
		MMRSet current = this;
		
		//Cycle through them..
		String coinid = zCoinID.to0xString();
		
		//Check the ZERO row..
		while(current != null) {
			
			ArrayList<MMREntry> zero = current.getZeroRow();
			for(MMREntry entry : zero) {
				if(!entry.getData().isHashOnly()) {
					if(entry.getData().getCoin().getCoinID().to0xString().equals(coinid)) {
						return entry;
					}
				}
			}
			
			//Search the parent..
			current = current.getParent();
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
	private MMREntry setEntry(int zRow, MiniNumber zEntry, MMRData zData) {
		if(mFinalized) {
			MinimaLogger.log("SETTING IN FINALIZED MMR!");
			return null;
		}
		
		//Store the Maximum
		if(zRow>mMaxRow) {
			mMaxRow = zRow;
		}
		
		//Check if already added..
		String entryname = getHashTableEntry(zRow, zEntry);
		MMREntry entry   = mSetEntries.get(entryname);
		
		//Create and add if not found
		if(entry == null) {
			entry = new MMREntry(zRow, zEntry);
			entry.setBlockTime(getBlockTime());
			entry.setData(zData);
			
			//Add it to the hastables
			addHashTableEntry(entry);
		}else {
			//Set the correct data
			entry.setData(zData);
		}
		
		//Is it a MAX
		if(mMaxEntries[zRow] == null) {
			mMaxEntries[zRow] = entry;
		}else if(mMaxEntries[zRow].getEntryNumber().isLess(zEntry)) {
			mMaxEntries[zRow] = entry;
		}
		
		//Return
		return entry;
	}
	
	protected MMREntry getEntry(int zRow, MiniNumber zEntry) {
		return getEntry(zRow, zEntry, MiniNumber.ZERO);
	}
	
	protected MMREntry getEntry(int zRow, MiniNumber zEntry, MiniNumber zMaxBack) {
		//Cycle down through the MMR sets..
		MMRSet current = this;
		
		//Now Loop..
		String entryname = getHashTableEntry(zRow, zEntry);
		while(current != null) {
			//Check within the designated range
			if(current.getBlockTime().isLess(zMaxBack)) {
				break;
			}
			
			//Check if already added..
			MMREntry entry   = current.mSetEntries.get(entryname);
			if(entry!=null) {
				return entry;
			}
			
			//Check the parent Set
			current = current.getParent();	
		}
		
		//If you can't find it - return empty entry..
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
			MMREntry sibling = getEntry(entry.getRow(), entry.getSibling());
			
			//Create the new row - hash LEFT + RIGHT
			MMRData parentdata = getParentMMRData(sibling, entry);
						
			//Set the Parent Entry
			entry = setEntry(entry.getParentRow(),entry.getParentEntry(),parentdata);
		}
		
		return ret;
	}
	
	/**
	 * Utility function when creating an MMRtree based on simple Hash values and not coins.. 
	 */
	public MMREntry addLeafNode(MiniData zData){
		return addUnspentCoin(new MMRData(zData, MiniNumber.ZERO));
	}
	
	/**
	 * Add data - an UNSPENT coin - Must be added to the correct mmrset
	 */
//	public MMREntry addExternalUnspentCoin(MMRProof zProof) {
//		//The Details
//		MiniNumber entrynum  = zProof.getEntryNumber();
//		MMRData proofdata    = zProof.getMMRData();
//		
//		//Do we already have this Entry..
//		MMREntry entry = getEntry(0, entrynum);
//		if(!entry.isEmpty() && !entry.getData().isHashOnly()) {
//			//Make sure its a keeper
//			addKeeper(entrynum);
//			
//			//We have it..
//			return entry;
//		}
//		
//		//Create a new entry
//		entry = setEntry(0, entrynum, proofdata);
//		MMREntry ret = entry;
//		
//		//Now go up the tree..
//		int prooflen = zProof.getProofLen();
//		int proofnum = 0;
//		while(proofnum < prooflen) {
//			MMREntry sibling = getEntry(entry.getRow(), entry.getSibling());
//			
//			//Do we add our own..
//			ProofChunk chunk = zProof.getProofChunk(proofnum++);
//			MMRData pdata = new MMRData(chunk.getHash(), chunk.getValue());
//			if(sibling.isEmpty()) {
//				//Set the data
//				sibling = setEntry(sibling.getRow(), sibling.getEntryNumber(), pdata);
//				
//			}else {
//				//Check the value is what we expect it to be
//				if(!sibling.getData().getFinalHash().isEqual(pdata.getFinalHash())) {
//					//Hmm..
//					MinimaLogger.log("Sibling Inconsistency!! in MMR @ "+entrynum+" when hard adding proof");
//					
//					return null;
//				}else {
//					//We have all this allready!
//					break;
//				}
//			}
//			
//			//Create the new combined value..
//			MMRData parentdata = null;
//			if(entry.isLeft()) {
//				parentdata = getParentMMRData(entry, sibling);
//			}else {
//				parentdata = getParentMMRData(sibling, entry);
//			}
//			
//			//Check if we have it..
//			MMREntry parent = getEntry(entry.getParentRow(),entry.getParentEntry());  
//			if(!parent.isEmpty()) {
//				if(!parent.getData().getFinalHash().isEqual(parentdata.getFinalHash())) {
//					//Hmm..
//					MinimaLogger.log("Parent Inconsistency!! in MMR @ "+entrynum+" when hard adding proof");
//					
//					return null;
//				}else {
//					//We have this..!
//					break;
//				}
//			}
//			
//			//Set the Parent Entry
//			entry = setEntry(entry.getParentRow(),entry.getParentEntry(),parentdata);
//		}
//		
//		//Its a keeper..
//		addKeeper(entrynum);
//		
//		return ret;
//	}
	
	/**
	 * Set entry to SPENT
	 */
	public MMREntry updateSpentCoin(MMRProof zProof) {
		//Double check the coin is valid..
		if(!checkProof(zProof)) {
			MinimaLogger.log("SERIOUS ERROR : trying to update MMR with invalid proof! "+zProof);
			return null;
		}
		
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
		
		//Is this is a Peak ? - if so, go no further..
		for(MMREntry peak : peaks) {
			if(entry.checkPosition(peak)) {
				return ret;
			}
		}
		
		//Now update the tree - Get the Sibling.. 
		MMREntry sibling = getEntry(0, entry.getSibling());
		
		//Is this a peak..
		int prooflen = zProof.getProofLen();
		if(sibling.isEmpty() && prooflen==0) {
			return ret;
		}
		
		//What time is this proof
		MiniNumber prooftime = zProof.getBlockTime();
		
		//Are there any proof chunks
		ProofChunk chunk  = null;
		MiniData phash    = null;
		MiniNumber pval   = null;
		int pcount        = 0;
		if(prooflen>0) {
			chunk = zProof.getProofChunk(pcount++);
			phash = chunk.getHash();
			pval  = chunk.getValue();
			
			//Do we need to fill it in.. 
			//don't need to check the Proof time as is either SPEMNT or UNSPENT ( and might be full details for the user..)
			if(sibling.isEmpty()) {
				sibling = setEntry(sibling.getRow(), sibling.getEntryNumber(), new MMRData(phash, pval));
			
			}else if(sibling.getBlockTime().isLess(zProof.getBlockTime()) && !sibling.getData().getFinalHash().isEqual(phash)) {
				//This might be FULL details..
				MinimaLogger.log("ZERO 0 SIBLING DIFFERENT HASH");
				sibling = setEntry(sibling.getRow(), sibling.getEntryNumber(), new MMRData(phash, pval));
			}
			
			//Set the Sibling in this MMRSET!.. this way the MMR peaks still work.. (as the max in a row MUST be on the left to be a peak ))
			setEntry(sibling.getRow(), sibling.getEntryNumber(),sibling.getData());
		}
		
		//Now go up the tree..
		while(!sibling.isEmpty()) {
			//Create the new parent
			//Create the new combined value..
			MMRData parentdata = null;
			if(entry.isLeft()) {
				parentdata = getParentMMRData(entry, sibling);
			}else {
				parentdata = getParentMMRData(sibling, entry);
			}
			
			//Set the Sibling in this MMRSET!.. this way the MMR peaks still work.. (as the max in a row MUST be on the left to be a peak ))
			setEntry(sibling.getRow(), sibling.getEntryNumber(), sibling.getData());
			
			//Set the Parent
			entry = setEntry(entry.getParentRow(), entry.getParentEntry(), parentdata);
			
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
				if(sibling.isEmpty() || sibling.getBlockTime().isLess(prooftime)) {
					sibling = setEntry(sibling.getRow(), sibling.getEntryNumber(), new MMRData(phash,pval));		
				}
			}
		}
		
		return ret;
	}
	
	/**
	 * Get An MMR Proof
	 */
	public MMRProof getProof(MiniNumber zEntryNumber) {
		//Get the Basic Proof..
		MMRProof proof = getProofToPeak(zEntryNumber);
		
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
	
	protected MMRProof getProofToPeak(MiniNumber zEntryNumber) {
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
	
	protected MMRProof getPeakToRoot(MiniData zPeak) {
		//Sum of all the initial proofs...
		MMRProof totalproof = new MMRProof();
		totalproof.setHashBitLength(MMR_HASH_BITS);
		
		//Get the Peaks..
		ArrayList<MMREntry> peaks = getMMRPeaks();
		
		//Now take all those values and put THEM in an MMR..
		MiniData currentpeak    = zPeak;
		MMREntry keeper 		= null;
		while(peaks.size() > 1) {
			//Create a new MMR
			MMRSet newmmr = new MMRSet(MMR_HASH_BITS);
			
			//Add all the peaks to it..
			for(MMREntry peak : peaks) {
				//Create the new data..
				MMRData data = new MMRData(peak.getHashValue(), peak.getData().getValueSum());
				
				//Add this..
				MMREntry current = newmmr.addUnspentCoin(data);
				
				//Is this the one to follow..
				if(peak.getHashValue().isEqual(currentpeak)) {
					keeper = current;
				}
			}
			
			//MUST have found the desired peak..
			if(keeper == null) {
				MinimaLogger.log("ERROR MMR NO Peak to ROOT found..");
				return null;
			}
			
			//Now get the keeper proof..
			MMRProof proof = newmmr.getProofToPeak(keeper.getEntryNumber());
			
			//Now add that to the total proof..
			int len = proof.getProofLen();
			for(int i=0;i<len;i++) {
				totalproof.addProofChunk(proof.getProofChunk(i));
			}
			
			//Now get the peaks.. repeat..
			peaks = newmmr.getMMRPeaks();
			
			//What to follow..
			proof.setData(keeper.getHashValue(), keeper.getData().getValueSum());
			currentpeak = proof.getFinalHash();
			keeper      = null;
		}
		
		return totalproof;
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
		//MUST have data to be checked
		if(zProof.getMMRData().isHashOnly()) {
			MinimaLogger.log("checkProof PROOF check HASHONLY! : "+zProof);
			return false;
		}
		
		//Check is not spent.. 
		if(zProof.getMMRData().isSpent()) {
			MinimaLogger.log("checkProof PROOF is SPENT! : "+zProof);
			return false;
		}
		
		//Get the MMRSet at the time this proof was made.. must be a recent proof..
		MMRSet proofset = getParentAtTime(zProof.getBlockTime());
		
		//The proof is it too old.. we can't check it. It's invalid.
		if(proofset == null) {
			MMRSet oldest = getOldestMMR();
			MinimaLogger.log("checkProof Proof too Old MAX:"+oldest.getBlockTime()+" proof:"+zProof);
			return false;
		}
				
		//Get the root..
		MMRData root = proofset.getMMRRoot();
		
		//Check..
		if(!zProof.getFinalHash().isEqual(root.getFinalHash())) {
			MinimaLogger.log("checkProof Proof not equal to ROOT! "+zProof);
			return false;
		}
		
		//So the proof was valid at that time.. if it has been SPENT, it will have been AFTER this block - and in our MMR
		MMREntry entry = getEntry(0, zProof.getEntryNumber(), zProof.getBlockTime().increment());
		
		//Is it there ?
		if(!entry.isEmpty()) {
			if(!entry.getData().getFinalHash().isEqual(zProof.getMMRData().getFinalHash())) {
				MinimaLogger.log("ERROR Proof Coin changed since proof created "+zProof);
				return false;
			}
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
	public boolean addKeeper(MiniNumber zEntry) {
		if(!isKeptAllready(zEntry)) {
			mKeepers.add(zEntry);
			return true;
		}
		
		return false;
	}
	
	/**
	 * Get the Keeper
	 */
	public ArrayList<MiniNumber> getKeepers() {
		return mKeepers;
	}
	
	/**
	 * Do we already keep this entry..
	 * 
	 * @param zNumber
	 * @return
	 */
	public boolean isKeptAllready(MiniNumber zNumber) {
		for(MiniNumber keep : mKeepers) {
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
	private void copyParentKeepers() {
		//Set not finalized..
		mFinalized = false;
		
		//First get the Keepers..
		ArrayList<MiniNumber> parentkeepers = new ArrayList<>();
		if(mParent!=null) {
			parentkeepers = mParent.getKeepers();
		}
		
		//Cycle through the current crop..
		ArrayList<MiniNumber> newkeepers = new ArrayList<>();
		for(MiniNumber keep : mKeepers) {
			//Get that LATEST entry and all the entries it uses on the way up..
			MMREntry entry = getEntry(0, keep);
			if(!entry.getData().isSpent()) {
				newkeepers.add(keep);
			}
		}
		
		//Reset
		mKeepers = newkeepers;
		
		//Cycle through the Keepers..
		for(MiniNumber keep : parentkeepers) {
			//Get that LATEST entry and all the entries it uses on the way up..
			MMREntry entry = getEntry(0, keep);
			
			//Check valid.. SHOULD NOT HAPPEN
			if(entry.isEmpty() || entry.getData().isHashOnly()) {
				MinimaLogger.log("copyKeepers on NULL Keeper Entry! "+keep);
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
					setEntry(sibling.getRow(), sibling.getEntryNumber(), sibling.getData());
					
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
	 * Recursively copy the parents..
	 * 
	 * Returns the minimum block copied..
	 */
	public MiniNumber copyAllParentKeepers(MiniNumber zCascade) {
		//Start at this point..
		MMRSet curr = this;
		
		//Minimum block copied
		MiniNumber minblock = zCascade;
		
		//Store all the pparents..
		ObjectStack stack = new ObjectStack();
		while(curr.getBlockTime().isMore(zCascade)) {
			//Add to the stack..
			stack.push(curr);
			
			//Get the parent..
			curr = curr.getParent();
		}
		
		//Now run through the stack..
		while(!stack.isEmpty()) {
			//Get the parent MMR..
			MMRSet mmr = (MMRSet) stack.pop();
			
			//Store it..
			if(mmr.getParent() != null) {
				MiniNumber pblock = mmr.getParent().getBlockTime();
				if(minblock == null) {
					minblock = pblock;
				}else if(pblock.isLess(minblock)) {
					minblock = pblock;
				}
			}
			
			//Copy the parents MMR keepers..
			mmr.copyParentKeepers();
		}
		
		//Return minimum block..
		return minblock;
	}
	
	/**
	 * Get a Parent block at a certain time..
	 */
	public MMRSet getParentAtTime(MiniNumber zTime) {
		MMRSet current = this;
		
		while(current != null) {
			if(current.getBlockTime().isEqual(zTime)) {
				return current;
			}
			
			//Too far.. only goes back in time further..
			if(current.getBlockTime().isLess(zTime)) {
				return null;
			}
			
			current = current.getParent();
		}

		return null;
	}
	
	private MMRSet getOldestMMR() {
		MMRSet current = this;
		
		while(current != null) {
			if(current.getParent() == null) {
				return current;
			}
			
			current = current.getParent();
		}
		
		return current;
	}
	
	/**
	 * Return the Parent of 2 sibling children
	 * @param zLeftChild
	 * @param zRightChild
	 * @return
	 */
	private MMRData getParentMMRData(MMREntry zLeftChild, MMREntry zRightChild) {
		//Combine the Values..
		MiniNumber sumvalue   = zLeftChild.getData().getValueSum().add(zRightChild.getData().getValueSum());
		
		//Make the unique MMRData Hash
//		MiniData combined = Crypto.getInstance().hashAllObjects( MMR_HASH_BITS,
//				zLeftChild.getHashValue(),zRightChild.getHashValue());
		MiniData combined = Crypto.getInstance().hashAllObjects( MMR_HASH_BITS,
				zLeftChild.getHashValue(),zRightChild.getHashValue(),sumvalue);
		
		//Create a new data proof
		return new MMRData(combined,sumvalue);
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
		int len = mSetEntries.size();
		zOut.writeInt(len);
		
		//Now write out each row..
		Enumeration<MMREntry> entries = mSetEntries.elements();
		while(entries.hasMoreElements()) {
			MMREntry entry = entries.nextElement();
			entry.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mBlockTime   = MiniNumber.ReadFromStream(zIn);
		mEntryNumber = MiniNumber.ReadFromStream(zIn);
		
		//Now the Entries..
		mSetEntries       = new Hashtable<>();
		mMaxEntries       = new MMREntry[MAXROWS];
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
				}else if(mMaxEntries[row].getEntryNumber().isLess(entry.getEntryNumber())) {
					mMaxEntries[row] = entry;
				}
				
				//And add..
				addHashTableEntry(entry);
			}
		}
		
		//Finalize..
		finalizeSet();
	}
	
	/**
	 * Get a DEEP copy of this transaction
	 * @throws IOException 
	 */
	public MMRSet deepCopy(){
		try {
			//First write transaction out to a byte array
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			writeDataStream(dos);
			dos.flush();
			dos.close();
			
			ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
			DataInputStream dis = new DataInputStream(bais);
			
			MMRSet mmr = new MMRSet();
			mmr.readDataStream(dis);
			
			dis.close();
			baos.close();
			
			return mmr;
			
		}catch(IOException ioexc) {
			MinimaLogger.log(ioexc);
		}	
		
		return null;
	}
	
	/**
	 * Serious tester Code
	 */
	public static MMRData makeMMRData(int zValue) {
		Coin cc = new Coin(MiniData.getRandomData(20), new MiniData("0x01"), new MiniNumber(zValue), MiniData.ZERO_TXPOWID);
		return new MMRData(MiniByte.FALSE, cc, MiniNumber.ZERO, new ArrayList<StateVariable>());
	}
	
	public static void getAllProofs(MMRSet zSet){
		int num = zSet.mEntryNumber.getAsInt();
		for(int i=0;i<num;i++) {
			MMRProof pp = zSet.getProof(new MiniNumber(i));
		}
	}
	
	public static void printMMR() {
		getCurrentMMR().finalizeSet();
		
		System.out.println("MMR : "+getCurrentMMR().getBlockTime());
		ArrayList<MMREntry> zero = getCurrentMMR().getZeroRow();
		for(MMREntry entry : zero) {
			System.out.println(entry.getRow()+"/"+entry.getEntryNumber()+":"+entry.getData().getValueSum()+" "+entry.getData().isHashOnly());
		}
		
		ArrayList<MMREntry> peaks = getCurrentMMR().getMMRPeaks(); 
		System.out.println("PEAKS:"+peaks.size());
		for(MMREntry entry : peaks) {
			System.out.println(entry.getRow()+" "+entry.getEntryNumber()+" "+entry.getData().getValueSum());
		}
		
		System.out.println("MAX ENTRY:"+getCurrentMMR().mEntryNumber);
		
		System.out.println("ROOT:"+getCurrentMMR().getMMRRoot());
		System.out.println();
		
		getAllProofs(getCurrentMMR());
	}
	
	public static void spend(int zEntry){
		MMRSet parent = getCurrentMMR().getParent().getParent();
		
		MMRProof pp = parent.getProof(new MiniNumber(zEntry));
		
		//Check the proof
		boolean valid = getCurrentMMR().checkProof(pp);
		if(!valid) {
			System.out.println("ERROR PROOF");
		}
	
		MiniNumber value = pp.getMMRData().getValueSum();
		getCurrentMMR().updateSpentCoin(pp);
		getCurrentMMR().addUnspentCoin(makeMMRData(value.getAsInt()));
	}
	
	static MMRSet currentMMR = null;
	public static void nextMMR() {
		currentMMR =  new MMRSet(currentMMR);
	}
	
	public static MMRSet getCurrentMMR() {
		return currentMMR;
	}
	
	public static void main(String[] zArgs) {
		//Start
		currentMMR = new MMRSet();
		
		//Genesis
		getCurrentMMR().addUnspentCoin(makeMMRData(0));
		printMMR();
		
		nextMMR();
		getCurrentMMR().addUnspentCoin(makeMMRData(10));
		getCurrentMMR().addUnspentCoin(makeMMRData(10));
		getCurrentMMR().addUnspentCoin(makeMMRData(30));
//		getCurrentMMR().addUnspentCoin(makeMMRData(5));
		printMMR();
		
		nextMMR();
		nextMMR();
		spend(1);
		spend(2);
		spend(3);
		printMMR();
		
		nextMMR();
		nextMMR();
		nextMMR();
		nextMMR();
		
		spend(6);
		printMMR();
		
//		nextMMR();
//		nextMMR();
//		nextMMR();
//		
//		spend(3);
//		printMMR();
		
		
//		nextMMR();
//		spend(4);
//		spend(5);
//		printMMR();
//		
//		nextMMR();
//		spend(6);
//		spend(2);
//		printMMR();
//		
//		nextMMR();
//		nextMMR();
//		nextMMR();
//		spend(10);
//		spend(8);
//		spend(9);
//		printMMR();
		
		
	}
	
}

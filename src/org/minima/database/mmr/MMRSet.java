package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.objects.Coin;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class MMRSet implements Streamable {
	
	/**
	 * What Block time does this MMR represent. Each represents 1 block.
	 */
	MiniNumber mBlockTime = MiniNumber.ZERO;
	
	/**
	 * The parent MMRData..
	 * 
	 * If you don't have it, ask your parent
	 */
	MMRSet mParent = null;
	
	/**
	 * What is the current entry number..
	 */
	public MiniNumber mEntryNumber = MiniNumber.ZERO;
	
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
	ArrayList<MiniNumber> mKeepers;
	
	/**
	 * Has the set been Finalized (Peaks / Root )
	 * No more changes after this.
	 */
	boolean mFinalized;
	
	MiniHash mFinalizedRoot;
	ArrayList<MMREntry> mFinalizedPeaks;
	ArrayList<MMREntry> mFinalizedZeroRow;
	
	/**
	 * Main Constructor
	 */
	public MMRSet() {
		this(null);
	}
	
	public MMRSet(MMRSet zParent) {
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
			mEntryNumber = new MiniNumber(tot);
			
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
	
	public void setBlockTime(MiniNumber zTime) {
		mBlockTime = zTime;
	}
	
	public MiniNumber getBlockTime() {
		return mBlockTime;
	}
	
	public void addKeeper(MiniNumber zEntry) {
		mKeepers.add(zEntry);
	}
	
	public MMRSet getParent() {
		return mParent;
	}
	
	private void incrementEntryNumber() {
		mEntryNumber = mEntryNumber.add(MiniNumber.ONE);
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
	 * Find an entry - but don't serach parents.
	 * @param zCoinID
	 * @return
	 */
	public MMREntry findEntry(MiniHash zCoinID) {
		//Get the zero row - no parents..
		ArrayList<MMREntry> zero=getZeroRow();
		
		for(MMREntry entry : zero) {
			if(!entry.getData().isHashOnly()) {
				if(entry.getData().getCoin().getCoinID().isExactlyEqual(zCoinID)){
					return entry;
				}
			}
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
	
	private MMREntry getEntry(int zRow, MiniNumber zEntry, boolean zCheckParent) {
		//Check if already added..
		for(MMREntry ent : mEntries) {
			if(ent.checkPosition(zRow, zEntry)) {
				return ent;
			}
		}
		
		//Check the parent Set
		if(zCheckParent && mParent!=null) {
			MMREntry entry = mParent.getEntry(zRow, zEntry, true);
			if(!entry.isEmpty()) {
				return entry;
			}
		}
		
		//If all else fails.. return empty entry..
		return new MMREntry(zRow, zEntry);
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
			MMREntry sibling = getEntry(entry.getRow(), entry.getLeftSibling(),true);
			
			//Create the new row - hash LEFT + RIGHT
			MiniHash combined = Crypto.getInstance().hashObjects(sibling.getHashValue(), entry.getHashValue());
			MMRData data = new MMRData(combined);
			
			//Set the Parent Entry
			entry = setEntry(entry.getParentRow(),entry.getParentEntry(),data);
		}
		
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
		MMREntry sibling = getEntry(0, entry.getSibling(),true);
		
		//Is this a peak..
		if(sibling.isEmpty() && zProof.getProofLen()==0) {
			return ret;
		}
		
		//Is an input missing or is it a less recent update 
		int pcount = 0;
		MiniHash phash = zProof.getProof(pcount++);
		
		//Do we need to fill it in..
		if(sibling.isEmpty()) {
			sibling = setEntry(sibling.getRow(), sibling.getEntry(), new MMRData(phash));
		}else if(sibling.getBlockTime().isLessEqual(zProof.getBlockTime())) {
			//Is it the original.. has all the micro details.. internal nodes are just the hash anyway
			MiniHash orighash = sibling.getData().getFinalHash();
			if(!orighash.isExactlyEqual(phash)) {
				sibling = setEntry(sibling.getRow(), sibling.getEntry(), new MMRData(phash));
			}
		}
		
		//Now go up the tree..
		while(!sibling.isEmpty()) {
			//Create the new parent
			MiniHash combined = null;
			if(entry.isLeft()) {
				combined = Crypto.getInstance().hashObjects(entry.getHashValue(),sibling.getHashValue());
			}else {
				combined = Crypto.getInstance().hashObjects(sibling.getHashValue(), entry.getHashValue());
			}
			
			//Create the new MMR Data
			MMRData data = new MMRData(combined);
			
			//Set the Sibling in this MMRSET!.. this way the MMR peaks still work.. 
			//(as the max in a row MUST be on the left to be a peak ))
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
			sibling = getEntry(entry.getRow(), entry.getSibling(),true);
			
			//Check for a valid sibling
			if(pcount < zProof.getProofLen()) {
				phash = zProof.getProof(pcount++);
				if(sibling.isEmpty()) {
					sibling = setEntry(sibling.getRow(), sibling.getEntry(), new MMRData(phash));		
				}else if(sibling.getBlockTime().isLessEqual(zProof.getBlockTime())) {
					//Is it the original.. has all the micro details.. internal nodes are just the hash anyway
					MiniHash orighash = sibling.getData().getFinalHash();
					if(!orighash.isExactlyEqual(phash)) {
						sibling = setEntry(sibling.getRow(), sibling.getEntry(), new MMRData(phash));	
					}	
				}
			}
		}
		
		return ret;
	}
	
	/**
	 * Get An MMR Proof
	 */
	public MMRProof getProof(MiniNumber zEntryNumber) {
		//First get the initial Entry.. check parents aswell..
		MMREntry entry = getEntry(0, zEntryNumber, true);
		
		//Now get all the hashes in the tree to a peak..
		MMRProof proof = new MMRProof(zEntryNumber, entry.getData(), mBlockTime);
		
		//Go up to the MMR Peak..
		MMREntry sibling = getEntry(entry.getRow(), entry.getSibling(), true);
		while(!sibling.isEmpty()) {
			//Add to our Proof..
			proof.addHash(sibling.getHashValue(), sibling.isLeft());	
			
			//Now get the Parent.. just need a reference even if is empty. To find the sibling.
			MMREntry parent = new MMREntry( sibling.getParentRow(), sibling.getParentEntry() );
			
			//And get the Sibling of the Parent..
			sibling = getEntry(parent.getRow(), parent.getSibling(), true);
		}
		
		return proof;
	}
	
//	/**
//	 * Get Proof to ROOT
//	 */
//	private MMRProof getPeakToRoot(MiniData32 zPeak) {
//		//Sum of all the initial proofs...
//		MMRProof totalproof = new MMRProof();
//		
//		//Get the Peaks..
//		ArrayList<MMREntry> peaks = getMMRPeaks();
//		
//		//Now take all those values and put THEM in an MMR..
//		MMREntry keeper      = null;
//		MiniData32 keepvalue = zPeak;
//		while(peaks.size() > 1) {
//			//Create a new MMR
//			MMRSet newmmr = new MMRSet();
//			
//			//Add all the peaks to it..
//			for(MMREntry peak : peaks) {
//				//Create the new input..
//				MMRData data = new MMRData(peak.getHashValue());
//				
//				//Is this the one to follow..
//				if(peak.getHashValue().isExactlyEqual(keepvalue)) {
//					keeper = newmmr.addUnspentCoin(data);	
//				}else {
//					newmmr.addUnspentCoin(data);	
//				}	
//			}
//			
//			//Now get the keeper proof..
//			MMRProof proof = newmmr.getProofToPeak(keeper.getEntry());
//			
//			//Now add thatto the totsl proof..
//			int len = proof.getProofLen();
//			for(int i=0;i<len;i++) {
//				totalproof.addHash(proof.getProof(i), proof.getLeftHash(i).isTrue());
//			}
//			
//			//Now get the peaks.. repeat..
//			peaks = newmmr.getMMRPeaks();
//		}
//		
//		return totalproof;
//	}
	
//	/**
//	 * Get the full proof to the root of the MMR
//	 * 
//	 * @param zEntry
//	 * @return
//	 */
//	public MMRProof getFullProofToRoot(MiniNumber zEntry) {
//		//Get the Basic Proof..
//		MMRProof proof = getProofToPeak(zEntry);
//		
//		//Now get the peak this points to..
//		MiniData32 peak = proof.calculateProof();
//		
//		//Now find the path to root for this peak
//		MMRProof rootproof = getPeakToRoot(peak);
//		
//		//Now add the two..
//		int len = rootproof.getProofLen();
//		for(int i=0;i<len;i++) {
//			proof.addHash(rootproof.getProof(i), rootproof.getLeftHash(i).isTrue());
//		}
//		
//		return proof;
//	}
	
	
	/**
	 * Check this is a valid UNSPENT output.
	 * 
	 * The Proof can point to a previous block. But must 
	 * be within the range that everyone store.. say 256 blocks.
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
		try {
			if(zProof.getMMRData().isSpent()) {
				return false;
			}
		}catch(Exception exc) {
			//ARRGGHH!!
			System.out.println("ERROR in MMRDATA module.. "+zProof);
			
			exc.printStackTrace();
			return false;
		}
		
		//Get the MMRSet at the time this proof was made.. must be a recent proof.. last 256 blocks.
		MMRSet proofset = getParentAtTime(zProof.getBlockTime());
		
		//The proof is it too old.. we can't check it. It's invalid.
		if(proofset == null) {
			return false;
		}
		
		//Check the proof point to the MMR ROOT..
		ArrayList<MMREntry> peaks = proofset.getMMRPeaks();
		
		//Calculate the proof..
		MiniHash proofpeak = zProof.calculateProof();
		
		//Is this is a Peak ? - if so, go no further..
		boolean found = false;
		for(MMREntry peak : peaks) {
			if(proofpeak.isExactlyEqual(peak.getHashValue())) {
				found = true;
				break;
			}
		}
		
		//Was it one of the peaks ?
		if(!found) {
			return false;
		}
		
		//So the proof was valid at that time.. if it has been SPENT, it will have been AFTER this block - and in our MMR
		MMREntry checker = getEntry(0, zProof.getEntryNumber(), true);
		
		//Is it there ?
		if(!checker.isEmpty()) {
			//Get the DATA - could be the original UNSPENT or the SPENT
			if(checker.getData().isSpent()) {
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
	public MiniHash getMMRRoot() {
		//Are we final
		if(mFinalized) {
			return mFinalizedRoot;
		}
		
		//Get the Peaks..
		ArrayList<MMREntry> peaks = getMMRPeaks();
		
		//Now take all those values and put THEM in an MMR..
		while(peaks.size() > 1) {
		
			//Create a new MMR
			MMRSet newmmr = new MMRSet();
			
			//Add all the peaks to it..
			for(MMREntry peak : peaks) {
				newmmr.addUnspentCoin(new MMRData(peak.getHashValue()));	
			}
			
			//Now get the peaks.. repeat..
			peaks = newmmr.getMMRPeaks();
		}
		
		return peaks.get(0).getHashValue();
	}
	
	/**
	 * Get the Keeper
	 */
	public ArrayList<MiniNumber> getKeepers() {
		return mKeepers;
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
	public void copyParentKeepers() {
		//First get the Keepers..
		ArrayList<MiniNumber> keepers = new ArrayList<>();
		if(mParent!=null) {
			keepers = mParent.getKeepers();
		}
		
		//Cycle through the current crop..
		ArrayList<MiniNumber> newkeepers = new ArrayList<>();
		for(MiniNumber keep : mKeepers) {
			//Get that LATEST entry and all the entries it uses on the way up..
			MMREntry entry = getEntry(0, keep, true);
			if(!entry.getData().isSpent()) {
				newkeepers.add(keep);
			}
		}
		//Reset
		mKeepers = newkeepers;
		
		//Cycle through the Keepers..
		for(MiniNumber keep : keepers) {
			//Get that LATEST entry and all the entries it uses on the way up..
			MMREntry entry = getEntry(0, keep, true);
			
			//If it's spent we don't keep it..
			if(entry.getData().isSpent()) {
				continue;
			}
			
			if(!isKeptAllready(keep)) {
				//Add to our list..
				mKeepers.add(keep);
			
				//Add it.. to THIS set.. not the parent..
				entry = setEntry(0, keep, entry.getData());
				
				//And now go go up the tree..
				MMREntry sibling = getEntry(entry.getRow(), entry.getSibling(), true);
				while(!sibling.isEmpty()) {
					//Add to our Set..
					setEntry(sibling.getRow(), sibling.getEntry(), sibling.getData());
					
					//Now get the Parent.. just need a reference even if is empty. To find the sibling.
					MMREntry parent = new MMREntry( sibling.getParentRow(), sibling.getParentEntry() );
					
					//And get the Sibling of the Parent..
					sibling = getEntry(parent.getRow(), parent.getSibling(), true);
				}
			}
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
	
//	/**
//	 * Cascade away the final blocks.. 
//	 * Keep all the important proofs
//	 */
//	public void cascadeKeeperBlock() {
//		//How long.. from here
//		int len = getParentLength();
//		
//		if(len>3) {
//			//Get the last but one
//			MMRSet prebase = getPenultimateParent();
//			
//			//Copy the important bits and remove..
//			prebase.copyParentKeepers();
//		}
//	}

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
		mEntryNumber = MiniNumber.ReadFromStream(zIn);
		
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
	}
	
}

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
import java.util.Random;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class MMR implements Streamable {
	
	/**
	 * Maximum number of coins in the MMR db is 2^256
	 */
	static int MAXROWS = 256;
	
	/**
	 * What Block time does this MMR represent. Each represents 1 block.
	 */
	MiniNumber mBlockTime = new MiniNumber(0);
	
	/**
	 * The parent MMRData..
	 * 
	 * If you don't have it, ask your parent
	 */
	MMR mParent = null;
	
	/**
	 * What is the current entry number..
	 */
	MMREntryNumber mEntryNumber = MMREntryNumber.ZERO;
	
	/**
	 * All the entries in this set 
	 */
	protected Hashtable<String, MMREntry> mSetEntries;
	
	/**
	 * The maximum row used in this Set
	 */
	int mMaxRow = 0;
	
	/**
	 * The Max entries per row..
	 */
	MMREntry mMaxEntries[];
	
	/**
	 * Has the set been Finalized (Peaks / Root )
	 * No more changes after this.
	 */
	boolean 			mFinalized;
	MMRData 			mFinalizedRoot;
	ArrayList<MMREntry> mFinalizedPeaks;
	
	/**
	 * Main Constructor
	 */
	public MMR() {
		this(null);
	}
	
	public MMR(MMR zParent) {
		//All the Entries in this set
		mSetEntries       = new Hashtable<>();
		
		//The Maximum Rows and entries
		mMaxEntries = new MMREntry[MAXROWS];
		mMaxRow     = 0;
		
		//Parent MMRSet
		mParent = zParent;
	
		//Not Finalized..
		mFinalized = false;
		
		//Now add the peaks..
		if(mParent != null) {
			if(!mParent.isFinalized()) {
				//Finalise the parent..
				mParent.finalizeSet();
			}
			
			//Set the Time.. 1 more than parent
			setBlockTime(mParent.getBlockTime().add(MiniNumber.ONE));
			
			//Calculate total entries..
			BigInteger tot = BigInteger.ZERO;
			BigInteger two = new BigInteger("2");
			
			ArrayList<MMREntry> peaks = mParent.getPeaks();
			for(MMREntry peak : peaks) {
				setEntry(peak.getRow(), peak.getEntryNumber(), peak.getMMRData());
				
				//Add to the total entries.. the peaks are the binary value
				tot = tot.add(two.pow(peak.getRow()));
			}
			
			//Set the Entry Number
			mEntryNumber = new MMREntryNumber(tot);
			
			//Check!
			if(!mEntryNumber.isEqual(mParent.mEntryNumber)) {
				MinimaLogger.log("SERIOUS ERROR - Entry Number Mismatch! "+mEntryNumber+"/"+mParent.mEntryNumber);
			}
		}
	}
	
	public void calculateEntryNumberFromPeaks() {
		//Calculate total entries..
		BigInteger tot = BigInteger.ZERO;
		BigInteger two = new BigInteger("2");
		
		ArrayList<MMREntry> peaks = getPeaks();
		for(MMREntry peak : peaks) {
			setEntry(peak.getRow(), peak.getEntryNumber(), peak.getMMRData());
			
			//Add to the total entries.. the peaks are the binary value
			tot = tot.add(two.pow(peak.getRow()));
		}
		
		//Set the Entry Number
		mEntryNumber = new MMREntryNumber(tot);
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
				maxentry.add(entry.toJSON());
			}
		}
		ret.put("maxentries", maxentry);
		
		ret.put("root", getRoot().toJSON());
		
		return ret;
	}
	
	public void finalizeSet() {
		//Reset
		mFinalized = false;
				
		//The peaks..
		mFinalizedPeaks = getPeaks();
		
		//Create the final values..
		mFinalizedRoot = getRoot();
		
		//We are now Finalized..
		mFinalized = true;
	}
	
	public void setFinalized(boolean zFinalized) {
		mFinalized = zFinalized;
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
	
	public MMREntryNumber getEntryNumber() {
		return mEntryNumber;
	}
	
	public void clearParent() {
		mParent = null;
	}
	
	public void setParent(MMR zMMR) {
		mParent = zMMR;
	}
	
	public MMR getParent() {
		return mParent;
	}
	
	public int getTotalEntries() {
		return mSetEntries.size();
	}
	
	private String getHashTableEntry(int zRow, MMREntryNumber zEntry) {
		return zRow+":"+zEntry.toString();
	}
	
	private void addHashTableEntry(MMREntry zEntry) {
		String name = getHashTableEntry(zEntry.getRow(), zEntry.getEntryNumber());
		mSetEntries.put(name, zEntry);
	}
	
	private void removeHashTableEntry(MMREntry zEntry) {
		String name = getHashTableEntry(zEntry.getRow(), zEntry.getEntryNumber());
		mSetEntries.remove(name);
	}
	
	/**
	 * Sets the Entry value in THIS SET ONLY. Does not affect parents.
	 * @param zRow
	 * @param zEntry
	 * @param zData
	 * @return
	 */
	public MMREntry setEntry(int zRow, MMREntryNumber zEntry, MMRData zData) {
		if(mFinalized) {
			MinimaLogger.log("SETTING IN FINALIZED MMR!");
			return null;
		}
		
		//Store the Maximum
		if(zRow>mMaxRow) {
			mMaxRow = zRow;
		}
		
		//Create new entry
		MMREntry entry = new MMREntry(zRow, zEntry, zData);
		
		//Add it to the hashtable - overwrite the old one if exists
		addHashTableEntry(entry);
		
		//Is it a MAX
		if(mMaxEntries[zRow] == null) {
			mMaxEntries[zRow] = entry;
		}else if(mMaxEntries[zRow].getEntryNumber().isLessEqual(zEntry)) {
			mMaxEntries[zRow] = entry;
		}
		
		//Return
		return entry;
	}
	
	public MMREntry getEntry(int zRow, MMREntryNumber zEntry) {
		return getEntry(zRow, zEntry, MiniNumber.ZERO);
	}
	
	public MMREntry getEntry(int zRow, MMREntryNumber zEntry, MiniNumber zMaxBack) {
		//Cycle down through the MMR sets..
		MMR current = this;
		
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
		
		return entry;
	}
	
	/**
	 * Add data to this MMR
	 */
	public MMREntry addEntry(MMRData zData) {
		//Create a new entry
		MMREntry entry = setEntry(0, mEntryNumber, zData);
		MMREntry ret   = entry;
		
		//1 more entry
		mEntryNumber = mEntryNumber.increment();
		
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
	 * Update an entry in the MMR.. 
	 * 
	 * The proof MUST have been checked before and be valid.
	 */
	public void updateEntry(MMREntryNumber zEntry, MMRProof zOldProof, MMRData zNewData) {
		//Get the current peaks
		ArrayList<MMREntry> peaks = getPeaks();
		
		//What is the proof time
		int currentproof 		= 0;
		MiniNumber prooftime 	= zOldProof.getBlockTime();
		
		//Set the Entry..
		MMREntry entry = setEntry(0, zEntry, zNewData);
				
		//Is this is a Peak ? - if so, go no further..
		MMRData parentdata, siblingdata = null;
		while(!entry.checkPosition(peaks)) {
			//Get the sibling.. only go as far back as the proof
			MMREntry sibling = getEntry(entry.getRow(), entry.getSibling(), prooftime);
			
			//If we don't have it use the proof value..
			if(sibling.isEmpty()) {
				//Get the proof data version
				siblingdata = zOldProof.getProofChunk(currentproof).getMMRData();
			}else {
				siblingdata = sibling.getMMRData();
			}
			currentproof++;
			
			//Set it so is a top level entry in this MMR
			sibling = setEntry(sibling.getRow(), sibling.getEntryNumber(), siblingdata);
			
			//Calculate the parent
			if(entry.isLeft()) {
				parentdata = getParentMMRData(entry, sibling);
			}else {
				parentdata = getParentMMRData(sibling, entry);
			}
			
			//Make the entry the parent..
			entry = setEntry(entry.getParentRow(), entry.getParentEntry(), parentdata);
		}
	}
	
	/**
	 * Get An MMR Proof
	 */
	public MMRProof getProof(MMREntryNumber zEntryNumber) {
		//Get this entry
		MMREntry entry = getEntry(0, zEntryNumber);
		
		//Get the Basic Proof..
		MMRProof proof = getProofToPeak(zEntryNumber);
		
		//Calculate the peak
		MMRData peak = proof.calculateProof(entry.getMMRData());
		
		//Now find the path to root for this peak
		MMRProof rootproof = getPeakToRoot(peak);
		
		//Now add the two..
		int len = rootproof.getProofLength();
		for(int i=0;i<len;i++) {
			proof.addProofChunk(rootproof.getProofChunk(i));
		}
		
		return proof;
	}
	
	public MMRProof getProofToPeak(MMREntryNumber zEntryNumber) {
		//First get the initial Entry.. check parents as well..
		MMREntry entry = getEntry(0, zEntryNumber);
		
		//Now get all the hashes in the tree to a peak..
		MMRProof proof = new MMRProof(mBlockTime);
		
		//Go up to the MMR Peak..
		MMREntry sibling = getEntry(entry.getRow(), entry.getSibling());
		while(!sibling.isEmpty()) {
			//Add to our Proof..
			proof.addProofChunk(sibling.isLeft(),sibling.getMMRData());	
			
			//Now get the Parent.. just need a reference even if is empty. To find the sibling.
			MMREntry parent = new MMREntry( sibling.getParentRow(), sibling.getParentEntry() );
			
			//And get the Sibling of the Parent..
			sibling = getEntry(parent.getRow(), parent.getSibling());
		}
		
		return proof;
	}
	
	protected MMRProof getPeakToRoot(MMRData zPeak) {
		//Sum of all the initial proofs...
		MMRProof totalproof = new MMRProof(getBlockTime());
		
		//Get the Peaks..
		ArrayList<MMREntry> peaks = getPeaks();
		
		//Now take all those values and put THEM in an MMR..
		MMRData currentpeak    	= zPeak;
		MMREntry keeper 		= null;
		while(peaks.size() > 1) {
			//Create a new MMR
			MMR newmmr = new MMR();
			
			//Add all the peaks to it..
			for(MMREntry peak : peaks) {
				//Add this..
				MMREntry current = newmmr.addEntry(peak.getMMRData());
				
				//Is this the one to follow..
				if(peak.getMMRData().getData().isEqual(currentpeak.getData())) {
					keeper = current;
				}
			}
			
			//Now get the keeper proof..
			MMRProof proof = newmmr.getProofToPeak(keeper.getEntryNumber());
			
			//Now add that to the total proof..
			int len = proof.getProofLength();
			for(int i=0;i<len;i++) {
				totalproof.addProofChunk(proof.getProofChunk(i));
			}
			
			//Recalculate
			currentpeak = totalproof.calculateProof(currentpeak);
			
			//What to follow..
			keeper      = null;
			
			//Now get the peaks.. repeat..
			peaks = newmmr.getPeaks();
		}
		
		return totalproof;
	}
	
	/**
	 * Check this proof is correct for THIS block
	 * 
	 * Can point to ROOT or to a PEAK
	 */
	public boolean checkProof(MMRData zMMRData, MMRProof zProof) {
		//Calculate the final data unit
		MMRData root = zProof.calculateProof(zMMRData);
		
		//Check proof root is equal to the root or one of the peaks
		if(root.isEqual(getRoot())) {
			return true;
		}
		
		//Check against all the peaks..
		ArrayList<MMREntry> peaks = getPeaks();
		for(MMREntry peak : peaks) {
			if(root.isEqual(peak.getMMRData())){
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Check this proof is correct for any block past the proof time
	 */
	public boolean checkProofTimeValid(MMREntryNumber zEntry, MMRData zMMRData, MMRProof zProof) {
		//Get the Parent MMR at this time
		MMR mmr = getParentAtTime(zProof.getBlockTime());
		if(mmr == null) {
			//We do not have this MMR - so cannot check..
			return false;
		}
		
		//Check it at that MMR time..
		if(!mmr.checkProof(zMMRData, zProof)) {
			return false;
		}
		
		//Now check that there is no entry AFTER this that is different..
		MMREntry checker = getEntry(0, zEntry, zProof.getBlockTime());
		
		//Could be empty - which means ours is valid
		if(checker.isEmpty()) {
			return true;
		}
		
		//Are they the same
		return checker.getMMRData().isEqual(zMMRData);
	}
	
	/**
	 * Get the MMR peaks of this Set
	 * @return
	 */
	public ArrayList<MMREntry> getPeaks(){
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
	public MMRData getRoot() {
		//Are we final
		if(mFinalized) {
			return mFinalizedRoot;
		}
		
		//Get the Peaks..
		ArrayList<MMREntry> peaks = getPeaks();
		
		//Now take all those values and put THEM in an MMR..
		while(peaks.size() > 1) {
		
			//Create a new MMR
			MMR newmmr = new MMR();
			
			//Add all the peaks to it..
			for(MMREntry peak : peaks) {
				MMRData newpeak = new MMRData(peak.getMMRData().getData(),peak.getMMRData().getValue());
				newmmr.addEntry(newpeak);	
			}
			
			//Now get the peaks.. repeat..
			peaks = newmmr.getPeaks();
		}
		
		return peaks.get(0).getMMRData();
	}
	
	/**
	 * Get a Parent MMR at a certain time..
	 */
	public MMR getParentAtTime(MiniNumber zTime) {
		MMR current = this;
		
		while(current != null) {
			if(current.getBlockTime().isEqual(zTime)) {
				return current;
			}
			
			//Too far.. only goes back in time further..
			if(current.getBlockTime().isLess(zTime)) {
				return null;
			}
			
			//Check the Parent
			current = current.getParent();
		}

		return null;
	}
	
	/**
	 * Return the Parent of 2 sibling children
	 * @param zLeftChild
	 * @param zRightChild
	 * @return
	 */
	private MMRData getParentMMRData(MMREntry zLeftChild, MMREntry zRightChild) {
		//Combine the Values..
		MiniNumber sumvalue   = zLeftChild.getMMRData().getValue().add(zRightChild.getMMRData().getValue());
		
		//Make the unique MMRData Hash
		MiniData combined = Crypto.getInstance().hashAllObjects( zLeftChild.getMMRData().getData(),
																zRightChild.getMMRData().getData(),
																sumvalue);
		
		//New MMRData
		MMRData parent = new MMRData(combined, sumvalue);
		
		//Create a new data proof
		return parent;
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
		MiniNumber elen = new MiniNumber(mSetEntries.size());
		elen.writeDataStream(zOut);
		
		//Now write out each row..
		Enumeration<MMREntry> entries = mSetEntries.elements();
		while(entries.hasMoreElements()) {
			MMREntry entry = entries.nextElement();
			entry.writeDataStream(zOut);
		}
	}
	
	/**
	 * Get a DEEP copy of this TxPoW
	 */
	public MMR deepCopy(){
		try {
			//First write transaction out to a byte array
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			writeDataStream(dos);
			dos.flush();
			dos.close();
			
			byte[] mmrbytes = baos.toByteArray();
			ByteArrayInputStream bais = new ByteArrayInputStream(mmrbytes);
			DataInputStream dis = new DataInputStream(bais);
			
			MMR deepcopy = new MMR();
			deepcopy.readDataStream(dis);
			
			dis.close();
			baos.close();
			
			return deepcopy;
			
		}catch(IOException ioexc) {
			MinimaLogger.log(ioexc);
		}	
		
		return null;
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mBlockTime   = MiniNumber.ReadFromStream(zIn);
		mEntryNumber = MMREntryNumber.ReadFromStream(zIn);
		
		//Now the Entries..
		mSetEntries       = new Hashtable<>();
		mMaxEntries       = new MMREntry[MAXROWS];
		mMaxRow = 0;
		
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			MMREntry entry = MMREntry.ReadFromStream(zIn);
			setEntry(entry.getRow(), entry.getEntryNumber(), entry.getMMRData());
		}
		
		//Finalise..
		finalizeSet();
	}
	
	public static MMR ReadFromStream(DataInputStream zIn) throws IOException {
		MMR mmr = new MMR();
		mmr.readDataStream(zIn);
		return mmr;
	}
	
	/**
	 * PRUNE
	 * 
	 * Recursive Remove subtrees below a ZERO SUM Value
	 */
	public void pruneTree() {
		//Get the Peaks..
		ArrayList<MMREntry> peaks = getPeaks();
		for(MMREntry peak : peaks) {
			prune(peak);
		}
	}
	
	private void prune(MMREntry zStartNode) {
		
		//Is this a valid MMRENtry
		if(zStartNode.isEmpty()) {
			//Already pruned..
			return;
		}
		
		//Which row are the children on..
		int childrow = zStartNode.getChildRow();
		if(childrow<0) {
			//We are at the base leaf nodes.. leave it..
			return;
		}
		
		//The children..
		MMREntry leftchild 	= getEntry(childrow, zStartNode.getLeftChildEntry());
		MMREntry rightchild = getEntry(childrow, zStartNode.getRightChildEntry());
		
		//Prune the children if they exist
		prune(leftchild);
		prune(rightchild);
		
		//Is this a ZERO node.. if so remove the children
		if(zStartNode.getMMRData().getValue().isEqual(MiniNumber.ZERO)) {
			removeHashTableEntry(leftchild);
			removeHashTableEntry(rightchild);
		}
		
	}
	
	/**
	 * TEST STUFF
	 */
	public static void main(String[] zArgs) {
	
		System.out.println("** MMR Tree Prune POC **");
		
		MMR mmr = new MMR();
		
		//First bit of data
		MMRData zero 	= new MMRData(new MiniData("0x00"), new MiniNumber(0));
		MMRData one 	= new MMRData(new MiniData("0x01"), new MiniNumber(1));
		
		//Add 16 entries..
		for(int loop=0;loop<16;loop++) {
			mmr.addEntry(one);
		}
		printmmrtree(mmr);
		
		//Set random values to Zero..
		for(int zz=0;zz<24;zz++) {
			int rand 				= new Random().nextInt(16);
			MMREntryNumber entry 	= new MMREntryNumber(rand);
			MMREntry ent = mmr.getEntry(0, entry);
			if(ent.isEmpty() || ent.getMMRData().getValue().isEqual(MiniNumber.ZERO)) {
				continue;
			}
			
			System.out.println("\nSet entry "+rand+" to 0");
			MMRProof proof 	= mmr.getProofToPeak(entry);
			mmr.updateEntry(entry, proof, zero);
			mmr.pruneTree();
			printmmrtree(mmr);
		}
	}
	
	public static void printinfo(MMR zTree) {
		System.out.println("");
		System.out.println("MMR TREE DATA");
		System.out.println("Total tree size : "+zTree.getTotalEntries());
		System.out.println("Current entry   : "+zTree.getEntryNumber());
		//The Peaks..
		ArrayList<MMREntry> peaks = zTree.getPeaks();
		for(MMREntry peak : peaks) {
			System.out.println("PEAK : "+peak);
		}
		MMRData root = zTree.getRoot();
		System.out.println("Peaks : "+zTree.getPeaks().size());
		System.out.println("Root  : "+root);
	}
	
	public static void printmmrtree(MMR zTree) {
		//Start from the max row..
		int toprow = zTree.mMaxRow;
		for(int i=toprow;i>=0;i--) {
		
			//The start gap
			int startgap 	= (int) (Math.pow(2, i) -1) * 2;
			int gap 		= (int) (Math.pow(2, i+1)) * 2;
			
			//Get the row..
			ArrayList<MMREntry> row = new ArrayList<>(); 
			Enumeration<MMREntry> entries = zTree.mSetEntries.elements();
			while(entries.hasMoreElements()) {
				MMREntry entry = entries.nextElement();
				if(entry.getRow() == i) {
					row.add(entry);
				}
			}
			
			//The final char buffrer for the row
			char[] str = new char[128];
			for(int c=0;c<128;c++) {
				str[c] = ' ';
			}
			
			StringBuffer strow = new StringBuffer("                                                                       ");
			for(MMREntry entry : row) {
				
				//Add the entry to the correct spot..
				int xpos 	 = entry.getEntryNumber().getBigDecimal().intValue();
				int finalpos = startgap+(xpos*gap);
				int value    = entry.getMMRData().getValue().getAsInt();
				String valstr = ""+value;
				char[] cc = valstr.toCharArray();
				
				System.arraycopy(cc, 0, str, finalpos, cc.length);
			}
			
			System.out.println(str);
		}
		
		//Print the peak value..
		System.out.println("Total Entries       : "+zTree.mSetEntries.size());
		System.out.println("Tree Peak Sum Value : "+zTree.getRoot().getValue());
		
	}
	
}


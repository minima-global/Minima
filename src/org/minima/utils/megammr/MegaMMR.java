package org.minima.utils.megammr;

import java.util.Random;

import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMREntryNumber;
import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class MegaMMR {

	public static void main(String[] zArgs) {
		
		//The Massive MMR
		MMR megammr = new MMR();
		
		//Now create some MMRs.. and squash them onto this MMR..
		MMR tempmmr = new MMR();
		
		//Add some coins..
		
		
		
	}
	
	public static void pruneDemo() {
		
		System.out.println("** MMR Tree Prune POC **");
		
		MMR mmr = new MMR();
		
		//First bit of data
		MMRData zero 	= new MMRData(new MiniData("0x00"), new MiniNumber(0));
		MMRData one 	= new MMRData(new MiniData("0x01"), new MiniNumber(1));
		
		//Add 16 entries..
		int treesize = 32;
		for(int loop=0;loop<treesize;loop++) {
			mmr.addEntry(one);
		}
		MMR.printmmrtree(mmr);
		
		//Set random values to Zero..
		for(int zz=0;zz<(treesize*2);zz++) {
			int rand 				= new Random().nextInt(treesize);
			MMREntryNumber entry 	= new MMREntryNumber(rand);
			MMREntry ent = mmr.getEntry(0, entry);
			if(ent.isEmpty() || ent.getMMRData().getValue().isEqual(MiniNumber.ZERO)) {
				continue;
			}
			
			System.out.println();
			System.out.println("\nSet entry "+rand+" to 0");
			MMRProof proof 	= mmr.getProofToPeak(entry);
			mmr.updateEntry(entry, proof, zero);
			mmr.pruneTree();
			MMR.printmmrtree(mmr);
		}
	}
	
}

package org.minima.database.cascade;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MiniFile;
import org.minima.utils.Streamable;

public class Cascade implements Streamable {

	CascadeNode mTip = null;
	
	BigDecimal mTotalWeight = BigDecimal.ZERO;
	
	public Cascade() {}
	
	public void addToTip(TxPoW zTxPoW) {
		//New Node
		CascadeNode node = new CascadeNode(zTxPoW);
		
		if(mTip != null) {
			//Set as child to the old tip
			node.setParent(mTip);
		}
		
		//This is the new tip
		mTip = node;
	}
	
	public CascadeNode getTip() {
		return mTip;
	}
	
	public BigDecimal getTotalWeight() {
		return mTotalWeight;
	}
	
	public int getLength() {
		int length=0;
		CascadeNode current 	= mTip;
		while(current != null) {
			length++;
			current = current.getParent();
		}
		
		return length;
	}
	
	public void cascadeChain() {
		//check not empty
		if(getTip() == null) {
			return;
		}
		
		//Start at the tip and work back..
		CascadeNode newcascade	= mTip;
		CascadeNode current 	= mTip.getParent();
		
		//Keep a score of the total weight
		mTotalWeight = newcascade.getCurrentWeight();
		
		int casclevel = 0;
		int totlevel  = 1;
		while(current != null) {
			//What super level is this node
			int superlev = current.getSuperLevel();
			
			//Are we above the minimum power
			if(superlev>=casclevel) {
				//Set the current level
				current.setLevel(casclevel);
				
				//Add to the new..
				newcascade.setParent(current);
				
				//New root
				newcascade = current;
				
				//Add to the total Weight
				mTotalWeight = mTotalWeight.add(newcascade.getCurrentWeight());
				
				//Increase node count at this level
				totlevel++;
				if(totlevel>=GlobalParams.MINIMA_CASCADE_LEVEL_NODES) {
					if(casclevel<GlobalParams.MINIMA_CASCADE_LEVELS-1) {
						casclevel++;
						totlevel = 0;
					}
				}
			}
			
			//Get the parent
			current = current.getParent();
		}
		
		//New cascade has no parent - it's the root..
		newcascade.setParent(null);
	}
	
	/**
	 * Check the cascade is valid and all parents point back correctly
	 */
	public boolean checkCascade() {
		return true;
	}
	
	/**
	 * Get a DEEP copy 
	 */
	public Cascade deepCopy() throws IOException{
		//First write transaction out to a byte array
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(baos);
		writeDataStream(dos);
		dos.flush();
		dos.close();
		
		//Now read it into a new transaction..
		byte[] bytes = baos.toByteArray();
		ByteArrayInputStream bais = new ByteArrayInputStream(bytes);
		DataInputStream dis = new DataInputStream(bais);
		
		Cascade deepcopy = new Cascade();
		deepcopy.readDataStream(dis);
		
		dis.close();
		baos.close();
		
		return deepcopy;
	}
	
	public String printCascade() {
		
		StringBuffer casstr = new StringBuffer();
		
		//Flip it..
		ArrayList<CascadeNode> nodes = new ArrayList<>();
		
		CascadeNode current = mTip;
		while(current != null) {
			nodes.add(0, current);
			current = current.getParent();
		}
		
		//Only half print it..
		int levelcounter 	= 0;
		int oldlevel 		= -1;
		for(CascadeNode node : nodes) {
			casstr.append(node+"\n");
			
//			if(node.getLevel() != oldlevel) {
//				if(levelcounter!=-1) {
//					casstr.append(levelcounter+" @ "+oldlevel+"\n");
//				}
//				casstr.append(node+"\n");
//				levelcounter = 0;
//			}else {
//				levelcounter++;
//			}
//			
//			//Store..
//			oldlevel = node.getLevel();
		}
		
		return casstr.toString();
	}

	public void loadDB(File zFile) {
		MiniFile.loadObject(zFile, this);
	}
	
	public void saveDB(File zFile) {
		MiniFile.saveObject(zFile, this);
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//How many nodes in this Cascade
		int len = 0;
		CascadeNode current = getTip();
		while(current != null) {
			len++;
			current = current.getParent();
		}
		
		//Now Write this out..
		MiniNumber.WriteToStream(zOut, len);
		
		//And write them all out..
		current = getTip();
		while(current != null) {
			current.writeDataStream(zOut);
			current = current.getParent();
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		//How many nodes..
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
				
		//Load them all..
		mTip 				= null;
		mTotalWeight 		= BigDecimal.ZERO;
		CascadeNode current = null;
		for(int i=0;i<len;i++) {
			CascadeNode node = CascadeNode.ReadFromStream(zIn);
			
			if(current != null) {
				current.setParent(node);
			}else {
				mTip = node;
			}
			
			current = node;
		}
		
		//And calculate weights..
		cascadeChain();
	}
}

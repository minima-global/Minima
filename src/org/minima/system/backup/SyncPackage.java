package org.minima.system.backup;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class SyncPackage implements Streamable{

	MiniNumber mCascadeNode;
	
	ArrayList<SyncPacket> mNodes;
	
	public SyncPackage() {
		mNodes = new ArrayList<>();
		mCascadeNode = MiniNumber.ZERO;
	}
	
	public void setCascadeNode(MiniNumber zNumber) {
		mCascadeNode = zNumber;
	}
	
	public MiniNumber getCascadeNode() {
		return mCascadeNode;
	}
	
	public ArrayList<SyncPacket> getAllNodes(){
		return mNodes;
	}

//	public BigInteger calculateWeight() {
//		ArrayList<SyncPacket> rev = new ArrayList<SyncPacket>();
//	
//		MiniNumber lastblock = MiniNumber.ONE;
//		
//		//First reverse the stream
//		for(SyncPacket spack : mNodes) {
//			rev.add(0,spack);
//			lastblock = spack.getTxPOW().getBlockNumber();
//		}
//		
//		lastblock = lastblock.increment();
//		
//		BigInteger totalweight = BigInteger.ZERO;
//		boolean cascadestarted = false;
//		
//		int lastdiff = 0;
//		int lastsup  = 0;
//		
//		//Now calculate the total weight
//		int num = 0;
//		for(SyncPacket spack : rev) {
//			TxPOW txpow = spack.getTxPOW();
//			
//			BigInteger normweight = Maths.BI_TWO.pow(txpow.getBlockDifficulty());
//			BigInteger supweight  = Maths.BI_TWO.pow(txpow.getSuperLevel());
//			
//			//Check if started..
//			if(num>GlobalParams.MINIMA_CASCADE_DEPTH) {
//				cascadestarted = true;
//			}
//			
//			if(!cascadestarted && txpow.getBlockNumber().isEqual(lastblock.decrement())) {
//				//Still normal..
//				totalweight = totalweight.add(normweight);
//				
//				MinimaLogger.log(num + ") ["+txpow.getBlockDifficulty()+"/"+txpow.getSuperLevel()+"] "+txpow.getBlockNumber()+" *"+normweight+" "+supweight);
//				
//				lastdiff = txpow.getBlockDifficulty();
//				lastsup  = txpow.getSuperLevel();
//				
//			}else {
//				//First time..
//				if(!cascadestarted) {
//					//Fix the last one.. Cascading tree does it like this..
//					BigInteger weight = Maths.BI_TWO.pow(lastdiff);
//					totalweight = totalweight.subtract(weight);
//					
//					//Add as a super.
//					weight = Maths.BI_TWO.pow(lastsup);
//					totalweight = totalweight.add(weight);
//					
//					cascadestarted = true;
//				}
//				
//				//And as normal
//				totalweight = totalweight.add(supweight);
//				
//				MinimaLogger.log(num + ") "+txpow.getBlockNumber()+" "+normweight+" *"+supweight);
//			}
//			
//			num++;
//			lastblock = txpow.getBlockNumber();
//		}
//		
//		MinimaLogger.log("Total : "+totalweight);
//		
//		return totalweight;
//	}
	
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		int len = mNodes.size();
		zOut.writeInt(len);
		for(SyncPacket node : mNodes) {
			node.writeDataStream(zOut);
		}
		mCascadeNode.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		int len = zIn.readInt();
		for(int i=0;i<len;i++) {
			SyncPacket node = new SyncPacket();
			node.readDataStream(zIn);
			mNodes.add(node);
		}
		mCascadeNode = MiniNumber.ReadFromStream(zIn);
	}
	
	@Override
	public String toString() {
		String ret = "";
		for(SyncPacket node : mNodes) {
			ret += node+",";
		}
		return ret;
	}
}

package org.minima.utils;

import java.util.ArrayList;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;

public class MerkleProof {

	public class MerkleProofChunk {
		MiniHash mData;
		MiniByte mLeftRight;
	
		public MerkleProofChunk() {
			// TODO Auto-generated constructor stub
		}
	}
	

	//The data you are trying to prove..
	MiniData mData;
	
	//The Merkle Brank that when applied to the data gves the final proof;
	ArrayList<MerkleProofChunk> mBranch;
	
	public MerkleProof(MiniData zData) {
		mData   = zData;
		mBranch = new ArrayList<>();
	}
	
	public void addBranchHash(MiniHash zHash, boolean zLeftRight) {
		
	}
	
	public int getProofLen() {
		return mBranch.size();
	}
	
	public MiniHash calculateFinalHash() {
		return null;
	}
	
}

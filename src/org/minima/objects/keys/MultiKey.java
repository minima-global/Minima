package org.minima.objects.keys;

import java.io.IOException;

import org.minima.database.mmr.MMRSet;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.BaseConverter;
import org.minima.utils.Crypto;
import org.minima.utils.Maths;
import org.minima.utils.MinimaLogger;

public class MultiKey extends BaseKey {
	
	public static final MiniNumber DEFAULT_KEYS_PER_LEVEL = new MiniNumber(16);
	public static final MiniNumber DEFAULT_LEVELS 		  = new MiniNumber(3);
	
	//The Leaf Node Keys..
	SingleKey[] mSingleKeys;
	
	//The Current Leaf Node being used..
	int mCurrentLeaf = -1;
	
	//The Current MultiKey for the leaf node..
	MultiKey mCurrentChildTree = null;
	
	//The signature of the Root of the current base
	MiniData mCurrentPublicKey = null;
	MiniData mCurrentSignature = null;
	MiniData mCurrentProof     = null;
	
	//The MMR tree of keys
	MMRSet mMMR;
	
	/**
	 * For verification only can start like this..
	 */
	public MultiKey() {}

	/**
	 * Create a MultiKey for verification with this public key
	 * @param zPublicKey
	 */
	public MultiKey(MiniData zPublicKey) {
		setPublicKey(zPublicKey);
	}

	/**
	 * Use Default settings to create a Multi-Key
	 * @param zBitLength
	 */
	public MultiKey(int zBitLength) {
		this(MiniData.getRandomData(zBitLength/8), DEFAULT_KEYS_PER_LEVEL, DEFAULT_LEVELS);
	}
	
	public MultiKey(int zBitLength, MiniNumber zKeysPerLevel, MiniNumber zLevel) {
		this(MiniData.getRandomData(zBitLength/8), zKeysPerLevel, zLevel);
	}

	public MultiKey(MiniData zPrivateSeed, MiniNumber zKeysPerLevel, MiniNumber zLevel) {
		super();
		
		//Set important values
		mMaxUses  = zKeysPerLevel;
		mLevel    = zLevel;
		mUses     = MiniNumber.ZERO;
		
		initKeys(zPrivateSeed);
	}
	
	@Override
	protected void initKeys(MiniData zPrivateSeed) {
		//Can calculate from the Private Seed
		mBitLength = new MiniNumber(zPrivateSeed.getLength()*8);
		
		//Store it
		mPrivateSeed = zPrivateSeed;
		
		//Create the Key Tree
		mSingleKeys = new SingleKey[mMaxUses.getAsInt()];
		
		//Now create the MMR tree
		mMMR = new MMRSet(mBitLength.getAsInt());
				
		//Create all the keys..
		int len = mMaxUses.getAsInt();
		for(int i=0;i<len;i++) {
			//Create the private seed
			MiniData spriv = getHashNumberConcat(i,mPrivateSeed, mBitLength.getAsInt());
			
			//Create the Key
			mSingleKeys[i] = new SingleKey(spriv);	
			
			//Add to the tree
			mMMR.addLeafNode(mSingleKeys[i].getPublicKey());
		}
		
		//Finalise the set
		mMMR.finalizeSet();
		
		//Get the root of the tree..
		mPublicKey = mMMR.getMMRRoot().getFinalHash();
	}
	
	public static int totalsigns = 0;
	
	@Override
	public MiniData sign(MiniData zData) {
		//Which key are we on..
		MiniNumber keynum = getUses();
		
		//Once used you cannot use it again..
		incrementUses();
		
		//How many signatures per leaf..
		MiniNumber perleaf = getMaxUses().pow(getLevel().decrement().getAsInt());
		
		//Which leaf node are we using..
		MiniNumber leafnode = keynum.div(perleaf).floor();
		
		if(leafnode.isMoreEqual(getMaxUses())) {
			MinimaLogger.log("SERIOUS ERROR Key "+getPublicKey().to0xString()
					+" used too many times! MAX USES:"+getMaxUses()+" LEVELS:"+mLevel
					+" ALLOWED:"+keynum+"<"+getTotalAllowedUses());
			
			//RESET THE KEY LIMIT.. no point saying NO as coins can get stuck..
			setUses(MiniNumber.ZERO);
			
			//Which key are we on..
			keynum = getUses();
			
			//Once used you cannot use it again..
			incrementUses();
			
			//Which leaf node are we using..
			leafnode = keynum.div(perleaf).floor();
		}
		
		//Are we top level
		int lf = leafnode.getAsInt();
		
		if(getLevel().isEqual(MiniNumber.ONE)) {
			//Sign the data with this key
			mCurrentPublicKey  = mSingleKeys[lf].getPublicKey();
			mCurrentSignature  = mSingleKeys[lf].sign(zData);
			mCurrentProof      = mMMR.getProof(leafnode).getChainSHAProof();
			
			//Create a multi sig.. no child signature
			MultiSig sig = new MultiSig(mCurrentPublicKey, mCurrentProof, mCurrentSignature);
			
			//Return this..
			return sig.getCompleteSig();
		}
		
		//Are we on the correct leaf or a new one..
		if(lf != mCurrentLeaf) {
			//Store
			mCurrentLeaf = lf;
			
			//Create the private seed - from the single key private key and the position..
			MiniData treepriv = getHashNumberConcat(mCurrentLeaf,
													mSingleKeys[mCurrentLeaf].getPrivateSeed(), 
													mBitLength.getAsInt());
			
			//Create a new Multi Key at this leaf position..
			mCurrentChildTree = new MultiKey(treepriv, getMaxUses(), getLevel().decrement()); 
			
			//And set the correct Use number.. could have just been loaded
			mCurrentChildTree.setUses(keynum.modulo(perleaf));
			
			//Get the Base..
			MiniData rootkey = mCurrentChildTree.getPublicKey();
			
			//Sign that..
			mCurrentPublicKey  = mSingleKeys[mCurrentLeaf].getPublicKey();
			mCurrentSignature  = mSingleKeys[mCurrentLeaf].sign(rootkey);
			mCurrentProof      = mMMR.getProof(new MiniNumber(mCurrentLeaf)).getChainSHAProof();
		}	
		
		//Use the current base 
		MiniData childsignature = mCurrentChildTree.sign(zData);
		
		//Create a multi sig..
		MultiSig sig = new MultiSig(mCurrentPublicKey, mCurrentProof, mCurrentSignature, childsignature);
		
		//Return this..
		return sig.getCompleteSig();
	}
	
	/**
	 * Create a deterministic seed from a private seed and a number
	 * 
	 * @param zNumber
	 * @param zData
	 * @param zBitStrength
	 * @return
	 */
	protected MiniData getHashNumberConcat(int zNumber, MiniData zData, int zBitStrength) {
		MiniData numberdata = new MiniData(BaseConverter.numberToHex(zNumber));
		MiniData newdata    = numberdata.concat(zData);
		byte[] hashdata     = Crypto.getInstance().hashData(newdata.getData(), zBitStrength);
		return new MiniData(hashdata);
	}

	@Override
	public boolean verify(MiniData zData, MiniData zMultiSignature) {
		//Convert into a MultiSig Structure
		MultiSig sigdata;
		try {
			sigdata = new MultiSig(zMultiSignature);
		} catch (IOException e) {
			MinimaLogger.log(e);
			return false;
		}
		
		//First check the Public Key is correct
		if(!sigdata.getRootKey().isEqual(mPublicKey)) {
			return false;
		}
		
		//Create a Single Key
		SingleKey skey = new SingleKey();
		skey.setPublicKey(sigdata.getPublicKey());
		
		//And now check the children
		if(sigdata.hasChildSignature()) {
			//The Child Signature
			MiniData childsig = sigdata.getChildSignature();
			
			//Convert to a multisig
			MultiSig msig;
			try {
				msig = new MultiSig(childsig);
			} catch (IOException e) {
				MinimaLogger.log(e);
				return false;
			}
			
			MiniData rootkey = msig.getRootKey();
			
			//Now check the Signature was used to sign the root of the child tree
			if(!skey.verify(rootkey, sigdata.getSignature())) {
				return false;
			}

			//Create a new MultiKey for the child tree
			MultiKey child = new MultiKey();
			child.setPublicKey(rootkey);
			
			//Now check the child signed this data
			return child.verify(zData, childsig);
		}
		
		//Just check the data was signed 
		return skey.verify(zData, sigdata.getSignature());
	}
	
	
	public static void main(String[] zargs) {		
		
		//get some data
		MiniData privseed = MiniData.getRandomData(20);
		long timenow      = System.currentTimeMillis();
		long timediff     = 0;
		
		System.out.println("MAKE KEY Start");
		MultiKey mkey = new MultiKey(privseed, new MiniNumber("3"), new MiniNumber("3"));
//		MultiKey mkey = new MultiKey(privseed, new MiniNumber("16"), new MiniNumber("2"));
		System.out.println(mkey.toJSON().toString());
		
		//Timer..
		timediff = System.currentTimeMillis()-timenow;
		System.out.println("Creation : "+Maths.ConvertMilliToTime(timediff));
		
		//get some data
		MiniData data = MiniData.getRandomData(20);
		System.out.println("Data    : "+data);
		System.out.println();
			
//		//SINGLE SIG EXAMPLE
//		timenow  = System.currentTimeMillis();
//		MiniData singlesig = mkey.sign(data);
//		System.out.println("SigLength:"+singlesig.getLength());
//		System.out.println(mkey.toJSON().toString());
//		System.out.println();
//		
//		//Timer..
//		timediff = System.currentTimeMillis()-timenow;
//		System.out.println("Sign Speed : "+Maths.ConvertMilliToTime(timediff));
//			
//		//Now Verify..
//		MultiKey verifykey = new MultiKey();
//		verifykey.setPublicKey(mkey.getPublicKey());
//		timenow  = System.currentTimeMillis();
//		boolean ok = verifykey.verify(data, singlesig);
//		timediff = System.currentTimeMillis()-timenow;
//		System.out.println("Verify Speed : "+Maths.ConvertMilliToTime(timediff)+" "+ok);
//		
//		//Stop Here..
//		if(true) {System.exit(0);}
		
		//MULTI SIGN EXAMPLE
		for(int i=0;i<1;i++) {
			MiniData sig = mkey.sign(data);
			System.out.println(i+")\tSigLength:"
					+sig.getLength()+"\thash:"
					+Crypto.getInstance().hashObject(sig,160).to0xString()
					+"\tVerify  : "+mkey.verify(data, sig));
			System.out.println(mkey.toJSON().toString());
			System.out.println();
		}
		
//		System.out.println();
//		System.out.println("Now read it in..");
//		
//		//Now write the key our..
//		MultiKey lodkey = new MultiKey(); 
//		try {
//			ByteArrayOutputStream baos = new ByteArrayOutputStream();
//			DataOutputStream dos = new DataOutputStream(baos);
//			
//			mkey.writeDataStream(dos);
//			
//			byte[] mdata = baos.toByteArray();
//			
//			dos.close();
//			
//			//Now read it back in..
//			ByteArrayInputStream bais = new ByteArrayInputStream(mdata);
//			DataInputStream dis = new DataInputStream(bais);
//			
//			lodkey.readDataStream(dis);
//			
//			dis.close();
//			
//		}catch(Exception exc) {
//			exc.printStackTrace();
//		}
//		
//		mkey = null;
//		System.out.println(lodkey.toJSON().toString());
//		
//		for(int i=0;i<5;i++) {
//			MiniData sig = lodkey.sign(data);
//			System.out.println(i+")\tSigLength:"
//					+sig.getLength()+"\thash:"
//					+Crypto.getInstance().hashObject(sig,160).to0xString()
//					+"\tVerify  : "+lodkey.verify(data, sig));
//			System.out.println(lodkey.toJSON().toString());
//			System.out.println();
//		}
		
	}

	
}

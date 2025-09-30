package org.minima.objects.keys;

import java.util.ArrayList;
import java.util.Collections;

import org.bouncycastle.crypto.digests.SHA3Digest;
import org.bouncycastle.pqc.crypto.gmss.util.WinternitzOTSVerify;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;

public class TreeKey {

	/**
	 * Maximum Levels for any Key allowed
	 */
	public static final int MAX_KEY_LEVELS		 = 8;
	
	/**
	 * Default Values
	 */
	public static final int DEFAULT_KEYSPERLEVEL = 64;
	public static final int DEFAULT_LEVELS 		 = 3;
	
	public static TreeKey createDefault(MiniData zPrivateSeed) {
		return new TreeKey(zPrivateSeed, DEFAULT_KEYSPERLEVEL, DEFAULT_LEVELS);
	}
	
	/**
	 * The ROOT of the Tree of Keys
	 */
	TreeKeyNode mRoot;
	
	int mLevels;
	int mKeysPerLevel;
	int mUses;
	int mMaxUses;
	
	MiniData mPrivateSeed;
	MiniData mPublicKey;
	
	public TreeKey() {}
	
	public TreeKey(MiniData zPrivateSeed, int zKeyNum, int zLevels) {
		
		//Levels and Keys
		mLevels 		= zLevels;
		mKeysPerLevel 	= zKeyNum;
		
		//Check maximum
		if(mLevels>MAX_KEY_LEVELS) {
			throw new IllegalArgumentException("Too many key Levels "+mLevels+" MAX:"+MAX_KEY_LEVELS);
		}
		
		mUses			= 0; 
		mMaxUses 		= (int) Math.pow(mKeysPerLevel, mLevels);
		
		//Store..
		mPrivateSeed = zPrivateSeed;
				
		//Initialise root
		mRoot = new TreeKeyNode(zPrivateSeed, mKeysPerLevel);
		
		//Get the Public Key.,.
		mPublicKey = mRoot.getPublicKey();
	}
	
	public void setPublicKey(MiniData zPublicKey) {
		mPublicKey = zPublicKey;
	}
	
	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData getPrivateKey() {
		return mPrivateSeed;
	}
	
	public int getMaxUses() {
		return mMaxUses;
	}
	
	public int getUses() {
		return mUses;
	}
	
	public void setUses(int zUses) {
		mUses = zUses;
	}
	
	public int getSize() {
		return mKeysPerLevel;
	}
	
	public int getDepth() {
		return mLevels;
	}
	
	public Signature sign(MiniData zData) {
		
		//Check range
		if(mUses >= mMaxUses) {
			MinimaLogger.log("SERIOUS ERROR : MAX TREEKEYS USED @ "+mPublicKey);
			mUses = 0;
		}
		
		//Get the Correct Node path..
		ArrayList<Integer> nodes = baseConversion(mUses, mKeysPerLevel, mLevels);
//		MinimaLogger.log("KEY TREE SIGN : "+mUses+" "+nodes.toString());
		
		//All the signatures..
		Signature signature = new Signature();
		
		//Now get those Nodes..
		TreeKeyNode current = mRoot;
		int depth 			= 1;
		for(Integer node : nodes) {
			
			//The node..
			int keynum = node.intValue();
			
			//Get the required key
			Winternitz wots = current.getWOTSKey(keynum);
			
			//The Public Key
			MiniData sigpubkey = wots.getPublicKey(); 
			
			//Get the MMRProof..
			MMRProof proof = current.getProof(keynum);
			
			//Is this the final node
			if(depth == mLevels) {
				
				//Sign the actual Data
				MiniData sigdata = wots.sign(zData);
				
				//Create a signature object
				SignatureProof sig = new SignatureProof(sigpubkey, sigdata, proof);
				
				//Add it..
				signature.addSignatureProof(sig);
				
			}else {
				
				//Get the correct child node..
				TreeKeyNode child = current.getChild(keynum);
				
				//Do we need to sign it.. ( only need to do this once is reused multiple times )
				if(!child.childSigExists()) {
					
					//Get the child's Public Key
					MiniData data = child.getPublicKey();
					
					//Sign the root of the child tree
					MiniData sigdata = wots.sign(data);
				
					//Create the signature object
					SignatureProof childsig = new SignatureProof(sigpubkey, sigdata, proof);
					
					//Set it for next time..
					child.setParentChildSig(childsig);
				}
				
				//Get the parent child sig
				SignatureProof parentchild = child.getParentChildSig();
				
				//Add it..
				signature.addSignatureProof(parentchild);
				
				//New current node
				current = child;
			}
			
			depth++;
		}
		
		//One signature done
		mUses++;
		
		//Return that..
		return signature;
	}


	public boolean verify(MiniData zData, Signature zSignature) {
		
		//Cycle through..
		int total = zSignature.getAllSignatureProofs().size();
		if(total>MAX_KEY_LEVELS) {
			MinimaLogger.log("[!] INVALID KEY found with "+total+" levels MAX:"+MAX_KEY_LEVELS);
			return false;
		}
		
		for(int depth=0;depth<total;depth++) {
			
			//Get the signature
			SignatureProof sigproof = zSignature.getAllSignatureProofs().get(depth);
			
			//Check this root public key is the one we need
			if(depth == 0) {
				
				//Check this is the MAIN public Key
				if(!sigproof.getRootPublicKey().isEqual(mPublicKey)) {
					return false;
				}
			}

			//Is this the last Signature
			if(depth == total-1) {
				
				//The LAST signature signs the actual DATA
				return Winternitz.verify(sigproof.getPublicKey(), zData, sigproof.getSignature());
				
			}else {
				
				//Any Signature but the last signs the child root public key
				SignatureProof childsig = zSignature.getAllSignatureProofs().get(depth+1);
				
				//Check this is what is signed..
				if(!Winternitz.verify(sigproof.getPublicKey(), childsig.getRootPublicKey(), sigproof.getSignature())) {
					return false;
				}
			}
		}
		
		return false;
	}
	

	/**
	 * Base converter to tell which nodes in the tree to use..
	 */
	private static ArrayList<Integer> baseConversion(int zNum, int zBase, int zLevels){
		ArrayList<Integer> ret = new ArrayList<>();
		
		int counter = zNum;
		while(counter != 0) {
			int div 	= counter / zBase;
			int remain 	= counter - (div * zBase);
			ret.add(remain);
			counter = div;
		}
		
		//Do we have it..
		int sizediff = zLevels - ret.size();
		for(int i=0;i<sizediff;i++) {
			ret.add(0);
		}
		
		//Reverse
		Collections.reverse(ret);
		
		return ret;
	}
	
	
	public static void main(String[] zArgs) {
		
		//MiniData pubkey  = new MiniData("0x7255D8635AB4D77AB2612BC3182504033E11A32C016CD82EF85E2DFFCBF348E0");
		MiniData pubkey  = new MiniData("0x514565894ADBA13A6D83D2C820AA3C228AA961F373F920876E680DEE7F743CEC");
		
		MiniData data 	 = new MiniData("0x805240B03B24D4F980075C4306FA586553176411870B8B144A0115354920D2CA");
		
		MiniData sigdata = new MiniData("0x35C95A0B3AC16128297FC798F2F9CEF52CD19C10965F3CACC101452198577B9E4"+
										"4996345E735EF5B20CD4848E029249809D5D01FB8E1FA8007F526219CE7F87E8745"+
										"9ABF27C9A0E29A1B21515BAB0BF7A308CD31693CAAAEF887E12A5D5BE90FEFA72A3"
										+"A5AB76730BC9EA8CBDEDC887852AB288DD17A33AE7A787B7ECEC11BBA999CF3DF3"
										+"165CCEE360D47979F8A9CEF9A4CC6497B9BE9B920AA3061C3F56B008224E863FC3"
										+"47A278F9B9F48B0294C06ACCEE979698290CD2CEF0B77729DC76C0175F1BBF5BED"
										+"0981CA577F7079CAC5F81248856F8F6555B8241E944334E115AD66C2DC007EEE8A"
										+"60143FFD4A8E0499B1D4BC023AB42B4105C0C74113ADCC074A50F6D45949844A6F"
										+"D820E12AD5C7885E1D40B2673CD9E76EFC5E72CA1CF3ABA6C4FDE4F1830A283FFE"
										+"D718BDE5C7BEBF80A83FEC2323D8F4A43761FE13F90AE794CBAABFD1C058262C96"
										+"F340A216644B374469966857D323534CD85C07BAC30293A4D7BCF8FF8E3496BF32"
										+"7EDF6468437F7AD0A0DC3D28722D9DAD480A4D309DD57B252EBC1F38D218B2CFE1"
										+"C59E6DF2D054D1EBCA4C87B921A6E3A6A66B444DD8F1BE903122F2833DCA156858"
										+"B9D51B55E20ECDE8CAEB3F37990E66698B53F37E6B306F0F07B1AAA73FE1B41341"
										+"3D0498C87F8E6D430AEEC66082AF192A67A88F561AADC3DFF328962F9EC233BC6E"
										+"F8CA81C6C7BD9F65603E2D6F28FAC9A34CAE579DCC34E73D604AA554DB518C1AE5"
										+"8EE94F1DF032670A9497077FA8AB6C5417A6029FF27AC0EBFD28F34C3C442ED21A"
										+"D927934BE2B4EA83D01E944E30D7EEF042098678D8D6F7597F14B9AFF84FA29528"
										+"CDE35BC8463371D610059D47B5CC5631B59EE73E069BCA345D190E2AB7BB64AF58"
										+"5D7BBE0A8701C17E9E3B1CFA3EF7E8EF223749A582F58F7876D3CED4B3A7876CB3"
										+"3DB20930DCD99A4F698EDACBB79492357B408A0E22E18715276454AB388587E2F7"
										+"68DD321A3911302D4DA2F31F8CE591D6548612565C895BEF9DAE8A11277DED871F"
										+"52341E7A1580AA4AE2B1AD6F8824981F26E443654E322AF729FC0189A28CBF21C8"
										+"0565B7755CB7941A887485A9F2575529150FBBAD7B957B7B2E16B08611EF2BB63C"
										+"579F1BD40555092ADF4BB32D9EB89F4380EF42D1AFCEED1239C23703B2417D4993D0EBE666DD72E35ADC339A023437A5E5C85C354B0A4CF031A19D8856BCFD7FCD73E357953CC0821E48424763305ECE1F9285131D42BA937F05F4232ACAD1428AEEFE0F2A75275D1DD488D70425DDAEE294FF3103E8E018C5217690D4DF2EFE1B624F16E5821B89DBDECC8600880C7E7B026D572E8F48388334F4EDFBEF4B8EE479E8DF0EDAD3FFBFFF01852341E87C78F26081544FBF3C1E99883FBB6D52FB0DEEEC7A76382A21A6F1522B38097A78F0466EB200769CAFFDF7322D92F0E4E39036AC0EA38590026B9FC6EAE9079B79CCE472FDEE17247F90A72469C1316388638F394856C11948F09F3C35F98BBA305FE51C1FBC0FB35E03065B168F5A8AB3AA3555ACE8E0BCE");
		
		MMRProof pf = new MMRProof();
		pf.addProofChunk(false, new MMRData(new MiniData("0x98C5B1826A1BDFFF0D67110010E082B4323C49460AE0B0C1739533F05B5B84A7"), MiniNumber.ZERO));
		pf.addProofChunk(false, new MMRData(new MiniData("0xB25130502BC380AFD3EA2BFF65DF5A55A0DA8E1DEFCDE2B034194B6492B4C539"), MiniNumber.ZERO));
		pf.addProofChunk(false, new MMRData(new MiniData("0x984CFF92F6994CB3673F8DDDF7D53BF2D28B8A0AE3C736D3D3F3CA45C0F99502"), MiniNumber.ZERO));
		pf.addProofChunk(false, new MMRData(new MiniData("0xF0BB4F2A78C657F3B00643815AD8929C62531F11606BC82C44D9E5FB4FFC4DC6"), MiniNumber.ZERO));
		pf.addProofChunk(false, new MMRData(new MiniData("0x555C49F757FF2956D19F6CD147760A479FCAF643821D115E777B4531B87DF7EC"), MiniNumber.ZERO));
		pf.addProofChunk(false, new MMRData(new MiniData("0xF894A117DEDB880D8E6202F051A328E3BB47322660B49B5CA636384B8E1D58AB"), MiniNumber.ZERO));
		
		//boolean ver = Winternitz.verify(pubkey, data, sig);
		SignatureProof sp = new SignatureProof(pubkey, sigdata, pf);
		
		Signature sig = new Signature();
		sig.addSignatureProof(sp);
		
		TreeKey tk = new TreeKey();
		tk.setPublicKey(sp.getRootPublicKey());
		
		boolean valid = tk.verify(data, sig);
		
		System.out.println("valid : "+valid+" len:"+sigdata.getLength());
		
		//Create a sig..
		
		
		//WOTS Verify
		WinternitzOTSVerify wver 	= new WinternitzOTSVerify(new SHA3Digest(256), 8);
		byte[] newpubkey 			= wver.Verify(data.getBytes(), sigdata.getBytes());
		MiniData resp 				= new MiniData(newpubkey);
		
		System.out.println("root : "+sp.getRootPublicKey().to0xString());
		
		System.out.println("orig : "+pubkey.to0xString());
		System.out.println("new  : "+resp.to0xString());
	
		
		/*MiniData privatekey = new MiniData("0x7255D8635AB4D77AB2612BC3182504033E11A32C016CD82EF85E2DFFCBF348E0");
		Winternitz wintz 	= new Winternitz(privatekey);
		MiniData pubkey 	= wintz.getPublicKey(); 
		
		MiniData data, sig;
		
		data 		= new MiniData("0x00");
		sig 		= wintz.sign(data);
		System.out.println(sig.to0xString()+"\n");
		
		data 		= new MiniData("0x01");
		sig 		= wintz.sign(data);
		System.out.println(sig.to0xString()+"\n");
		
		data 		= new MiniData("0x02");
		sig 		= wintz.sign(data);
		System.out.println(sig.to0xString()+"\n");
		
		data 		= new MiniData("0x03");
		sig 		= wintz.sign(data);
		System.out.println(sig.to0xString()+"\n");
		
		//Check it..
		//boolean ver = Winternitz.verify(pubkey, data, sig);
		//System.out.println(ver);
		*/
		
	}
}

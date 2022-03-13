package org.minima.kissvm;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;

import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.statements.StatementParser;
import org.minima.kissvm.tokens.ScriptToken;
import org.minima.kissvm.tokens.ScriptTokenizer;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class Contract {
	
	//The Transaction this Contract is being run on
	Transaction mTransaction;
	
	//The Witness
	Witness mWitness;
		
	//The final version of the script
	String mRamScript; 
		
	//The Complete contract code, from root level
	StatementBlock mBlock;
	
	//A list of valid signatures
	ArrayList<HexValue> mSignatures;
	
	//A list of all the user-defined variables
	Hashtable<String, Value> mVariables;
	
	//A list of all the global variables available to scripts, like Blocknumber etc..
	Hashtable<String, Value> mGlobals;
		
	//The previous state variables - accessed from the MMR data
	ArrayList<StateVariable> mPrevState = new ArrayList<StateVariable>();
	
	//Is this MonoTonic
	boolean mMonotonic = true;
	
	//Has the Script returned TRUE or FALSE
	private boolean mSuccess;
	private boolean mSuccessSet;
	
	/**
	 * Trace shows debug info as the program is parsed and executed
	 */
	boolean mTraceON = false;
	
	/**
	 * Did the contract script parse ok
	 */
	boolean mParseOK;
	
	/**
	 * Was there an Exception error
	 */
	boolean mException;
	String  mExceptionString;
	
	/**
	 * The Number Of Instructions!
	 */
	int mNumInstructions;
	
	/**
	 * Maximum allowed number of KISSVM instructions
	 */
	public int MAX_INSTRUCTIONS = 1024;
	
	/**
	 * A complete log of the contract execution
	 */
	String mCompleteLog="";
	
	/**
	 * Main Constructor
	 * @param zRamScript - the RamScript
	 */
	public Contract(String zRamScript, String zSignatures, Witness zWitness, Transaction zTransaction, ArrayList<StateVariable> zPrevState) {	
		this(zRamScript, new ArrayList<>(), zWitness, zTransaction, zPrevState, false);
	}
	
	public Contract(String zRamScript, String zSignatures, Witness zWitness, Transaction zTransaction, ArrayList<StateVariable> zPrevState, boolean zTraceON) {	
		this(zRamScript, new ArrayList<>(), zWitness, zTransaction, zPrevState, zTraceON);
	}
	
	public Contract(String zRamScript, ArrayList<MiniData> zSignatures, Witness zWitness, Transaction zTransaction, ArrayList<StateVariable> zPrevState) {	
		this(zRamScript, zSignatures, zWitness, zTransaction, zPrevState, false);
	}
	
	public Contract(String zRamScript, ArrayList<MiniData> zSignatures, Witness zWitness, Transaction zTransaction, ArrayList<StateVariable> zPrevState, boolean zTrace) {
		//Trace?
		mCompleteLog ="";
		mTraceON     = zTrace;
		
		//Clean the RamScript
		mRamScript = zRamScript;
		
		mTransaction = zTransaction;
		mWitness     = zWitness;
	
		mSignatures = new ArrayList<>();
		mVariables  = new Hashtable<>();
		mGlobals    = new Hashtable<>();
		
		mBlock      = null;
		mSuccess    = false;
		mSuccessSet = false;
		mParseOK    = false;
		mException  = false;
		mExceptionString = "";
		
		mNumInstructions = 0;
		
		mMonotonic = true;
		
		//Begin..
		traceLog("Contract   : "+mRamScript);
		traceLog("Size       : "+mRamScript.length());
		
		//Transaction..
		traceLog("Transaction   : "+mTransaction.toString());
		traceLog("Witness       : "+mWitness.toString());
		
		//Load the Signatures
		for(MiniData sig : zSignatures) {
			traceLog("Signature : "+sig.to0xString());
			mSignatures.add( new HexValue(sig) );
		}
		
		//State Variables
		ArrayList<StateVariable> svs = mTransaction.getCompleteState();
		for(StateVariable sv : svs) {
			traceLog("State["+sv.getPort()+"] : "+sv.toString());
		}

		//Previous State
		if(zPrevState == null) {
			mPrevState = new ArrayList<StateVariable>();	
		}else {
			mPrevState = zPrevState;
			for(StateVariable sv : mPrevState) {
				traceLog("PrevState["+sv.getPort()+"] : "+sv.toString());
			}	
		}
		
		//Parse the tokens
		try {
			//Tokenize the script
			ScriptTokenizer tokenize = new ScriptTokenizer(zRamScript);
			
			//Tokenize the script
			List<ScriptToken> tokens = tokenize.tokenize();
			
			int count=0;
			for(ScriptToken tok : tokens) {
				traceLog((count++)+") Token : ["+tok.getTokenTypeString()+"] "+tok.getToken());
			}
		
			//Convert this list of Tokens into a list of Statements
			mBlock = StatementParser.parseTokens(tokens);
			
			traceLog("Script token parse OK.");
			mParseOK = true;
			
		} catch (Exception e) {
			mException  = true;
			mExceptionString = e.toString();
			
			traceLog("PARSE ERROR : "+mExceptionString);
		}
	}
	
	public void setGlobals(	MiniNumber zBlock, 
							Transaction zTrx, 
							int zInput, 
							MiniNumber zInputBlkCreate, 
							String zScript) {
		
		//Get the Coin
		Coin cc = zTrx.getAllInputs().get(zInput);
		
		//set the environment
		setGlobalVariable("@BLOCK", new NumberValue(zBlock));
		
		setGlobalVariable("@CREATED", new NumberValue(zInputBlkCreate));
		setGlobalVariable("@COINAGE", new NumberValue(zBlock.sub(zInputBlkCreate)));
		
		setGlobalVariable("@INPUT", new NumberValue(zInput));
		setGlobalVariable("@COINID", new HexValue(cc.getCoinID()));
		setGlobalVariable("@AMOUNT", new NumberValue(cc.getAmount()));
		setGlobalVariable("@ADDRESS", new HexValue(cc.getAddress()));
		setGlobalVariable("@TOKENID", new HexValue(cc.getTokenID()));
		setGlobalVariable("@SCRIPT", new StringValue(zScript));
		
		setGlobalVariable("@TOTIN", new NumberValue(zTrx.getAllInputs().size()));
		setGlobalVariable("@TOTOUT", new NumberValue(zTrx.getAllOutputs().size()));
	}
	
	public void setGlobalVariable(String zGlobal, Value zValue) {
		mGlobals.put(zGlobal, zValue);
		traceLog("Global ["+zGlobal+"] : "+zValue);
	}
	
	public Value getGlobal(String zGlobal) throws ExecutionException {
		Value ret = mGlobals.get(zGlobal);
		if(ret==null) {
			throw new ExecutionException("Global not found - "+zGlobal);
		}
		
		//Will this break monotonic
		if(zGlobal.equals("@BLOCK") || zGlobal.equals("@COINAGE") || zGlobal.equals("@CREATED")) {
			mMonotonic = false;
		}
		
		return ret;
	}
	
	public Hashtable<String, Value> getGlobalVariables() {
		return mGlobals;
	}
	
	public void setAllGlobalVariables(Hashtable<String, Value> zGlobals) {
		mGlobals = zGlobals;
	}
	
	public boolean isParseOK() {
		return mParseOK;
	}
	
	public boolean isException() {
		return mException;
	}
	
	public String getException() {
		return mExceptionString;
	}
	
	public boolean isTrace() {
		return mTraceON;
	}
	
	public void traceLog(String zLog) {
		if(isTrace()) {
			MinimaLogger.log("INST["+mNumInstructions+"] - "+zLog);
		}
		
		//Store the complete Log
		mCompleteLog += "INST["+mNumInstructions+"] - "+zLog+"\n";
	}
	
	public String getCompleteTraceLog() {
		return mCompleteLog;
	}
	
	public void incrementInstructions() throws ExecutionException {
		mNumInstructions++;
		if(mNumInstructions > MAX_INSTRUCTIONS) {
			throw new ExecutionException("MAX instruction number reached! "+mNumInstructions);
		}
	}
	
	public int getNumberOfInstructions() {
		return mNumInstructions;
	}
	
	public void setMaxInstructions(int zMax) {
		MAX_INSTRUCTIONS = zMax;
	}
	
	public void run() {
		if(!mParseOK) {
			traceLog("Script parse FAILED. Please fix and retry.");
			return;
		}
		
		//Run the code block
		try {
			traceLog("Start executing the contract");
			
			mBlock.run(this);
			
		} catch (Exception e) {
			if(mTraceON) {
				MinimaLogger.log(e);
			}
			
			mException = true;
			mExceptionString = e.toString();
			
			//AUTOMATIC FAIL
			traceLog("Execution Error - "+e);
			
			mSuccess 	= false;
			mSuccessSet = true;
		}
		
		//How'd it go..
		traceLog("Contract instructions : "+mNumInstructions);
		traceLog("Contract finished     : "+mSuccess);
	}
	
	public void setRETURNValue(boolean zSUCCESS) {
		if(!mSuccessSet) {
			mSuccess 	= zSUCCESS;
			mSuccessSet = true;
		}
	}
	
	public boolean isSuccess() {
		return mSuccess;
	}
	
	public boolean isSuccessSet() {
		return mSuccessSet;
	}
	
	public boolean isMonotonic() {
		return mMonotonic;
	}
		
	public String getMiniScript() {
		return mRamScript;
	}
	
	public Transaction getTransaction() {
		return mTransaction;
	}
	
	public Witness getWitness() {
		return mWitness;
	}
	
	public JSONObject getAllVariables() {
		JSONObject variables = new JSONObject();
		
		//First the normal variables
		Enumeration<String> keys = mVariables.keys();
		while(keys.hasMoreElements()) {
			//Get the Key
			String key = keys.nextElement();
			
			//Get the Value
			Value val = mVariables.get(key);
			
			//Remove the commas for JSON formating
			if(key.contains(",")) {
				key = key.replace(",", " ");
				key = "( "+key.trim()+" )";
			}
			
			variables.put(key, val.toString());
//			if(val.getValueType() == ScriptValue.VALUE_SCRIPT) {
//				variables.put(key, "[ "+val.toString()+" ]");
//			}else{
//				variables.put(key, val.toString());
//			}
		}
		
		return variables;
	}
	
	public void removeVariable(String zName) throws ExecutionException {
		mVariables.remove(zName);
	}
	
	public boolean existsVariable(String zName) throws ExecutionException {
		return mVariables.containsKey(zName);
	}
	
	public Value getVariable(String zName) throws ExecutionException {
		Value ret = mVariables.get(zName);
		return ret;
	}
	
	public void setVariable(String zName, Value zValue) {
		mVariables.put(zName, zValue);
		traceVariables();
	}
	
	/**
	 * Get the Parameter Value as a MiniNumber or throw exception if not a NUmber 
	 * @throws ExecutionException 
	 */
	public NumberValue getNumberParam(int zParamNumber, MinimaFunction zFunction) throws ExecutionException {
		Value vv = zFunction.getParameter(zParamNumber).getValue(this);
		if(vv.getValueType() != Value.VALUE_NUMBER) {
			throw new ExecutionException("Incorrect Parameter type - should be NumberValue @ "+zParamNumber+" "+zFunction.getName());
		}
		return (NumberValue)vv;
	}
	
	public HexValue getHexParam(int zParamNumber, MinimaFunction zFunction) throws ExecutionException {
		Value vv = zFunction.getParameter(zParamNumber).getValue(this);
		if(vv.getValueType() != Value.VALUE_HEX) {
			throw new ExecutionException("Incorrect Parameter type - should be HEXValue @ "+zParamNumber+" "+zFunction.getName());
		}
		return (HexValue)vv;
	}
	
	public StringValue getStringParam(int zParamNumber, MinimaFunction zFunction) throws ExecutionException {
		Value vv = zFunction.getParameter(zParamNumber).getValue(this);
		if(vv.getValueType() != Value.VALUE_SCRIPT) {
			throw new ExecutionException("Incorrect Parameter type - should be ScriptValue @ "+zParamNumber+" "+zFunction.getName());
		}
		return (StringValue)vv;
	}
	
	public BooleanValue getBoolParam(int zParamNumber, MinimaFunction zFunction) throws ExecutionException {
		Value vv = zFunction.getParameter(zParamNumber).getValue(this);
		if(vv.getValueType() != Value.VALUE_BOOLEAN) {
			throw new ExecutionException("Incorrect Parameter type - should be BooleanValue @ "+zParamNumber+" "+zFunction.getName());
		}
		return (BooleanValue)vv;
	}
	
	
	public Value getState(int zStateNum) throws ExecutionException {
		if(!mTransaction.stateExists(zStateNum)) {
			throw new ExecutionException("State Variable does not exist "+zStateNum);
		}

		//Get it from the Transaction..
		String stateval = mTransaction.getStateValue(zStateNum).toString();
		
		//Clean it..
		return Value.getValue(stateval);
	}
	
	public Value getPrevState(int zPrev) throws ExecutionException {
		//Get the state variable..
		for(StateVariable sv : mPrevState) {
			if(sv.getPort() == zPrev) {
				//Clean it..
				String stateval = sv.toString();
				
				//Work it out
				return Value.getValue(stateval);
			}
		}
		
		throw new ExecutionException("PREVSTATE Missing : "+zPrev);
	}
	
	/**
	 * Could use the JSON but this looks better as no quotes.. ;p
	 */
	public void traceVariables() {
		//Output..
		String varlist = "{ ";
		
		//First the normal variables
		Enumeration<String> keys = mVariables.keys();
		while(keys.hasMoreElements()) {
			//Get the Key
			String key = keys.nextElement();
			
			//Get the Value
			Value val = mVariables.get(key);
			
			//Remove the commas for JSON formating
			if(key.contains(",")) {
				key = key.replace(",", " ");
				key = "( "+key.trim()+" )";
			}
			
			//Log it.. 
			int type = val.getValueType();
			varlist += key+" = "+val+", ";
//			switch (type)  {
//				case BooleanValue.VALUE_BOOLEAN :
//					varlist += key+" = "+Boolean.toString(val.isTrue()).toUpperCase()+", ";
//				break;
//				case ScriptValue.VALUE_SCRIPT : xx
//					varlist += key+" = [ "+val+" ], ";
//				break;
//				default:
//					varlist += key+" = "+val+", ";
//				break;
//			}		
		}
		
		traceLog(varlist+"}");
	}
	
	/**
	 * Check if this transaction has been signed by this public key
	 * @param zSignature
	 * @return
	 */
	public boolean checkSignature(HexValue zSignature) {
		MiniData checksig = zSignature.getMiniData();
		
		for(HexValue sig : mSignatures) {
			if(sig.getMiniData().isEqual(checksig)) {
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Clean up a script..
	 * 
	 * @param zScript
	 * @return The Converted Script
	 */
	public static String cleanScript(String zScript) {
		
		//The final result
		StringBuffer ret = new StringBuffer();
		
		//Remove all the excess white space
		String script = zScript.replaceAll("\\s+"," ").trim();
		
		//First CONVERT..
		ScriptTokenizer tokz = new ScriptTokenizer(script, true);
		try {
			//Get the list of Tokens..
			ArrayList<ScriptToken> tokens = tokz.tokenize();
		
			//Now add them correctly..
			boolean first = true;
			boolean whites = true;
			for(ScriptToken tok : tokens) {
				if(tok.getTokenType() == ScriptToken.TOKEN_COMMAND) {
					String command = tok.getToken();
					if(first) {
						ret.append(command+" ");
						first = false;
					}else {
						ret.append(" "+command+" ");
					}
					
					whites = true;
					
				}else if(ScriptTokenizer.BOOLEAN_TOKENS_LIST.contains(tok.getToken())) {
					ret.append(" "+tok.getToken()+" ");
				
					whites = true;
					
				}else if(tok.getToken().startsWith("0x")) {
					String hex = "0x"+tok.getToken().substring(2).toUpperCase();
					
					if(whites) {
						ret.append(hex);
					}else {
						ret.append(" "+hex);
					}
					
					whites = false;
					
				}else {
					String strtok = tok.getToken();
					
					//Is it an end of word or whitespace..
					if(ScriptTokenizer.isWhiteSpace(strtok) || ScriptTokenizer.mAllEOW.contains(strtok)) {
						ret.append(tok.getToken());
						whites = true;
					}else {
						if(whites) {
							ret.append(tok.getToken());
						}else {
							ret.append(" "+tok.getToken());
						}
						whites = false;
					}
				}
			}
		
		} catch (MinimaParseException e) {
			MinimaLogger.log("Clean Script Error @ "+zScript+" "+e);
			return zScript;
		}
		
		return ret.toString().trim();
	}
	
	public static void main(String[] zArgs) {
		
//		String scr = new String("let (a 1 0xFF )  = 4 + -2  let t = concat( 0x00 0x34   0x45  )");
		String scr = new String("let x = $1");
		
		String clean = Contract.cleanScript(scr);
		
		MinimaLogger.log(scr);
		MinimaLogger.log(clean);
		
	}
}

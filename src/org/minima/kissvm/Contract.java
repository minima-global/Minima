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
import org.minima.objects.Token;
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
	 * The STACK depth - how deep can you go
	 */
	public static final int MAX_STACK_DEPTH = 64;
	int mStackDepth = 0;
	
	/**
	 * MAX Function params
	 */
	public static final int MAX_FUNCTION_PARAMS = 32;
	
	/**
	 * MAX size of String or HEX value - 64k
	 */
	public static final int MAX_DATA_SIZE = 64 * 1024;
	
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
			resetStackDepth();
			mBlock = StatementParser.parseTokens(tokens, 0);
			
			traceLog("Script token parse OK.");
			mParseOK = true;
			
		} catch (Exception e) {
			mException  = true;
			mExceptionString = e.toString();
			
			traceLog("PARSE ERROR : "+mExceptionString);
		}
	}
	
	public void setGlobals(	MiniNumber zBlock,
							MiniNumber zBlockTimeMilli,
							Transaction zTrx, 
							int zInput, 
							MiniNumber zInputBlkCreate, 
							String zScript) {
		
		//Get the Coin
		Coin cc = zTrx.getAllInputs().get(zInput);
		
		//set the environment
		setGlobalVariable("@BLOCK", new NumberValue(zBlock));
		setGlobalVariable("@BLOCKMILLI", new NumberValue(zBlockTimeMilli));
		
		setGlobalVariable("@CREATED", new NumberValue(zInputBlkCreate));
		setGlobalVariable("@COINAGE", new NumberValue(zBlock.sub(zInputBlkCreate)));
		
		setGlobalVariable("@INPUT", new NumberValue(zInput));
		setGlobalVariable("@COINID", new HexValue(cc.getCoinID()));
		
		//AMOUNT is the amount of Minima or Token(Scaled)..
		MiniNumber amt = cc.getAmount();
		if(!cc.getTokenID().isEqual(Token.TOKENID_MINIMA)) {
			//Scale the amount
			amt = cc.getToken().getScaledTokenAmount(amt);
		}
		setGlobalVariable("@AMOUNT", new NumberValue(amt));
		
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
		if(	zGlobal.equals("@BLOCK") ||
			zGlobal.equals("@BLOCKMILLI") ||
			zGlobal.equals("@COINAGE")) {
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
			MinimaLogger.log("INST["+mNumInstructions+"] STACK["+getStackDepth()+"] - "+zLog);
		}
		
		//Store the complete Log
		mCompleteLog += "INST["+mNumInstructions+"] - "+zLog+"\n";
	}
	
	public String getCompleteTraceLog() {
		return mCompleteLog;
	}
	
	public void incrementInstructions() throws ExecutionException {
		incrementInstructions(1);
	}
	
	public void incrementInstructions(int zNum) throws ExecutionException {
		mNumInstructions+=zNum;
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
	
	public void resetStackDepth() {
		mStackDepth = 0;
	}
	
	public int getStackDepth() {
		return mStackDepth;
	}
	
	public void incrementStackDepth() throws ExecutionException  {
		mStackDepth++;
		if(mStackDepth>MAX_STACK_DEPTH) {
			throw new ExecutionException("Stack depth too deep! (MAX "+MAX_STACK_DEPTH+") "+mStackDepth);
		}
	}
	
	public void decrementStackDepth() {
		mStackDepth--;
	}
	
	public void run() {
		if(!mParseOK) {
			traceLog("Script parse FAILED. Please fix and retry.");
			return;
		}
		
		//Reset the Stack Depth
		resetStackDepth();
		
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
		return cleanScript(zScript, false);
	}
	
	public static String cleanScript(String zScript, boolean zLog) {
		
		//The final result
		StringBuffer ret = new StringBuffer();
		
		//Remove all the excess white space
		String script = zScript.replaceAll("\\s+"," ").trim();
		
		//Remove all \
//		script = script.replaceAll("\\\\","").trim();
		
		//First CONVERT..
		ScriptTokenizer tokz = new ScriptTokenizer(script, true);
		try {
			//Get the list of Tokens..
			ArrayList<ScriptToken> tokens = tokz.tokenize();
		
			//Now add them correctly..
			boolean whites 		= true;
			ScriptToken prevtok = null;
			for(ScriptToken tok : tokens) {
				
				if(zLog) {
					MinimaLogger.log(tok.toString());
				}
				
				if(tok.getTokenType() == ScriptToken.TOKEN_COMMAND) {
					String command = tok.getToken();

					//Always a space before and after a COMMAND
					if(ret.toString().endsWith(" ")) {
						ret.append(command+" ");
					}else {
						ret.append(" "+command+" ");
					}
					
					whites = true;
					
				}else if(ScriptTokenizer.BOOLEAN_TOKENS_LIST.contains(tok.getToken())) {
					ret.append(" "+tok.getToken()+" ");
				
					whites = true;
					
				}else if(tok.getToken().startsWith("0x")) {
					String hex = "0x"+tok.getToken().substring(2).toUpperCase();
					
					if(prevtok!=null && prevtok.getToken().endsWith(")")) {
						ret.append(" "+hex);
					}else {
						if(whites) {
							ret.append(hex);
						}else {
							ret.append(" "+hex);
						}
					}
					
					whites = false;
					
				}else if(tok.getToken().startsWith("(") || tok.getToken().startsWith("[")) {
					
					String strtok = tok.getToken();
					
					if(whites) {
						boolean isspacerequired = prevtok!=null 
								&& (prevtok.getToken().endsWith(")") || prevtok.getToken().endsWith("]"));
						
						if(isspacerequired) {
							ret.append(" "+strtok);
						}else {
							ret.append(strtok);
						}
						
					}else {
					
						boolean isspacerequired = prevtok!=null 
								&& (ScriptTokenizer.mAllEOW.contains(prevtok.getToken()));
						
						if(isspacerequired) {
							ret.append(" "+strtok);
						}else {
							ret.append(strtok);
						}
					}
					
					whites=true;
				
				}else {
					String strtok = tok.getToken();
					
					boolean islastclosebracket = prevtok!=null && (prevtok.getToken().endsWith(")") || prevtok.getToken().endsWith("]"));
					
					//Is it an end of word or whitespace..
					if(islastclosebracket && !ScriptTokenizer.mAllAFTER.contains(strtok)) {
						
						ret.append(" "+strtok);
						whites = false;
						
					}else if(ScriptTokenizer.isWhiteSpace(strtok) || ScriptTokenizer.mAllEOW.contains(strtok)) {
						ret.append(strtok);
						whites = true;
					}else {
						if(whites) {
							ret.append(strtok);
						}else {
							ret.append(" "+strtok);
						}
						whites = false;
					}
				}
				
				//Keep the last letter of the previous token
				prevtok = tok;
			}
		
		} catch (MinimaParseException e) {
			MinimaLogger.log("Clean Script Error @ "+zScript+" "+e);
			return zScript;
		}
		
		return ret.toString().trim();
	}
	
	public static void main(String[] zArgs) {
		
		
//		String scr =  "LET a = [ LET returnvalue = $1 + $2 ] "
//					+ "LET b = 2 "
//					+ "LET c = 3 "
//					+ "LET z = FUNCTION(a b c) "
//					+ "return true";
		
//		String scr = "LET n=STATE(0) LET m=STATE(1) LET script=[RETURN MULTISIG(]+STRING(n) LET counter=0 WHILE counter LT m DO LET script=script+[ ]+STRING(PREVSTATE(counter+2)) LET counter=INC(counter) ENDWHILE LET script=script+[)] EXEC script";
//		String scr = "WHILE  counter LT m DO  LET  script=script+[ ]+STRING(PREVSTATE(counter+2)) LET counter=INC(counter) ENDWHILE LET script=script+[)] EXEC script";
//		String scr = "IF x LT 6 THEN LET y=5 ENDIF LET f=0";
		//String scr = "LET func=[ LET g = $1 + $2 ] LET hh=REPLACEFIRST(func [$1] [$2] ) LET y=FUNCTION(func 1 2)";
		
		String scr = "LET g = ~0xFF00EE";
//		String scr = "~0xFF ~(0x00)";
		
//		String scr = "LET a = [$1$1$1$1$1$1$1$1$1$1] // the script\r\n"
//				+ "LET b = [$2$2$2$2$2$2$2$2$2$2] // first script parameter\r\n"
//				+ "LET c = [$3$3$3$3$3$3$3$3$3$3]\r\n"
//				+ "LET d = [$4$4$4$4$4$4$4$4$4$4]\r\n"
//				+ "LET e = [$5$5$5$5$5$5$5$5$5$5]\r\n"
//				+ "LET f = [$6$6$6$6$6$6$6$6$6$6]\r\n"
//				+ "LET g = [$7$7$7$7$7$7$7$7$7$7]\r\n"
//				+ "LET h = [$8$8$8$8$8$8$8$8$8$8] // last script parameter\r\n"
//				+ "LET z = FUNCTION(a b c d e f g h)";
		
		//String scr = "LET a=0xFF WHILE TRUE DO LET a = a<<1000 ENDWHILE";
		
//		String scr = "LET a = 0xff<<10000";

		MinimaLogger.log("");
		MinimaLogger.log("Script:"+scr);
		
		String clean = cleanScript(scr,false);
		MinimaLogger.log("Clean :"+clean);
		
		//Run it..
		Contract ctr = new Contract(clean, new ArrayList<>(), new Witness(), new Transaction(), new ArrayList<>(),true);
		ctr.run();
	}
}

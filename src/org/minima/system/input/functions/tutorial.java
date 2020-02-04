package org.minima.system.input.functions;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.minima.system.input.CommandFunction;
import org.minima.utils.MinimaLogger;

public class tutorial extends CommandFunction{

	public static String TUTORIAL_TEXT = "Minima transactions are a series of inputs, a series of outputs and a variable list from 0-255 known as the state, which you can access from the script with GETSTATE. The state can be accessed by all the input scripts, and is stored in the MMR database, so can be accessed by PREVSTATE in the next transaction the outputs are added to, as inputs. \n" + 
			"\n" + 
			"The sum of the outputs must be less than or equal to the sum of the inputs, for every tokenid used. The difference in raw minima is the Burn. \n" + 
			"\n" + 
			"A Minima input script returns true or false. The default is return FALSE, so all inputs must explicitly return true for the transaction to be valid.\n" + 
			"\n" + 
			"A transaction can be signed, in full, by one or more public keys.\n" + 
			"\n" + 
			"Minima allows input scripts to have perfect knowledge of the entire transaction. How many, their token type, the amount and address of all inputs and outputs are available. An input knows it's own script ( @SCRIPT ) and can ensure an output of a similar address exists in the outputs. \n" + 
			"\n" + 
			"Using REPLVAR new addresses can be created by replacing existing variables in the current  or pre-existing scripts, and checking of complex addresses can be achieved by using MAST and a list a variables before the main bulk of the hashed script.\n" + 
			"\n" + 
			"The addition of the state variables in the MMR Proof DB, allow for complex scripts with knowledge of their past to be created.\n" + 
			"\n" + 
			"Minima transactions are complex Logic Gates, with analogue inputs and outputs, a simple yet powerful control language, and a single-shot history state mechanic. I think of them as \"Script Gates\".    \n" + 
			"\n" + 
			"Grammar\n" + 
			"-------\n" + 
			"\n" + 
			"ADDRESS     ::= SHA3 ( BLOCK )\n" + 
			"BLOCK       ::= STATEMENT_1 STATEMENT_2 ... STATEMENT_n\n" + 
			"STATEMENT   ::= LET VARIABLE = EXPRESSION | \n" + 
			"                IF EXPRESSION THEN BLOCK \n" + 
			"                [ELSEIF EXPRESSION THEN BLOCK]* \n" + 
			"                [ELSE BLOCK] ENDIF | \n" + 
			"                MAST BLOCK [ORMAST BLOCK]* ENDMAST |\n" + 
			"                RETURN EXPRESSION\n" + 
			"EXPRESSION  ::= RELATION AND RELATION  | RELATION OR RELATION  |  \n" + 
			"                RELATION XOR RELATION  | RELATION NAND RELATION | \n" + 
			"                RELATION NOR RELATION  |  RELATION NXOR RELATION | RELATION\n" + 
			"RELATION    ::= LOGIC EQ LOGIC  | LOGIC NEQ LOGIC  | \n" + 
			"                LOGIC GT LOGIC  | LOGIC GTE LOGIC  | \n" + 
			"                LOGIC LT LOGIC  | LOGIC LTE LOGIC  | LOGIC\n" + 
			"LOGIC       ::= LOGIC & OPERATION | LOGIC | OPERATION | \n" + 
			"                LOGIC ^ OPERATION | OPERATION\n" + 
			"OPERATION   ::= OPERATION + MULDIV | OPERATION - MULDIV | \n" + 
			"                OPERATION % MULDIV | \n" + 
			"                OPERATION << MULDIV | OPERATION >> MULDIV | MULDIV\n" + 
			"MULDIV      ::= MULDIV * PRIME | MULDIV / PRIME | PRIME\n" + 
			"PRIME       ::= NOT PRIME |  NEG PRIME | BASEUNIT\n" + 
			"BASEUNIT    ::= VAR | VALUE | GLOBAL | FUNCTION | ( EXPRESSION )\n" + 
			"VAR         ::= ^[a-z]{1,10}$\n" + 
			"VALUE       ::= NUMBER | DATA | BINARY\n" + 
			"DATA        ::= HEX | SCRIPT\n" + 
			"BINARY      ::= TRUE | FALSE\n" + 
			"GLOBAL      ::= @BLKNUM | @INPUTNUM |\n" + 
			"      	        @AMOUNT | @ADDRESS | @TOKENID | @COINID |\n" + 
			"                @SCRIPT | @TOTIN | @TOTOUT\n" + 
			"NUMBER      ::= \"^-?\\\\d*(\\\\.\\\\d+)?$\"\n" + 
			"BYTE        ::= [0-255]\n" + 
			"HEX         ::= 0x[0-9A-F]{2}*\n" + 
			"SCRIPT      ::= [ ASCII ]\n" + 
			"FALSE       ::= 0\n" + 
			"TRUE        ::= NOT FALSE\n" + 
			"MASTBLK     ::= $HEX\n" + 
			"FUNCTION    ::= FUNC ( EXPRESSION1 EXPRESSION2 .. EXPRESSIONn ) \n" + 
			"FUNC        ::= CONCAT | LEN | REV | SUBSET | RPLVAR |\n" + 
			"                ASCII | BOOL | HEX | NUMBER | SCRIPT |\n" + 
			"                ABS | CEIL | FLOOR | MIN | MAX | INC | DEC |\n" + 
			"                BITSET | BITGET | PROOF | SHA3 | SHA2 |\n" + 
			"                SIGNEDBY | MULTISIGNEDBY | CHECKSIG |\n" + 
			"                GETOUTADDR | GETOUTAMT | GETOUTTOK | VERIFYOUT |\n" + 
			"                GETINADDR | GETINAMT | GETINTOK | GETINID | VERIFYIN |\n" + 
			"                SUMINTOK | SUMOUTTOK | STATE | PREVSTATE | *DYNSTATE\n" + 
			"\n" + 
			"Globals\n" + 
			"-------\n" + 
			"\n" + 
			"@BLKNUM    : Block number this transaction is in\n" + 
			"@INPUT     : Input number in the transaction\n" + 
			"@AMOUNT    : Amount of this input\n" + 
			"@ADDRESS   : Address of this input\n" + 
			"@TOKENID   : TokenID of this input\n" + 
			"@COINID    : CoinID of this input\n" + 
			"@SCRIPT    : Script for this input\n" + 
			"@TOTIN     : Total number of inputs for this transaction\n" + 
			"@TOTOUT    : Total number of outputs for this transaction\n" + 
			"@INBLKNUM  : Block number this output was created - useful for OP_CSV\n" + 
			"\n" + 
			"Functions\n" + 
			"---------\n" + 
			"\n" + 
			"CONCAT ( DATA DATA )\n" + 
			"Concatenate the 2 data values into 1 value . Both values must be the same DATA type. \n" + 
			"\n" + 
			"LEN ( HEX )\n" + 
			"Length of the data\n" + 
			"\n" + 
			"REV ( HEX )\n" + 
			"Reverse the data\n" + 
			"\n" + 
			"SUBSET ( HEX NUMBER NUMBER )\n" + 
			"Return a subset of the data start length\n" + 
			"\n" + 
			"RPLVAR ( SCRIPT SCRIPT SCRIPT ) \n" + 
			"In a script, replace a variable definition with the following Expression. Can be used on @SCRIPT or other to create a covenant with new variables and check outputs.\n" + 
			"\n" + 
			"ASCII ( HEX )\n" + 
			"Convert the HEX value of a script value to a script\n" + 
			"\n" + 
			"BOOLEAN ( VALUE )\n" + 
			"Convert to TRUE or FALSE value.\n" + 
			"\n" + 
			"HEX ( SCRIPT )\n" + 
			"Convert SCRIPT to HEX\n" + 
			"\n" + 
			"NUMBER ( HEX )\n" + 
			"Convert HEX to NUMBER\n" + 
			"\n" + 
			"SCRIPT ( HEX ) \n" + 
			"Convert a HEX value to SCRIPT\n" + 
			"\n" + 
			"ABS ( NUMBER )\n" + 
			"Return the absolute value of a number\n" + 
			"\n" + 
			"CEIL ( NUMBER )\n" + 
			"Return the number rounded up\n" + 
			"\n" + 
			"FLOOR ( NUMBER ) \n" + 
			"Return the number rounded down\n" + 
			"\n" + 
			"MIN ( NUMBER NUMBER )\n" + 
			"Return the minimum value of the 2 numbers\n" + 
			"\n" + 
			"MAX ( NUMBER NUMBER )\n" + 
			"Return the maximum value of the 2 numbers\n" + 
			"\n" + 
			"INC ( NUMBER )\n" + 
			"Increment a number\n" + 
			"\n" + 
			"DEC ( NUMBER )\n" + 
			"Decrement a number\n" + 
			"\n" + 
			"BITSET ( HEX NUMBER BINARY )\n" + 
			"Set the value of the BIT at that Position to 0 or 1\n" + 
			"\n" + 
			"BITGET ( HEX NUMBER ) \n" + 
			"Get the BINARY value of the bit at the position.\n" + 
			"\n" + 
			"ADDR ( SCRIPT )\n" + 
			"Convert the SCRIPT into a HEX address\n" + 
			"\n" + 
			"CHAINSHA ( HEX HEX ) \n" + 
			"Recursively hash the first HEX value with the proof provided in the second. A proof is a BYTE denoting left or right with a hex data value. Returns the final result that can be checked in script. \n" + 
			"\n" + 
			"SHA3 ( HEX ) \n" + 
			"Returns the SHA3 value of the HEX value\n" + 
			"\n" + 
			"SHA2 ( HEX ) \n" + 
			"Returns the SHA2 value of the HEX value\n" + 
			"\n" + 
			"SIGNEDBY ( HEX )\n" + 
			"Returns true if the transaction is signed by this public key\n" + 
			"\n" + 
			"MULTISIG ( BYTE HEX1 HEX2 .. HEXn )\n" + 
			"Returns true if the transaction is signed by N of the public keys\n" + 
			"\n" + 
			"CHECKSIG ( HEX HEX )\n" + 
			"Check valid signature with provided public key.\n" + 
			"\n" + 
			"GETOUTADDR ( BYTE ) \n" + 
			"Return the HEX address of the specified output\n" + 
			"\n" + 
			"GETOUTAMT ( BYTE ) \n" + 
			"Return the amount of the specified output \n" + 
			"\n" + 
			"GETOUTTOK ( BYTE ) \n" + 
			"Return the token id of the specified output\n" + 
			"\n" + 
			"VERIFYOUT ( BYTE HEX NUMBER HEX )\n" + 
			"Verify the specified output has the specified address, amount and tokenid. \n" + 
			"\n" + 
			"GETINADDR ( BYTE ) \n" + 
			"Return the HEX address of the specified input\n" + 
			"\n" + 
			"GETINAMT ( BYTE ) \n" + 
			"Return the amount of the specified input\n" + 
			"\n" + 
			"GETINTOK ( BYTE ) \n" + 
			"Return the token id of the specified input\n" + 
			"\n" + 
			"VERIFYIN ( BYTE HEX NUMBER HEX )\n" + 
			"Verify the specified input has the specified address, amount and tokenid. \n" + 
			"\n" + 
			"SUMINTOK ( HEX )\n" + 
			"Sum the input values of a certain token \n" + 
			"\n" + 
			"SUMOUTTOK ( HEX )\n" + 
			"Sum the output values of a certain token \n" + 
			"\n" + 
			"STATE ( BYTE )\n" + 
			"Return the state value 0-255\n" + 
			"\n" + 
			"PREVSTATE ( BYTE )\n" + 
			"Return the state value 0-255 of the input stored in the MMR data in the initial transaction this input was created. Allows for a state to be maintained from 1 spend to the next.\n" + 
			"\n" + 
			"*DYNSTATE ( BYTE  EXPRESSION )\n" + 
			"Can be called only once per transaction. Will change the State value to the expression value.  N = N+1. This way rolling transactions are possible. Multiple calls to the same input in the same block.\n" + 
			"\n" + 
			"Examples\n" + 
			"--------\n" + 
			"\n" + 
			"RETURN SIGNEDBY ( 0x12345.. )\n" + 
			"\n" + 
			"IF SIGNEDBY ( 0x123456.. ) AND SIGNEDBY ( 0x987654.. ) THEN\n" + 
			"   RETURN TRUE\n" + 
			"ELSE IF @BLKNUM GT 198765 AND SIGNEDBY ( 0x12345.. ) THEN\n" + 
			"   RETURN TRUE\n" + 
			"ENDIF\n" + 
			"\n" + 
			"LET x = GETSATE ( 23 )\n" + 
			"LET shax = SHA3 ( x )\n" + 
			"IF shax EQ 0x6785456 AND SIGNEDBY ( 0x12345.. ) THEN RETURN TRUE ENDIF\n" + 
			"\n" + 
			"";
	
	public tutorial() {
		super("tutorial");
		setHelp("", "Explain Minima and go through Scripting", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
//		InputStream in = getClass().getClassLoader().getResourceAsStream("org/minima/system/input/functions/tutorial.txt");
//		BufferedReader br = new BufferedReader(new InputStreamReader(in));
//		
//		int linenumber=1;
//		String line  = null;
//        String total = "";
//		while ( (line = br.readLine()) != null) {
//            // do something with the line here
//        	getResponseStream().getDataJSON().put(getLineNumber(linenumber++), line);
//        	
//        	total += line+"\n";
//        }
		
//		getResponseStream().getDataJSON().put("Tutorial", TUTORIAL_TEXT);
//		getResponseStream().endStatus(true, "");
//		
		getResponseStream().hardEndStatus(TUTORIAL_TEXT);
		
//        if(getResponseStream().isLocal()) {
//        	
//        }else {
//        	getResponseStream().endStatus(true, "");
//        }
	}
	
	public String getLineNumber(int zLine) {
		if(zLine<10) {
			return "00"+zLine;
		}else if(zLine<100) {
			return "0"+zLine;
		}
		
		return ""+zLine;
	}
	
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new tutorial();
	}
}

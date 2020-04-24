package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;

public class tutorial extends CommandFunction{

	public static String TUTORIAL_TEXT = "\n" + 
			"Minima transactions are a series of inputs, a series of outputs and a variable list known as the state, which you can access from the script with STATE. The state can be accessed by all the input scripts, and is stored in the MMR database, so can be accessed by PREVSTATE in the next transaction the outputs are added to, as inputs. \n" + 
			"\n" + 
			"The sum of the outputs must be less than or equal to the sum of the inputs, for every tokenid used. The difference in raw minima is the Burn. \n" + 
			"\n" + 
			"A Minima input script returns TRUE or FALSE. The default is return FALSE, so all inputs must explicitly RETURN TRUE for the transaction to be valid.\n" + 
			"\n" + 
			"A transaction can be signed, in full, by one or more public keys.\n" + 
			"\n" + 
			"Minima allows input scripts to have perfect knowledge of the entire transaction. How many, token type, amount and the address of all inputs and outputs are available. An input knows it's own script ( @SCRIPT ) and can ensure an output of a similar address exists in the outputs. \n" + 
			"\n" + 
			"A script can run for 512 instructions. An instruction is 1 operation or function.\n" + 
			"\n" + 
			"The addition of the state variables in the MMR Proof DB, allow for complex scripts with knowledge of their past to be created. A simple state mechanic for transactional history rather than a global state for ALL transactions.\n" + 
			"\n" + 
			"Minima tracks all coins that are to an address you possess and all coins that have a public key or address you possess in the STATE or PREVSTATE.\n" + 
			"\n" + 
			"Minima transactions are scriptable Logic Gates, with analogue inputs and outputs, a simple yet powerful control language, and a previous history state mechanic.\n" + 
			"\n" + 
			"I think of them as Script Gates.\n" + 
			"\n" + 
			"Grammar\n" + 
			"-------\n" + 
			"\n" + 
			"ADDRESS     ::= SHA3 ( BLOCK )\n" + 
			"BLOCK       ::= STATEMENT_1 STATEMENT_2 ... STATEMENT_n\n" + 
			"STATEMENT   ::= LET VARIABLE = EXPRESSION |\n" + 
			"                LET ( EXPRESSION_1 EXPRESSION_2 ... EXPRESSION_n ) = EXPRESSION |\n" + 
			"                IF EXPRESSION THEN BLOCK [ELSEIF EXPRESSION THEN BLOCK]* [ELSE BLOCK] ENDIF | \n" + 
			"                WHILE EXPRESSION DO BLOCK ENDWHILE |\n" + 
			"                EXEC EXPRESSION |\n" + 
			"                MAST EXPRESSION |\n" + 
			"                ASSERT EXPRESSION |\n" + 
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
			"BASEUNIT    ::= VARIABLE | VALUE | GLOBAL | FUNCTION | ( EXPRESSION )\n" + 
			"VARIABLE    ::= ^[a-z]{1,16}$\n" + 
			"VALUE       ::= NUMBER | BYTE | HEX | SCRIPT | BINARY\n" + 
			"NUMBER      ::= ^-?\\\\d*(\\\\.\\\\d+)?$\n" + 
			"BYTE        ::= [0-255]\n" + 
			"HEX         ::= 0x[0-9A-F]{2}*\n" + 
			"SCRIPT      ::= [ ASCII ]\n" + 
			"BINARY      ::= TRUE | FALSE\n" + 
			"FALSE       ::= 0\n" + 
			"TRUE        ::= NOT FALSE\n" + 
			"GLOBAL      ::= @BLKNUM | @INPUT | @INBLKNUM | @BLKDIFF\n" + 
			"      	        @AMOUNT | @ADDRESS | @TOKENID | @COINID |\n" + 
			"                @SCRIPT | @TOTIN | @TOTOUT\n" + 
			"FUNCTION    ::= FUNC ( EXPRESSION_1 EXPRESSION_2 .. EXPRESSION_n ) \n" + 
			"FUNC        ::= HEXCAT | STRCAT | LEN | REV | SUBSET | RPLVAR | GET |\n" + 
			"                ASCII | BOOL | HEX | NUMBER | SCRIPT |\n" + 
			"                ABS | CEIL | FLOOR | MIN | MAX | INC | DEC | SIGDIG | POW |\n" + 
			"                BITSET | BITGET | PROOF | SHA3 | SHA2 |\n" + 
			"                SIGNEDBY | MULTISIG | CHECKSIG |\n" + 
			"                GETOUTADDR | GETOUTAMT | GETOUTTOK | VERIFYOUT |\n" + 
			"                GETINADDR | GETINAMT | GETINTOK | GETINID | VERIFYIN |\n" + 
			"                STATE | PREVSTATE | SAMESTATE | DYNSTATE\n" + 
			"\n" + 
			"Globals\n" + 
			"-------\n" + 
			"\n" + 
			"@BLKNUM      : Block number this transaction is in\n" + 
			"@INBLKNUM    : Block number when this output was created\n" + 
			"@BLKDIFF     : Difference between BLKNUM and INBLKNUM\n" + 
			"@INPUT       : Input number in the transaction\n" + 
			"@AMOUNT      : Amount of this input\n" + 
			"@ADDRESS     : Address of this input\n" + 
			"@TOKENID     : TokenID of this input\n" + 
			"@COINID      : CoinID of this input\n" + 
			"@SCRIPT      : Script for this input\n" + 
			"@TOKENSCRIPT : Script for this input\n" + 
			"@TOTIN       : Total number of inputs for this transaction\n" + 
			"@TOTOUT      : Total number of outputs for this transaction\n" + 
			"\n" + 
			"Functions\n" + 
			"---------\n" + 
			"\n" + 
			"HEXCAT ( HEX_1 HEX_2 ... HEX_n )\n" + 
			"Concatenate the values. All values are treated as HEX values \n" + 
			"\n" + 
			"STRCAT ( SCRIPT_1 SCRIPT_2 ... SCRIPT_n )\n" + 
			"Concatenate the values. All values are treated as SCRIPT values. \n" + 
			"A space is added between each and then the script is CLEANED. \n" + 
			"\n" + 
			"LEN ( HEX )\n" + 
			"Length of the data\n" + 
			"\n" + 
			"REV ( HEX )\n" + 
			"Reverse the data\n" + 
			"\n" + 
			"SUBSET ( HEX NUMBER NUMBER )\n" + 
			"Return the HEX subset of the data - start - length\n" + 
			"\n" + 
			"RPLVAR ( SCRIPT SCRIPT SCRIPT ) \n" + 
			"In a script, replace a variable definition with the following Expression. Can be used with @SCRIPT for recursive covenants.\n" + 
			"\n" + 
			"GET ( NUMBER NUMBER .. NUMBER )\n" + 
			"Return the array value set with LET ( EXPRESSION EXPRESSION .. EXPRESSION )  \n" + 
			"\n" + 
			"ASCII ( HEX )\n" + 
			"Convert the HEX value of a script value to a script\n" + 
			"\n" + 
			"BOOL ( VALUE )\n" + 
			"Convert to TRUE or FALSE value\n" + 
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
			"POW ( NUMBER NUMBER )\n" + 
			"Returns the power of N of a number. N must be a whole number.\n" + 
			"\n" + 
			"SIGDIG ( NUMBER NUMBER )\n" + 
			"Set the significant digits of the number\n" + 
			"\n" + 
			"BITSET ( HEX NUMBER BINARY )\n" + 
			"Set the value of the BIT at that Position to 0 or 1\n" + 
			"\n" + 
			"BITGET ( HEX NUMBER ) \n" + 
			"Get the BINARY value of the bit at the position.\n" + 
			"\n" + 
			"CHAINSHA ( HEX HEX ) \n" + 
			"Recursively SHA3 hash the first HEX value with the merkle proof provided in the second. Returns the final result that can be checked in script. Use the 'chainsha' function in Minima to construct Hash Trees proofs for MAST and Signature Public Keys.   \n" + 
			"\n" + 
			"SHA3 ( NUMBER HEX ) \n" + 
			"Returns the SHA3 value of bitlength NUMBER of the HEX value. The SHA3 bitlength can be 160, 256 or 512.\n" + 
			"\n" + 
			"SHA2 ( HEX ) \n" + 
			"Returns the SHA2 value of the HEX value. 256 bits.\n" + 
			"\n" + 
			"SIGNEDBY ( HEX )\n" + 
			"Returns true if the transaction is signed by this public key\n" + 
			"\n" + 
			"MULTISIG ( BYTE HEX1 HEX2 .. HEXn )\n" + 
			"Returns true if the transaction is signed by N of the public keys\n" + 
			"\n" + 
			"CHECKSIG ( HEX HEX HEX)\n" + 
			"Check public key, data and signature \n" + 
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
			"VERIFYOUT ( BYTE HEX NUMBER HEX [NUMBER])\n" + 
			"Verify the specified output has the specified address, amount and tokenid. Optional 4th parameter relates to AMOUNT. -1 LTE, 0 EQ, 1 GTE. Default EQ.\n" + 
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
			"VERIFYIN ( BYTE HEX NUMBER HEX [NUMBER])\n" + 
			"Verify the specified input has the specified address, amount and tokenid. Optional 4th parameter relates to AMOUNT. -1 LTE, 0 EQ, 1 GTE. Default EQ.\n" + 
			"\n" + 
			"STATE ( BYTE )\n" + 
			"Return the state value for the given number\n" + 
			"\n" + 
			"PREVSTATE ( BYTE )\n" + 
			"Return the state value stored in the MMR data in the initial transaction this input was created. Allows for a state to be maintained from 1 spend to the next\n" + 
			"\n" + 
			"SAMESTATE ( BYTE [BYTE] )\n" + 
			"Return TRUE if the previous state and current state are the same. If 2 parameters are set then checks all the values inbetween the 2 values inclusively\n" + 
			"\n" + 
			"DYNSTATE ( BYTE EXPRESSION )\n" + 
			"Dynamically set the state value. You can only use this ONCE per state per transaction. MUST be used before STATE or SAMESTATE.\n" + 
			"\n" + 
			"Examples\n" + 
			"--------\n" + 
			"\n" + 
			"LET thing = 23\n" + 
			"LET ( 12 2 ) = 45.345\n" + 
			"LET ( 0 0 1 ) = 0xFF\n" + 
			"LET ( 0xFF ( thing + 1 ) ) = [ RETURN TRUE ]\n" + 
			"\n" + 
			"--\n" + 
			"\n" + 
			"RETURN SIGNEDBY ( 0x12345.. )\n" + 
			"\n" + 
			"--\n" + 
			"\n" + 
			"IF SIGNEDBY ( 0x123456.. ) AND SIGNEDBY ( 0x987654.. ) THEN\n" + 
			"   RETURN TRUE\n" + 
			"ELSE IF @BLKNUM GT 198765 AND SIGNEDBY ( 0x12345.. ) THEN\n" + 
			"   RETURN TRUE\n" + 
			"ENDIF\n" + 
			"\n" + 
			"--\n" + 
			"\n" + 
			"LET x = STATE ( 23 )\n" + 
			"LET shax = SHA3 ( x )\n" + 
			"IF shax EQ 0x6785456.. AND SIGNEDBY ( 0x12345.. ) THEN \n" + 
			"  RETURN TRUE \n" + 
			"ENDIF\n" + 
			"\n" + 
			"--\n" + 
			"\n" + 
			"EXEC [ RETURN TRUE ]\n" + 
			"\n" + 
			"--\n" + 
			"\n" + 
			"MAST 0xA6657D2133E29B0A343871CAE44224BBA6BB87A972A5247A38A45D3D2065F7E4\n" + 
			"\n" + 
			"--\n" + 
			"\n" + 
			"LET old = [ LET add = 0xFFEEDDFFEEDD ]\n" + 
			"LET new = RPLVAR ( old [ add ] [ 0xEE ]] )\n" + 
			"\n" + 
			"--\n" + 
			"\n" + 
			"ASSERT STATE ( 0 ) EQ INC ( PREVSTATE ( 0 ) )\n" + 
			"\n" + 
			"--\n" + 
			"";
	
	public tutorial() {
		super("tutorial");
		setHelp("", "Explain Minima and go through Scripting", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Just print out the Tutorial Text
		getResponseStream().getDataJSON().put("Tutorial", TUTORIAL_TEXT);
		getResponseStream().endStatus(true, "");

//		getResponseStream().hardEndStatus(TUTORIAL_TEXT);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new tutorial();
	}
}

# Minima

This is the main repository for the Minima application.

Minima is a new cryptocurrency with an emphasis on every user being able to run a full and complete node. The system uses a UTXO model like Bitcoin, but data is stored in an MMR Proof DB, where every user only keeps track of their own coins, rather than all users keeping track of all coins. The scripting is simple yet advanced, taking all the wonderful ideas that people would like to see on Bitcoin, like covenants, a simple state machine, MAST (merkelized abstract syntax trees), quantum secure signatures, and merkle proof checks.. use `tutorial` in the app to see the scripting.   

The White Paper is included in this repo. Nothing makes me happier than people who can be bothered to read it. 

The project is setup as an Eclipse Java project. You should be able to clone the repo, add to eclipse, compile and run.

If you run it with no parameters it connects to the main Minima testnet.

cd into the JAR folder and run as follows :

```
cd ./jar

java -jar minima.jar
```

For a full list of parameters run :

```
java -jar minima.jar -help
```

To start a private test-net

```
java -jar minima.jar -private -clean
```

And to keep your data just start without the -clean

```
java -jar minima.jar -private
```

You can then connect to it from another instance of the app by running :

```
java -jar minima.jar -connect 127.0.0.1 9001 -port 9010 -rpcport 9011 -clean
```
Note that this will set the port of the 2nd instance to 9010 and the rpc port to 9011.. otherwise the app will not allow you to start, as the ports will already be in use.

Using the `-clean` parameter deletes any previous data and ensures you can resysnc to the current chain.

If you compile from scratch - you can use the bin folder.. you need to link the H2 sql db.

```
cd ./bin

java -cp ../lib/*:. org.minima.Start
```

You can add -private and all the other parameters to that.


### Demo Session

Once you are in..

Start by seeing all the functions at your disposal

```
help
```

You can give yourself 50 testnet Mini with

```
gimme50
```
 
..and away you go.

Get an address

```
newaddress
```

Send to an imaginary address

```
send 1.234 0xFF
```

Check the system

```
status
```

Get your balance

```
balance
```

Create a token

```
createtoken mycoin 1000
```

The BEST way to play with Minima is via the MiniDAPP system that runs on port 21000 

Full details  @ [http://mifi.minima.global](http://mifi.minima.global)

Use the Terminal, Wallet, Script IDE etc..

For a complete explanation of the Minima Scripting language use

```
tutorial
```

To show how simple the language actually is.. here it is :

```

ADDRESS     ::= SHA3 ( BLOCK )
BLOCK       ::= STATEMENT_1 STATEMENT_2 ... STATEMENT_n
STATEMENT   ::= LET VARIABLE = EXPRESSION | 
                IF EXPRESSION THEN BLOCK 
                [ELSEIF EXPRESSION THEN BLOCK]* 
                [ELSE BLOCK] ENDIF | 
                MAST BLOCK [ORMAST BLOCK]* ENDMAST |
                RETURN EXPRESSION
EXPRESSION  ::= RELATION AND RELATION  | RELATION OR RELATION  |  
                RELATION XOR RELATION  | RELATION NAND RELATION | 
                RELATION NOR RELATION  |  RELATION NXOR RELATION | RELATION
RELATION    ::= LOGIC EQ LOGIC  | LOGIC NEQ LOGIC  | 
                LOGIC GT LOGIC  | LOGIC GTE LOGIC  | 
                LOGIC LT LOGIC  | LOGIC LTE LOGIC  | LOGIC
LOGIC       ::= LOGIC & OPERATION | LOGIC | OPERATION | 
                LOGIC ^ OPERATION | OPERATION
OPERATION   ::= OPERATION + MULDIV | OPERATION - MULDIV | 
                OPERATION % MULDIV | 
                OPERATION << MULDIV | OPERATION >> MULDIV | MULDIV
MULDIV      ::= MULDIV * PRIME | MULDIV / PRIME | PRIME
PRIME       ::= NOT PRIME |  NEG PRIME | BASEUNIT
BASEUNIT    ::= VAR | VALUE | GLOBAL | FUNCTION | ( EXPRESSION )
VAR         ::= ^[a-z]{1,10}$
VALUE       ::= NUMBER | DATA | BINARY
DATA        ::= HEX | SCRIPT
BINARY      ::= TRUE | FALSE
GLOBAL      ::= @BLKNUM | @INPUTNUM |
      	        @AMOUNT | @ADDRESS | @TOKENID | @COINID |
                @SCRIPT | @TOTIN | @TOTOUT
NUMBER      ::= "^-?\\d*(\\.\\d+)?$"
BYTE        ::= [0-255]
HEX         ::= 0x[0-9A-F]{2}*
SCRIPT      ::= [ ASCII ]
FALSE       ::= 0
TRUE        ::= NOT FALSE
MASTBLK     ::= $HEX
FUNCTION    ::= FUNC ( EXPRESSION1 EXPRESSION2 .. EXPRESSIONn ) 
FUNC        ::= CONCAT | LEN | REV | SUBSET | RPLVAR |
                ASCII | BOOL | HEX | NUMBER | SCRIPT |
                ABS | CEIL | FLOOR | MIN | MAX | INC | DEC |
                BITSET | BITGET | PROOF | SHA3 | SHA2 |
                SIGNEDBY | MULTISIGNEDBY | CHECKSIG |
                GETOUTADDR | GETOUTAMT | GETOUTTOK | VERIFYOUT |
                GETINADDR | GETINAMT | GETINTOK | GETINID | VERIFYIN |
                SUMINTOK | SUMOUTTOK | STATE | PREVSTATE | *DYNSTATE

Globals
-------

@BLKNUM    : Block number this transaction is in
@INPUT     : Input number in the transaction
@AMOUNT    : Amount of this input
@ADDRESS   : Address of this input
@TOKENID   : TokenID of this input
@COINID    : CoinID of this input
@SCRIPT    : Script for this input
@TOTIN     : Total number of inputs for this transaction
@TOTOUT    : Total number of outputs for this transaction
@INBLKNUM  : Block number this output was created - useful for OP_CSV

```

A complete breakdown of the functions is also shown in the app.

#### WARNING

This is experimental code. Nothing is yet set in stone. Things will break. There are bugs. It's the wild west out there.

Now.. ENJOY!! 
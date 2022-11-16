# Minima

This is the main repository for the Minima application.

Minima is a new blockchain with an emphasis on every user being able to run a full and complete node. The system uses a UTXO model like Bitcoin, but data is stored in an MMR Proof DB, where every user only keeps track of their own coins, rather than all users keeping track of all coins. The scripting is simple yet advanced, taking all the wonderful ideas that people would like to see on Bitcoin, like covenants, a simple state machine, MAST (merkelized abstract syntax trees), quantum secure signatures, and merkle proof checks.. use `tutorial` in the app to see the scripting.

The White Paper is included in this repo. Nothing makes me happier than people who can be bothered to read it.

The project is setup as an Eclipse Java project. You should be able to clone the repo, add to eclipse, compile and run.

If you run it with no parameters it connects to the main Minima testnet.

cd into the JAR folder and run as follows (also assigns max 1GB RAM to Minima):

```
cd ./jar

java -Xmx1G -jar minima.jar
```

For a full list of parameters run :

```
java -Xmx1G -jar minima.jar -help
```

To start a private test-net

```
java -Xmx1G -jar minima.jar -private -clean
```

And to keep your data just start without the -clean

```
java -Xmx1G -jar minima.jar -private
```

You can then connect to it from another instance of the app by running :

```
java -jar minima.jar -connect 127.0.0.1 9001 -port 9010 -clean -data minimadata2
```
Note that this will set the base port of the 2nd instance to 9010.. otherwise the app will not allow you to start, as the ports will already be in use.

Using the `-clean` parameter deletes any previous data and ensures you can resysnc to the current chain.

Use `-conf` to specify an absolute path to a configuration file. Follows the same keys as the parameters i.e. `-port 9008` -> `port=9008`.
Parameters included as arguments take precedence over those included in configuration file.

And finally `-data` specifies an absolute path to a different folder to store the data files for this second running instance.

If you compile from scratch - you can use the bin folder.. you need to link the H2 sql db and the rhino javascript library both of which are in the lib folder.

```
cd ./bin

java -cp ../lib/*:. org.minima.Start
```

You can add -private and all the other parameters to that.


Apple Silicon: please use OpenJDK Java 11 (LTS) macOS ARM 64 bit (Zuul version 11.45.27 or later)
https://www.azul.com/downloads/zulu-community/?version=java-11-lts&os=macos&architecture=arm-64-bit&package=jdk

### building Minima

The most reliable way to build Minima is to use gradlew (the gradle wrapper). You do not need to install anything except a JDK, the wrapper will install gradle locally.

To build using gradlew from the command line, you only need to type:
```
./gradlew build
```

To build a fatjar (all in one jar containing everything needed to run Minima) from the command line, use the following command:
```
./gradlew shadowJar
```

To run the fatjar:
```
java -jar build/libs/minima-all.jar
```

To build from the IDE, you will first need to generate IDE configuration files. This is valid for Eclipse, VS Code, etc. It is better to stop your IDE before running this command.
```
./gradlew cleanEclipse eclipse
```

If you still have project errors, try in Visual Studio Code: View > Command Palette... > Java: Clean Java Language Server Workspace > Restart and delete.

To run non default main tasks, try:
```
./gradlew runp2p
```


### Tests

You can run the tests directly from your IDE or from command-line.

```
./gradlew test
```

Generate code coverage test report with:
```
./gradlew test jacocoTestReport
```

The reports can be found at
```
./build/reports/tests/test/index.html
./build/reports/jacoco/test/html/index.html
```

### Docker

For those using Docker, you can try the Minima docker image without compiling the source code:
```
docker pull minimaglobal/minima:tests
docker run minima:tests
```

The tests tag maps to the tests github branch.

You can also build your own Docker minima image with the following command:
```
docker build -t minima:latest .
```

Note: if running on ARM (Linux or Apple Silicon) please us the following command instead:
```
docker build -t minima:latest -f Dockerfile.arm64v8 .
```

Start the image with default settings:
```
docker run minima:latest
```

### End to end testing

Requirements:
- Docker
- jq

Build local Docker image:
```
docker build -t minima:latest .
```

Start a 3 nodes private Minima network using Docker:
```
./scripts/start_docker_network_private_net.sh
```

By default the network is isolated. You can run curl commands using dockere. See script source code for details.

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

The BEST way to play with Minima is via the MiniDAPP system that by default runs on port 9004

Full details  @ [http://mifi.minima.global](http://mifi.minima.global)

Use the Terminal, Wallet, Script IDE etc..

For a complete explanation of the Minima Scripting language use

```
tutorial
```

To show how simple the language actually is.. here it is ( in full ) :

```

ADDRESS     ::= SHA3 ( BLOCK )
BLOCK       ::= STATEMENT_1 STATEMENT_2 ... STATEMENT_n
STATEMENT   ::= LET VARIABLE = EXPRESSION |
                LET ( EXPRESSION_1 EXPRESSION_2 ... EXPRESSION_n ) = EXPRESSION |
                IF EXPRESSION THEN BLOCK [ELSEIF EXPRESSION THEN BLOCK]* [ELSE BLOCK] ENDIF |
                WHILE EXPRESSION DO BLOCK ENDWHILE |
                EXEC EXPRESSION |
                MAST EXPRESSION |
                ASSERT EXPRESSION |
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
BASEUNIT    ::= VARIABLE | VALUE | GLOBAL | FUNCTION | ( EXPRESSION )
VARIABLE    ::= ^[a-z]{1,16}$
VALUE       ::= NUMBER | BYTE | HEX | SCRIPT | BINARY
NUMBER      ::= ^-?\\d*(\\.\\d+)?$
BYTE        ::= [0-255]
HEX         ::= 0x[0-9A-F]{2}*
SCRIPT      ::= [ ASCII ]
BINARY      ::= TRUE | FALSE
FALSE       ::= 0
TRUE        ::= NOT FALSE
GLOBAL      ::= @BLKNUM | @INPUT | @INBLKNUM | @BLKDIFF
      	        @AMOUNT | @ADDRESS | @TOKENID | @COINID |
                @SCRIPT | @TOTIN | @TOTOUT
FUNCTION    ::= FUNC ( EXPRESSION_1 EXPRESSION_2 .. EXPRESSION_n )
FUNC        ::= HEXCAT | STRCAT | LEN | REV | SUBSET | RPLVAR | GET |
                ASCII | BOOL | HEX | NUMBER | SCRIPT |
                ABS | CEIL | FLOOR | MIN | MAX | INC | DEC | SIGDIG | POW |
                BITSET | BITGET | BITCOUNT | CHAINSHA | SHA3 | SHA2 |
                SIGNEDBY | MULTISIG | CHECKSIG |
                GETOUTADDR | GETOUTAMT | GETOUTTOK | VERIFYOUT |
                GETINADDR | GETINAMT | GETINTOK | GETINID | VERIFYIN |
                STATE | PREVSTATE | SAMESTATE | DYNSTATE

Globals
-------

@BLKNUM      : Block number this transaction is in
@BLKTIME     : Block time in seconds from Jan 01 1970
@PREVBLKHASH : Hash of the previous Block
@INPUT       : Input number in the transaction
@INBLKNUM    : Block number when this output was created
@BLKDIFF     : Difference between BLKNUM and INBLKNUM
@AMOUNT      : Amount of this input
@ADDRESS     : Address of this input
@TOKENID     : TokenID of this input
@COINID      : CoinID of this input
@SCRIPT      : Script for this input
@TOKENSCRIPT : Script for this input
@TOTIN       : Total number of inputs for this transaction
@TOTOUT      : Total number of outputs for this transaction
@FLOATING    : Is this a floating input
@PRNG        : Pseudo random number - Globally Unique

```

A complete breakdown of the functions is also shown in the app.

#### WARNING

This is still experimental code. Nothing is yet set in stone. Things will break. There are bugs. It's the wild west out there.

..enjoy :)

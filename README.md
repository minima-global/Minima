# Minima

This is the main repository for the Minima application.

Minima is a new blockchain with an emphasis on every user being able to run a full and complete node. The system uses a UTXO model like Bitcoin, but data is stored in an MMR Proof DB, where every user only keeps track of their own coins, rather than all users keeping track of all coins. The scripting is simple yet advanced, taking all the wonderful ideas that people would like to see on Bitcoin, like covenants, a simple state machine, MAST (merkelized abstract syntax trees), quantum secure signatures, and merkle proof checks.. use `tutorial` in the app to see the scripting.

The White Paper is included in this repo. Nothing makes me happier than people who can be bothered to read it.

The project is setup as an Eclipse Java project. You should be able to clone the repo, add as an Eclipse project, compile and run.

If you run it with no parameters it connects to the main Minima network. This should work out of the box.

A jar is provided in the jar folder which you can run with : 

```
java -jar minima.jar
```

The default main port for Minima is 9001. If you are running this on a server please open this port externally.

For a full list of parameters run :

```
java -jar minima.jar -help
```

The main interface into Minima is known as the MDS.

This is the MiniDAPP System. Essentially these are Web apps that access Minima via your node.

You can start the MDS system up by running..

```
java -jar minima.jar -mdsenable -mdspassword 123
```

The MDS runs over SSL, with a self signed certificate, on the port 2 up from the main port.. so by default it is on

https://127.0.0.1:9003/

If you do not specify an MDS password a good one is generated for you - which you can see by running mds on the cli

```
mds
```

To start a private test-net

```
java -jar minima.jar -nop2p -test -genesis
```

And to keep your data on the next run just start without the -genesis

```
java -jar minima.jar -nop2p -test
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

If you compile from scratch - you can use the bin folder.. or just run from Eclipse as normal

```
cd ./bin

java -cp ../lib/*:. org.minima.Start
```

### building Minima

You can compile Minima as normal in Eclipse - but you can also use gradle.

To build using gradlew from the command line, you only need to type:

```
./gradlew clean build
```

To run the fatjar - which includes all the libs:

```
java -jar ./build/libs/minima-all.jar
```

### Tests

Minima copmes with many test cases. These are run when you do a clean build. You can run the tests directly from your IDE or from command-line.

```
./gradlew test
```

### Docker

For those using Docker, you can try the Minima docker image without compiling the source code:

```
docker pull minimaglobal/minima:latest
docker run minima:latest
```

### Demo Session

Once you have started Minima - you can type commands into the cli..

Start by seeing all the functions at your disposal

```
help
```

Get an address

```
getaddress
```

Send to an imaginary address - if you start a private network you will have 1 billion Minima.

```
send amount:20 adress:0xFF
```

Check the system

```
status
```

Get your balance

```
balance
```

For a complete explanation of the Minima Scripting language use

```
tutorial
```

..

Good luck!

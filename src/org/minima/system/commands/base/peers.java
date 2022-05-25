package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.cascade.Cascade;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Magic;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.NetworkManager;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

import java.io.File;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class peers extends Command {

	public peers() {
		super("peers","prints the peers list this node has");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		P2PManager p2PManager = (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();
		ret.put("peers-list", InetSocketAddressIO.addressesListToJSONArray(new ArrayList<>(p2PManager.getPeers())));
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new peers();
	}

}

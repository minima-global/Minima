package org.minima.system.genesis;

import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class GenesisCoin extends Coin {

	public static final MiniData GENESIS_COINID = new MiniData("0x5350415254414355534C4F5645534D494E494D41");
	
	public GenesisCoin() {
		super(GENESIS_COINID, Address.TRUE_ADDRESS.getAddressData(), MiniNumber.BILLION, Token.TOKENID_MINIMA);
	}
	
}

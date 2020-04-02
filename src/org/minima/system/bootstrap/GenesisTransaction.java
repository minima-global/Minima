package org.minima.system.bootstrap;

import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class GenesisTransaction extends Transaction {

	public GenesisTransaction() {
		super();
		
//		//Create the correct inputs..
//		Coin in = new Coin();
//		in.setAddress(new Address(new RamData32("FFFFFFFFFFFFFFFF") ));
//		
//		RamNumber val = new RamNumber("1000000");
//		
//		in.setValue(val);
//		
//		Coin out = new Coin();
//		out.setAddress(new Address(new RamData32("11223344") ));
//		out.setValue(new RamNumber(500000));
//		
//		Coin out2 = new Coin();
//		out2.setAddress(new Address(new RamData32("55667788") ));
//		out2.setValue(new RamNumber(500000));
//		
//		//Add them to this Transaction
//		addInput(in);
//		addOutput(out);
//		addOutput(out2);
	}
	
}

package org.minima.utils.tests.numbers;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;

public class BigD {
	
		public static void main(String[] zArgs) {
	
//			MathContext ctext = MathContext.DECIMAL32;
//			BigDecimal x = new BigDecimal("0000001.01",ctext).setScale(10);
//			BigDecimal y = new BigDecimal("3");
//			BigDecimal z = x.divide(y,MathContext.DECIMAL128);
//			SimpleLogger.log(x+" : "+y+" : "+z);
			
			try {
				MiniNumber num = new MiniNumber("1");
				
				//Get the Data..
				ByteArrayOutputStream baos 	= new ByteArrayOutputStream();
				DataOutputStream dos = new DataOutputStream(baos);
				
				//Write the Object to the Stream
				num.writeDataStream(dos);
				
				//flush it..
				dos.flush();
				
				//Get the Data..
				byte[] objdata = baos.toByteArray();
				
				MiniData data = new MiniData(objdata);
				
				//For now.. 
				MinimaLogger.log("HASH Object "+data);
				
				
			}catch (Exception e) {
				//Error Hashing!?
				System.err.print(e);
			}
			
			
//			for(int i=0;i<10;i++)
//			{
//				TxPOW tx = new TxPOW();
//				tx.setNonce(new RamNumber(i));
//						
//				//Test some hashing..
//				RamData hash = Crypto.getInstance().hashObject(tx);
//				
//				SimpleLogger.log(tx+" .. "+hash);
//			}
			
				
		}
	
}

package org.minima.system.external;

import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class ProcessManager extends SystemHandler {

	public static final String PROCESS_TXNCALL = "PROCESS_TXNCALL";
	public static final String PROCESS_RELCOIN = "PROCESS_RELCOIN";
	
	public String mTxnCall = ""; 
	public String mRelCoin = ""; 
	
	public ProcessManager(Main zMain) {
		super(zMain,"PROCESS");
	}
	
	public void setTXNCallFunction(String zFunction) {
		mTxnCall = zFunction.trim();
	}
	
	public void setRelCoin(String zPostURL) {
		mRelCoin = zPostURL.trim();
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(PROCESS_TXNCALL)) {
//			if(false && !mTxnCall.equals("")) {
//				//Construct the message..
//				Transaction trans = (Transaction) zMessage.getObject("transaction");
//				MiniNumber total  = (MiniNumber) zMessage.getObject("total"); 
//				
//				//Make  JSON Object..
//				JSONObject data = trans.toJSON();
//				data.put("total", total.toString());
//				
//				//Now convert to String..
//				String jj = data.toJSONString();
//				
//				//Now convert that to RAW HEX data
//				MiniData hex = new MiniData(jj.getBytes());
//				MiniData hex = new MiniData(jj.getBytes());
//				MiniData hex = new MiniData(jj.getBytes());
//				
//				//Now create the function..
////				String func = mTxnCall+" "+hex.toString();
////				String func = mTxnCall+" "+hex.toString();
////				String func = mTxnCall+" "+hex.toString();
////				String func = mTxnCall+" "+hex.toString();
//				String func = mTxnCall+" '"+jj+"'";
//				
//				PostMessage(new Message(PROCESS_COMMAND).addString("command", func));
//			}
		
		}else if(zMessage.getMessageType().equals(PROCESS_RELCOIN)) {
			if(!mRelCoin.equals("")) {
				//Get the Coin..
				Coin cc            = (Coin)zMessage.getObject("coin");
				MiniData transid = (MiniData)zMessage.getObject("transid");
				MiniData txpowid = (MiniData)zMessage.getObject("txpowid");
				boolean spent      = zMessage.getBoolean("spent");
				
				//Make  JSON Object..
				JSONObject data = new JSONObject();
				data.put("coin", cc.toJSON());
				data.put("transid", transid.toString());
				data.put("txpowid", txpowid.toString());
				data.put("spent", spent);
				
				//Now call the WebPage..!
				//HACK _ NEED A POST REQUEST!
//				PostReqEx.post(mRelCoin, data.toJSONString());
				
//				//Now convert that to RAW HEX data
//				MiniData hex = new MiniData(jj.getBytes());
//				String func = mTxnCall+" "+hex.toString();
				
//				//Now create the function..
//				String func = mRelCoin+" '"+jj+"'";
//				
//				PostMessage(new Message(PROCESS_COMMAND).addString("command", func));
			}
		}
	}
	

}

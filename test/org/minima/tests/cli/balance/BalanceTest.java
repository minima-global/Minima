package org.minima.tests.cli.balance;

import java.util.ArrayList;

import org.minima.Minima;
import org.minima.system.Main;
import org.minima.system.network.webhooks.NotifyManager;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;

public class BalanceTest {

    public String command = "";
    public Minima minima;

    public BalanceTest() {
        
        //New Instance of Minima
        minima = new Minima();
        
        //Add a listener
        Main.setMinimaListener(new MessageListener() {
            
            @Override
            public void processMessage(Message zMessage) {
                if(zMessage.getMessageType().equals(MinimaLogger.MINIMA_LOG)){

                }else if(zMessage.getMessageType().equals(NotifyManager.NOTIFY_POST)){
                    //Get the JSON..
                    JSONObject notify = (JSONObject) zMessage.getObject("notify");

                    //What is the Event..
                    String event    = (String) notify.get("event");
                    JSONObject data = (JSONObject) notify.get("data");

                    if(event.equals("NEWBLOCK")) {
                        //Get the TxPoW
                        JSONObject mTxPowJSON = (JSONObject) data.get("txpow");
                        
                    }else if(event.equals("NEWBALANCE")) {
                        //Balance change..
                    }
                }
            }
        });
        
        
        //Now start her up..
        ArrayList<String> vars = new ArrayList<>();

        vars.add("-daemon");
        vars.add("-nop2p");
        vars.add("-test");
        vars.add("-genesis");
        
        //Set a data dir..
        //vars.add("-data");
        //vars.add("");

        //No shut down hook for CTRL C
        //vars.add("-noshutdownhook");

        //Auto connect
        //vars.add("-connect");
        //vars.add("127.0.0.1:9001");
        
        minima.mainStarter(vars.toArray(new String[0]));

        //Catch error..
        try {
            
            //Now wait for valid..
            Thread.sleep(1000);
            
            //Run status
            String status = minima.runMinimaCMD("status",false);

            //Make a JSON
            JSONObject json = (JSONObject) new JSONParser().parse(status);

            //Get the status..
            while(!(boolean)json.get("status")){
                Thread.sleep(2000);

                //Run Status..
                status = minima.runMinimaCMD("status");

                //Make a JSON
                json = (JSONObject) new JSONParser().parse(status);

                MinimaLogger.log("Waiting for Status .. "+json.toString());
            }

           
            
        }catch(Exception exc) {
            exc.printStackTrace();
        }
        
        //Shut down..
       
    }
    public void setCommand(String _command){
        command = _command;
    }

    public String runCommand(){
           
            String balance = minima.runMinimaCMD("balance",false);
            
            //Make a JSON
            //JSONObject jsonbalance = (JSONObject) new JSONParser().parse(balance);
            
            //CHECK STUFF
            
            //Re convert back to string..
            //String prettyjson = MiniFormat.JSONPretty(jsonbalance);

            return balance.toString();
    }

    public String getCommandOutput(){
        return "";
    }
    
}
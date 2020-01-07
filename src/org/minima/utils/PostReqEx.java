package org.minima.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.minima.objects.Coin;
import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public class PostReqEx {

//	public static String post(String zURL, String zData) throws ClientProtocolException, IOException {
//		System.out.print("Remote URL :"+zURL+" "+zData);
//		
//		CloseableHttpClient client = HttpClients.createDefault();
//	    HttpPost httpPost = new HttpPost(zURL);
//	 
//	    List<NameValuePair> params = new ArrayList<NameValuePair>();
//	    params.add(new BasicNameValuePair("data", zData));
//	    
//	    httpPost.setEntity(new UrlEncodedFormEntity(params));
//	 
//	    CloseableHttpResponse response = client.execute(httpPost);
//
//	    HttpEntity entity = response.getEntity();
//	    
//	    InputStream is = entity.getContent();
//	    BufferedReader br = new BufferedReader(new InputStreamReader(is));
//	    
//	    String line = null;
//	    StringBuilder sb = new StringBuilder();
//	    
//	    while ((line = br.readLine()) != null) {
//            sb.append(line + "\n");
//        }
//	    
//	    client.close();
//	    
//	    return sb.toString().trim();
//	}
	
	public static void main(String[] zArgs) {
		
//		try {
//			Coin cc = new Coin( new MiniData32("0xFFEEDDFEE56785765"), 
//								new MiniData32("0xEEEEEEEEEEEEEEE"), 
//								new MiniNumber("10"), 
//								new MiniData32("0x00"));
//			
//			JSONObject jj = new JSONObject();
//			jj.put("coin", cc.toJSON());
//			jj.put("txpowid", new MiniData32("0x4897593845693485").toString());
//			jj.put("transid", new MiniData32("0xABCCFEDBCBCBC66565").toString());
//			jj.put("spent", true);
//			
//			System.out.println("Sending : "+jj.toJSONString());
//			
//			String ret = post("http://127.0.0.1/callback/newrelcoin.php",jj.toJSONString());
//		
//			System.out.println("Received : "+ret);
//		
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		
//		System.out.println("Finished..");
		
	}
	
	

}

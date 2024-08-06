package org.minima.utils;

import java.math.BigInteger;
import java.util.Iterator;
import java.util.Random;
import java.util.concurrent.TimeUnit;

import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public class MiniFormat {

	public static JSONArray convertToJSON(String zJsonString) throws ParseException {
		
		//Convert response..
		return (JSONArray) new JSONParser().parse(zJsonString);
	}
	
	public static String JSONPretty(JSONArray zJSONArray) {
		return JSONPretty(zJSONArray, false);
	}
	
	public static String JSONPretty(JSONArray zJSONArray, boolean zDebug) {
		Iterator<JSONObject> it = zJSONArray.iterator();
		
		boolean arr = false;
		if(zJSONArray.size()>1) {
			arr = true;
		}
		
		String result = "";
		if(arr) {
			result = "[";
		}
		
		boolean first =true;
		while(it.hasNext()) {
			
			if(!first) {
				result += ",";
			}
			first = false;
			
			JSONObject json = it.next();
			String res = JSONPretty(json,zDebug);
			
			result += res;
		}
		
		if(arr) {
			result += "]";
		}
		
		return result;
	}
	
	public static String JSONPretty(JSONObject zJSONObj) {
		return JSONPretty(zJSONObj, false);
	}
	
	public static String JSONPretty(JSONObject zJSONObj, boolean zDebug) {
		//The Work String
		String work = zJSONObj.toString().trim();
		
		//Too long clogs it up.. 
		int len = work.length();
		
		if(zDebug) {
			MinimaLogger.log("JSONPRETTY len:"+len+" obj:"+zJSONObj.toString());
		}
		
		//Check length ?
//		if(len>100000) {
//			//TOO LONG...
//			return work;	
//		}
		
		//The Copy
		String ret       = "";
		
		try {
			
			int tabs         = 0;
			String tabstring = maketabstring(tabs);
			int oldpos       = 0;
			int currentpos   = 0;
			
			while(true) {
				oldpos = currentpos;
				
				int indquotes = work.indexOf("\"",currentpos);
				int indopen   = work.indexOf("{",currentpos);
				int indclose  = work.indexOf("}",currentpos);
				int indcomma  = work.indexOf(",",currentpos);
				
				if(indquotes == -1 && indopen == -1 && indclose == -1 && indcomma == -1) {
					//Add the rest..
					ret += work.substring(currentpos, len);
					
					//Finished..
					break;
				}
				
				if(indquotes == -1) {indquotes = Integer.MAX_VALUE;}
				if(indopen == -1)   {indopen   = Integer.MAX_VALUE;}
				if(indclose == -1)  {indclose  = Integer.MAX_VALUE;}
				if(indcomma == -1)  {indcomma  = Integer.MAX_VALUE;}
				
				if(indopen < indclose && indopen < indcomma && indopen < indquotes) {
					//OPEN BRACKET
					tabs++;
					tabstring = maketabstring(tabs);
					
					String substr = work.substring(oldpos,indopen);
					currentpos = indopen+1;
					ret += substr;
					
					ret += "{\n";
					ret += tabstring;
				}else  if(indclose < indopen && indclose < indcomma && indclose < indquotes) {
					//CLOSE BRACKET
					tabs--;
					tabstring = maketabstring(tabs);
					
					String substr = work.substring(oldpos,indclose);
					currentpos = indclose+1;
					ret += substr;
					
					ret += "\n";
					ret += tabstring;
					ret += "}";
				
				}else  if(indquotes < indopen && indquotes < indcomma && indquotes < indclose) {
					//Get the inbetween bit..
					String prequote = work.substring(oldpos,indquotes);
					ret += prequote;
					
					//Quoted string..
					int quoteend = work.indexOf("\"",indquotes+1);
					
					//Get the text
					String quote = work.substring(indquotes,quoteend+1);
					
					currentpos = quoteend+1;
					ret += quote;
					
				}else{
					//COMMA
					String substr = work.substring(oldpos,indcomma);
					currentpos = indcomma+1;
					ret += substr;
					
					ret+= ",\n";
					ret += tabstring;
				}
			}
			
			//Clean up the response
			ret = ret.replaceAll("\\\\/", "/");
			ret = ret.replaceAll("\\\\n", "\n");
			
		}catch(Exception exc) {
			if(zDebug) {
				MinimaLogger.log("[!] Error JSONPretty.. return unmodified : "+work,false);
			}
			return work;
		}
		
		return ret;
	}
	
	private static String maketabstring(int zNum) {
		String ret = "";
		for(int i=0;i<zNum;i++) {
			ret+="  ";
		}
		return ret;
	}
	
	public static String formatSize(long v) {
	    if (v < 1024) return v + " bytes";
	    int z = (63 - Long.numberOfLeadingZeros(v)) / 10;
	    return String.format("%.1f %sB", (double)v / (1L << (z*10)), " KMGTPE".charAt(z));
	}
	
	public static String zeroPad(int zTotLength, MiniNumber zNumber) {
		//Get the number..
		String num       = zNumber.floor().toString();
		int len          = num.length();
		int add 		 = zTotLength-len;
		for(int i=0;i<add;i++) {
			num = "0"+num;
		}
		
		return num;
	}
	
	public static String ConvertMilliToTime(long zMilli) {
		
		long milliseconds = zMilli;
		
		long dy = TimeUnit.MILLISECONDS.toDays(milliseconds);
		
		long yr = dy / 365;
		dy %= 365;
		
		long mn = dy / 30;
		dy %= 30;
		
		long wk = dy / 7;
		dy %= 7;
		
		long hr = TimeUnit.MILLISECONDS.toHours(milliseconds)
				- TimeUnit.DAYS.toHours(TimeUnit.MILLISECONDS.toDays(milliseconds));
		
		long min = TimeUnit.MILLISECONDS.toMinutes(milliseconds)
				- TimeUnit.HOURS.toMinutes(TimeUnit.MILLISECONDS.toHours(milliseconds));
		
		long sec = TimeUnit.MILLISECONDS.toSeconds(milliseconds)
				- TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(milliseconds));
		
		long ms = TimeUnit.MILLISECONDS.toMillis(milliseconds)
				- TimeUnit.SECONDS.toMillis(TimeUnit.MILLISECONDS.toSeconds(milliseconds));
		
		return String.format("%d Years %d Months %d Weeks %d Days %d Hours %d Minutes %d Seconds", 
						yr,mn, wk, dy, hr, min, sec);
	}

	public static String createRandomString(int len) {
		Random rand = new Random();
		byte[] data = new byte[len];
		rand.nextBytes(data);
		BigInteger bignum = new BigInteger(1,data);
		return bignum.toString(32).toUpperCase();
	}
	
	public static void main(String[] zArgs) throws ParseException {
		
		String tester = "[{\"command\":\"balance\",\"status\":true,\"pending\":false,\"response\":[{\"token\":\"Minima\",\"tokenid\":\"0x00\",\"confirmed\":\"702.98859999729999999999999999999999608499999877\",\"unconfirmed\":\"0\",\"sendable\":\"498.98989999989999999999999999999999608499999877\",\"coins\":\"81\",\"total\":\"1000000000\"},{\"token\":{\"name\":\"neverhidden\"},\"tokenid\":\"0xF5F858AAD86199FDB6EC02F5F3EBF3E3693E26FEF075A3352F22DD8033B648A8\",\"confirmed\":\"99\",\"unconfirmed\":\"0\",\"sendable\":\"99\",\"coins\":\"1\",\"total\":\"99\"},{\"token\":{\"name\":\"herwfewr\",\"url\":\"https%3A%2F%2Fmedia.giphy.com%2Fmedia%2F18YeX8h3cTw1eKZCNZ%2Fgiphy.gif\",\"description\":\"\",\"owner\":\"\",\"webvalidate\":\"\"},\"tokenid\":\"0x03E879AC5F6063168BD8151D4502C61145ABA8CAED91F0FB9EFE943B0D5D5882\",\"confirmed\":\"123\",\"unconfirmed\":\"0\",\"sendable\":\"123\",\"coins\":\"1\",\"total\":\"123\"},{\"token\":{\"name\":\"elias\"},\"tokenid\":\"0xF5B12CBD41761C20672BCFA483F2ACC0C4A1AF4B22B1BA7A11C11D55731D1E29\",\"confirmed\":\"123\",\"unconfirmed\":\"0\",\"sendable\":\"123\",\"coins\":\"10\",\"total\":\"123\"},{\"token\":{\"name\":\"EliasToken\",\"url\":\"data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAACUAAAAyCAYAAADbTRIgAAAAAXNSR0IArs4c6QAAEjFJREFUWEdVmQmYnWV1x3%2Fvt9519iWZmYTJBiEQIBCQgEHog6VQba2aIlr1eQQUrBRbEWu1WOVBsVUqAgoCIiLVAAoVSpEdFIgECwZMDAJJJPuEyUxm7vKtb3vOd4P2Jve5c%2B%2B3nfd%2F%2Fuec%2FzmvufGG71vPL7HyuJMZGJzDrp3bufmblzP7%2B1foHRvnw5%2F8Z8YXLiRqQ2ZzyqGDvH649nbOevxGgrhJyQ%2BxSQtjcxyH4m0NruNiHIPxK1AqEWWGm80oM3PHWXHU0TiuQ63iMjnTYOPLm8i76px7zgcwX7j9XpsmCfNHF9DT1cPO7VvZvP5JSjbFuh4LV65m%2FvgiLAabg%2B%2BDYxx%2BuX4dR73yNK4ccVw8DIYM13UxBv10XIN1fYzjA5a4HTN98jvZH8VMz0zSTlKefGEDxpXj0FOv8aGz12BW3%2FJTa9Ic11o8vZmD6zj6afMcscRYdFWeQAA4noM82Xc9RcLR8z2M6%2BE4Lo7n6%2FnINZ6PLazEyPWuS25lIXIvo%2F%2BtcXCMfDW6YHPKrfdbmyTIKa6eZvUE33WxNseVG1qrhjrytxzzA12577lqUG5zPDGo8%2Fb8AOO5apQcTzF4QYAjhnkOWQbGdcixaoixcp5R463YcNIt91mbZcoHTy01uIJOYTguwg15O%2FrbQYPlAXKuICKrFncJmQQxxxHUXH27gY8r7pGHeo6iaYxDKlxwDOKMXO9r9D5q%2BFu%2B%2B1NrslwhdY3Fx8ERtAoWKVPkAl8IKy52XQJfbmzUxQq3GOOJK%2BVv4ZIYJy4sDHN8r2C%2FV3y3Rt5iTGFIroDJ04QnBrPyxnusuEJcFMqFeY4jUaTHC9cJOp483Gbq1tD3cIU3HQTFCHGdIKDGCUc6rnM8caWDVaM8jO%2BR6ZIFHgkKoYR8t2qo8MscfcNPrBgg%2FFcam5we1%2BBnCUnukss%2FsVlclWWEga9GCcH9DtyePlR%2BE44JtwSYguyOGOJ6yi%2Fk03P1XDFSjehEZhFBRYCZ4268x2ZZrqEtRjnW0nzmQXasf5xMrM%2FE4yhK5A7JVIPkQFN%2FU5dYpaZGqSJuJDKLKC3%2BNpRHB5Q7Qn7P%2BJCmtKam%2BbPrbqFv8aEaDBqNwit5r%2FjO3TZLO0TvkDpZ%2Fxjdqzz8oTrWFuzycgdvapa9P3iIbZumsbg4NiPPM3K3Ss1tUwpaHDk8wi%2B27C%2BM6rzmnHUS1aMWYDKHWlAmnI3Z%2BPDTHHn%2BJWqULEsN6wSNOer6n9g8y3DyDE84gkO8%2FjGGV1dw%2BysFfhaStmHNl2%2BjO56gMuAQllMgxdm8mB3zj2PeyF34R4R4jVUc%2BJ9HNFL1JZnd83h%2BxcncO3ICNs0IY8PLDzzDMR%2F7NP2HHqZIK0KOwfN9zJHfvssqmfNMXREYMeoJ%2Bo516Zo%2FiBG4LWRZk%2FbWKX7ztfs0VRSvnKu%2BfiVX%2FuvV7NmzW8nabSeYMcP%2FDym%2Fp8bIB89SA%2Bu1OjZJ%2Bd1%2FPcHy8z5Dnxhliiqh0SwcXHrtWltwSQ5YvNwSPfsoY6u7CYe6yXODk0tiyMkzh9bEJG889xrnv1blmcv%2BHu%2FRO3ny8ScKd%2BKSZalWAiG7rgZYeM4ZyhehiUSNiQ2%2Fe%2BRpVpz3GfqXHKpGaTb3vCJfHfWtu2ySpZokERcC7XWPMH5qF%2BFAL2QGK2EiuAhbER7Jbz42r2IzDzexZDHgBUTxDFmrRVgV0seQ5ZgsIMttEcU2J48StjzyLCvP%2F0d6Fy16k0tqmHBr%2BbV3WGvFg5Y8Swgcl%2BbTDzF6Qo3e8T5M5mLzIsFJ6pWSkuUeaewTRz4ji4%2BgZ96Ypo56OeD1537NxK7XydM2pBF%2BKdMItlZqHkgQJ1HO1kfWsfKjl9C7eAlWuCQ5S%2Bqt8OuI6%2B6wB8NeKn2WxjSfepCx43spD3VTrwqkAe0oojHbwlDGDUrUyn2878wPctdHXuKN%2F54gocGyjy5g4IuL2XDLT2g609i8Rasxi%2BsaKpWQLEk15aQJbH50Hcefdwnd4wtw%2FaKQ40hSdjGHX7u2MEqLrsHkGTNPPMjS04awtQqkFsexGhW5JEhbwWYBi4YOId24hBcufYHP3bQcm2d876vP0%2F2%2BeSy7dDkP3HYHJpoFEqIoIYliTbqZjQncEq89%2BUuO%2FvAn6VmwWCuDJFdjPM3%2B5tCrf6iZSAxzhTrWcuDnP%2BOwkwapjfTrDYUHQckhzSvkeUCeBfzte8%2Fn%2Fps286t%2F%2BAURe%2FjQxSey9urnaXKAC5r%2FxP3X3kzmJ5C3ybOijGVpSpLkSoc9v3qRoz94IT3jC4t6p5InKPi1%2BKrbrNQ1IbEaZ3OaTz3MEafOpTa3j6gdMdsoMn6tVqappO3ms%2B%2F7CGtvfpGnL7qXmCkOXz5Gewdsnfwtn5j9d%2B656SYgwtUIkIiUhRuiJMMkOVMv%2Foal77%2BQ8ug8zWOOlB9cLeRm0ddv09iSzFxCtE9G9PTDLDtlDpU5PVJfmJltk6Y1LC1KQYWW28WS7kNwDgzy1pmUtZdv4rXdzxGVWqy582OMnbiMO%2B64nbw1g8lTPIngPFPDRADNEJJseoml7%2F4QpbljKm80%2BHEIwrBwnyQzQUguEOk1%2B%2BQDHH7aPMpDAxrKUriSOCaJKURfrQsv6Odv3n4mD9%2B2nh2PbSZ2IpaedRTHrDmen937OFNT%2ByA%2BgM0STQuasjR6JRODu2MHI2eejT88UrhMlYbIHhez5Krb7UHZ61rRNBmtpx5i8SmjhMN9kCX4oqxMTt6yzDZzvCCkMjBIT%2B8iKn1zGRudS%2Bil7JmOmJ3Yw8uvvgqTO7BxU8uKGGMyqwla1pRnOf7evQyevgbTP4zvHxSIPo7vYhZ97fsCkipPgVhcGa17iEWrR%2Bga6i8cai2xzSglDu22S5RDORjCdPXilQZwBvrIuwO8yWkaM29gJydI5bPdkhZI0fEyS%2BA4TE%2FPEhEwZGBg1dvJu%2FrxvCIVaFoQo8a%2FeqvkLs3mWV5I4vYzD7HslLlU5vRpAGj%2FoOLHkrUFsYBy0E%2BTkKBvgLb1ifw21apHPrufbOYAzM7gxIleL2kmS3KiRkacGlIvZ2Gtm65jV9MOqgSBr0SXMuNLQT7kyu9qQZZ%2F0qtJHTxoVHm4R28qBmmFkVfswH5ppqrYwMcEZZwuVxOjKUPeaoAgFMU4aUYaiyE57YYQypAal5KfMm9olGDxCtKwqslTFavX0eijV9yk0VdU%2FsKw9lMPcuRpI5SHxKhCx%2BUS0lKKmg5uaogagbZcsahOP6Y93aBnTi9W0nUaYRJDuxERRami42EplQIas7lqv%2FljYwSLlpP6JdXwqun9Qt%2Bbsa8USAlCcZoQuC7RUw8x8%2Bt1nHLFRzC5q1alqSzUqGpMpyFol7SDmfVaxGlM4Flq5RJpmtGcbpJkDnmaF21W3qZerWNzQdwysXUnw%2F1z6H3bGeR%2BqVCc0gX5XqGnhi%2F%2FjgCgels45Yume%2FYx2hueU0hV4WoLZLWxlB9C3y9acnJESkspkAfKylxTdMZCWs91qJQCFYrtKNFWStySZZa5q%2F%2BE8qrTyL3gzbonRhlx49AXr7eBCPlOOyWNw6H7t3BiKaOrt0dbLGkWJBh27dnLYF8ftWql02IV3Yis6qBylG46yXI2b93OnP4eRod6iZKU13dPcsjcAUq%2BS7PZ5AXqbKzPI3eFT6F2OtJMSJdkBv7lW9JD4xtHe7rUZiyZ%2BB2ryjldPd0azoEv3aBh1549DIhRlbJyUBDWNl%2B6FMFDWyaI44grr%2F0%2B7zj9ZE5auZx9b0zx6LoNvOdPV1EuBZp%2Bwu5u3N5eEuNx92TALirYTo9our5wnS250mW4ZMZyVN3jrZOb2D0xRalaUwR0ViC%2BUd1TTFPEgDhO8FyjIb17Yi%2FdXSJ1ylodZmeb1GsVSqG0%2BBAnCT31qp6fZynL5w9z%2BPiwJHcenAr49kQZI9Esta%2FvsmutzAYkkhZUXK5ZXiXwHNY%2B%2FAv%2B7YYfYRsznTZKhF7hZAlJ7aG1veocVubl2kyKmrPGau4Jy2X6ursZHuilr7eb3t4eeuol3vW2Y1i94jCN7l9Ou3zzjSoHCEB41fP5q22lVOG4Ho8vH1kjdAybtu3k%2Fqd%2BxbYduzSaJGKkBqZpospTvif6e65IijTRmiYpo6NiJQCEDqLL1a0CNBCGAfOH%2B7n4nD%2Fn2GXjpGmuAXTFzoAnZ0tanM3H73zANk3A%2B4darOxWHIjznEazxWyz8Qc0BBFFyKgmuv72e3jLETkTrRGmGyVe3LSJJMtIkoxKpaSBcfThC5X0cSJOshy7ZB6LR4XsAYfMGVBjiq7acM3ukPtmQoykhE89uN6OpdO8Z7DFUD38oyZXIqoYfr0ZYQdbb9GTCdg4VUUZRzH7Ww2mvFnaSZOZbJaF6RgLDh8vpIFMXXRCUww0ZEalLbzkB4EwCJjesZ87nMPIFamfPmlX2L0cZ%2FZRCqVDES9YatWi31PNLIMxGVpI%2Fy8tktzLHBx8FV2OijRRJXlWjIvCEMe4eF6gGt31ffwgxISh%2BFC5mDSa2jTIbIHZBs%2Bu36YBZy740X3284szap4hLIU0GhGNZpMRKcaaCDsTFJUXnmZ0beqytBhadLgkBFXNJCioVunME%2BS7ljCZFXQGHUKDJKExuV%2BzvqiT6uAA1%2Byo63nm4tt%2FbC9b6uJ6jo4LBQYhaJRaPnn5N7BRRJzD2NioIjjS30ur1eSV7bt0%2FFMKPAYH%2Btmzdx9ZmnPqiSt4fP3z2kvJ5KWrVmN6dlYX8%2F417%2BI%2F7r6PY5Yv5aPnvlcXJfPWidd3MnfhOFe9FpJ4Ieblp%2B%2B2%2FaHBU%2FgLov%2Fs8fVc8a1beedpJ3HPE88WUaVR4nHWqSfzwsbNXHDOX%2FGl676rI6I4ihgd7eelTb%2BnWipx9hnH8Z9PbCDPU6qVKp%2B68AI%2B%2FaWvaEHu6%2BnhhBXLufrLFxWoymRXEXZo7drNDY35mN3P3GXFbeJ3CWffcdm1b5oLP%2F91kriN44dMvDGpKeAdp53Ei5teZXB4kE2vbCOLE3bt20dfV52Pf%2BAveODnzzI5dUC19pq%2FPIubblvLsUccRq0UsGXnbk5fdaz2f0034NILzy6iRdxeqyuSja3budk%2FHLNn3Y%2BtrKherythwyBgwyvbeGnrdqJmwmGHLuDlLdu4%2FsYf6pyg1WzwztNOZmKmweJFC6h297Nzy1YGuusM9VV4ddsOgpIETFGe9s%2B0eXbjZubNGWB8ziC%2BF7Jq9fGccerKgo%2BCVLmstGlNTvO91nwx6k4rbpG3DBeCUKq6xfGNcrpcreqFm7Zs1wiWeUFQ6qJaq%2BMnOb%2Fd%2BAovvLBBkZyZ2k%2BaJ8rpdqtNmqTs2rWTyy67qBjMyugwzxno76GvrxuSFEoh7QMHNF%2BFvT3cPDGM%2BcE3PmfLlZq6z%2FMlAyd4jke1UiaKIqrlkGoppFatMrpsGWGtVIxoBPpGQ0sOMqqqlIoxrzxIBvJxXERdKcRGxd%2FSNYk0Fu2kBUpG33G7iGrRVGHAnukc84m%2FO89WApdKuaRR19tT1wLbnm1oLhEXpHGMcTy8QKIl02QatdsqyMQFMjWWEBmYO0Stu97JWTrr1eDJskxH3olAr%2BMBGZpknXRQpASJ%2FHKpRH1sIYb6kDVxpFNbUej9g4PqvgNT%2B0nSXA2MWi1NnOJGrW2Cjszeg6BASuS05zA8Po%2BBkTkq7nTlukkj7ZnsPHhKISkpSZLoLkaeZKoixNh6dzfd3T2c%2Fp53Y%2BgZs0zt1da6uLvI32JK92YC1FmrqE7dKiiGYVoixCBJjgX8%2BruOvXXfpDM7%2F0MSfXPrQ4YWMuPyXB0H2DBUfTU8OsLF3%2FimYD5uacdFJERNJbIuQ0gggwnpcA9mZx3Pya5RAFFU5JmD21ZyvZ7XMUjDXdxb7O%2BIskQaUykxckwWEYo%2BL4Ao9fdy5Mkn8tcXX4Jh6QnFoF9kSSwnC1lj3QozmQy8ilmohqLst%2BgotzP0FNep6uzsGGhX20FKZqjiQt9X0afDVtk8ytJiK0TuJdFoHMJqhbBSZuSQ%2BZx76WcwZ15zqxU5GwQeTm4p%2BT4H3tjH5o0b2fP6dtqN2WKEEwmxJckWuwVJEmvxlOZCB%2F%2FGaEDIw2UiKJtGByVymqZUymVi4aGO3Dua3nXxPZ9yuUS5XqNUqXDuRRdhPvv8q1b35pToIn3lphkNqVdZWnQr4nfZ31GEZFukcJOIN0FBrpMBRdGqSX7LdP9G0ZFND1UYbhGlco%2B8GNAJ9TxhlaoQCAOPrv%2FLi%2F8L95RBKGaZbG8AAAAASUVORK5CYII%3D\",\"description\":\"\",\"ticker\":\"ELS\",\"webvalidate\":\"\"},\"tokenid\":\"0xEF536EC66C51EFDFA207065C3A1F67AFBB7FDFEE0DEDEFB237A7FB7FD96EB69A\",\"confirmed\":\"1000\",\"unconfirmed\":\"0\",\"sendable\":\"1000\",\"coins\":\"1\",\"total\":\"1000\"},{\"token\":{\"name\":\"{\\\"name\\\":\\\"EliasToken\\\",\\\"url\\\":\\\"data:image\\/png\"},\"tokenid\":\"0xB3FEF50070166CEAF8F9CDBC45F8142E7E1D93F35967F5415E388DFE0A0BCCCA\",\"confirmed\":\"1000\",\"unconfirmed\":\"0\",\"sendable\":\"1000\",\"coins\":\"1\",\"total\":\"1000\"},{\"token\":{\"name\":\"gg\"},\"tokenid\":\"0x6C1E89B281FD5BDFC581D47E37751E3E81DD60F8E25D89670BBE4C483CDA3F02\",\"confirmed\":\"123\",\"unconfirmed\":\"0\",\"sendable\":\"123\",\"coins\":\"1\",\"total\":\"123\"},{\"token\":{\"name\":\"testing\"},\"tokenid\":\"0x17F5C34C755C8F59E31DE73957B01F588AAB18AF26826E4828844C19401BF61B\",\"confirmed\":\"123\",\"unconfirmed\":\"0\",\"sendable\":\"123\",\"coins\":\"1\",\"total\":\"123\"},{\"token\":{\"name\":\"TEST\"},\"tokenid\":\"0x4C4BFFDE7C1A46FD36767E56E9260487A27BF2717990E74DAC1BFE73C87C5FE6\",\"confirmed\":\"5\",\"unconfirmed\":\"0\",\"sendable\":\"5\",\"coins\":\"1\",\"total\":\"5\"},{\"token\":{\"name\":\"jimmy2\"},\"tokenid\":\"0x93F2AF1236F5E076B38B4716DB9B6B2E81C1D5DE41EBCEFDA28329AE2D772058\",\"confirmed\":\"323\",\"unconfirmed\":\"0\",\"sendable\":\"323\",\"coins\":\"1\",\"total\":\"323\"},{\"token\":{\"name\":\"jimmy\"},\"tokenid\":\"0x206AE09BE76F9E1D2B8D92AEDDC12BA85CD152F3D037349ADDF853F2C53EDF98\",\"confirmed\":\"300\",\"unconfirmed\":\"0\",\"sendable\":\"300\",\"coins\":\"1\",\"total\":\"300\"},{\"token\":{\"name\":\"jimyyboy\"},\"tokenid\":\"0xAE0D72FE6055E6A06D38252D4255D58BF82A5BCAC1E457CB0F42275BBBAF7B0D\",\"confirmed\":\"12\",\"unconfirmed\":\"0\",\"sendable\":\"12\",\"coins\":\"1\",\"total\":\"12\"},{\"token\":{\"name\":\"elias\"},\"tokenid\":\"0x849A613949DE85BBB841F312E1B19A3000D08F0388EC79FAA99CD0918F620573\",\"confirmed\":\"777\",\"unconfirmed\":\"0\",\"sendable\":\"777\",\"coins\":\"2\",\"total\":\"777\"},{\"token\":{\"name\":\"TokenB\",\"url\":\"\",\"description\":\"\",\"ticker\":\"\",\"webvalidate\":\"\"},\"tokenid\":\"0xD96D0CF90A1A828D1B1C61D3B66D12D51AC982633A2D13A7C87FA502135E61E1\",\"confirmed\":\"10\",\"unconfirmed\":\"0\",\"sendable\":\"10\",\"coins\":\"2\",\"total\":\"10\"},{\"token\":{\"name\":\"TokenA\",\"url\":\"\",\"description\":\"\",\"ticker\":\"\",\"webvalidate\":\"\"},\"tokenid\":\"0x1A3990B91A2E94C84DCCBC77808EF02CEE0B66C434B67AA765BB9C26D49D23F1\",\"confirmed\":\"10\",\"unconfirmed\":\"0\",\"sendable\":\"9\",\"coins\":\"11\",\"total\":\"10\"}]}]\n"
				+ "";
		
		JSONArray obj = convertToJSON(tester);
		
		String ver = JSONPretty(obj,true);
		
		System.out.println(ver);
	}
}

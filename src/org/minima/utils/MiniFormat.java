package org.minima.utils;

import org.minima.objects.base.MiniNumber;

public class MiniFormat {

	public static String JSONPretty(String zJSON) {
		//The Work String
		String work = zJSON.trim();
		
		//Too long clogs it up.. 
		int len = work.length();
		if(len>100000) {
			//TOO LONG...
			return work;	
		}
		
		//The Copy
		String ret       = "";
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
        
		return ret;
	}
	
	
	
//	public static String PrettyJSON(String zJSON) {
//		//The Copy
//		String ret = "";
//		
//		//How long
//		int len  = zJSON.length();
//		int tabs = 0;
//		String tabstring = maketabstring(tabs);
//		
//		boolean inquotes = false;
//		for(int i=0;i<len;i++) {
//			char cc = zJSON.charAt(i);
//	
//			//Are we in quotes..
//			if(cc == '"') {
//				inquotes = !inquotes;
//			}
//			
//			if(!inquotes) {
//				if(cc == '{') {
//					tabs++;
//					tabstring = maketabstring(tabs);
//					
//					ret+= "{\n";
//					ret += tabstring;
//					
//				}else if(cc == '}') {
//					tabs--;
//					tabstring = maketabstring(tabs);
//					
//					ret += "\n";
//					ret += tabstring;
//					ret += "}";
//				
//					
//				}else if(cc == ',') {
//					ret+= ",\n";
//					ret += tabstring;
//					
//				}else {
//					ret+= cc;
//				}
//			}else {
//				ret+= cc;
//			}
//		}
//		
//		return ret;
//	}
	
	public static String filterSafeTextEmoji(String zText) {
		String characterFilter = "[^\\x00-\\x7F\\p{L}\\p{M}\\p{N}\\p{P}\\p{Z}\\p{Cf}\\p{Cs}\\s]";
		return zText.replaceAll(characterFilter,"");
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
		String num       = zNumber.toString();
		int len          = num.length();
		int add 		 = zTotLength-len;
		for(int i=0;i<add;i++) {
			num = "0"+num;
		}
		
		return num;
	}
	
}

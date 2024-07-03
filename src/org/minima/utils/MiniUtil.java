package org.minima.utils;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Locale;

import org.minima.utils.json.JSONArray;

public class MiniUtil {

	public static final SimpleDateFormat DATEFORMAT = new SimpleDateFormat("dd_MM_yyyy_HHmmss", Locale.ENGLISH );
	
	public static JSONArray convertArrayList(ArrayList<String> zStringList) {
		JSONArray ret = new JSONArray();
		for(String str : zStringList) {
			ret.add(str);
		}
		return ret;
	}
	
	public static ArrayList<String> convertJSONArray(JSONArray zStringArray){
		ArrayList<String> ret = new ArrayList<>();
		for(Object obj : zStringArray) {
			String str = (String)obj;
			ret.add(str);
		}
		return ret;
	}
	
}

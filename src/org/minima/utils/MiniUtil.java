package org.minima.utils;

import java.util.ArrayList;

import org.minima.utils.json.JSONArray;

public class MiniUtil {

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

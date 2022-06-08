package org.minima.utils;

import java.util.ArrayList;

import org.minima.objects.base.MiniData;

public class ListCheck {

	/**
	 * Check a MiniData list for an Item
	 */
	public static boolean MiniDataListContains(ArrayList<MiniData> zList, MiniData zElement) {
		//Cycle through the list
		for(MiniData item : zList) {
			if(item.isEqual(zElement)) {
				return true;
			}
		}
		
		return false;
	}
	
}

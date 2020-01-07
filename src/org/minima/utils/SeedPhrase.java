package org.minima.utils;

import java.math.BigInteger;
import java.security.SecureRandom;

public class SeedPhrase {

	public static String[] ARTICLE  = {"THE","A"};
	
	public static String[] ADJS     = {"QUICK","SLOW","SLY","FAST","FAT","THIN","GREEDY","WARM","TIRED",
										"CUNNING","SAD","HAPPY","LAZY","HUNGRY","LOVELY","NICE","SHINY","SCALY","FURRY"};
	
	public static String[] COLOUR   = {"RED","GREEN","BLUE","YELLOW","BLACK","WHITE","PINK","ORANGE","GREY",
										"BRONZE","GOLDEN","SILVER","METAL"};
	
	public static String[] NOUNS    = {"CAT","DOG","BIRD","FISH","LION","HIPPO","WOLF","COW","MOUSE","TIGER",
										"FOX","RABBIT","ZEBRA","DEER","MONKEY","GORILLA","APE","PARROT","SLUG","WORM",
										"ELEPHANT","RAT"};
	
	public static String[] VERB     = {"LICKED","TOUCHED","KICKED","LIKED","ATE","EYED","SAW","SNIFFED","PUSHED",
										"PULLED","DRAGGED","DROPPED","CHEWED","PUNCHED","TICKLED","FLICKED","WASHED",
										"TOUCHED"};
	
	public static String[] CONJ     = {"AND","AS","BUT","WHEN","AFTER","BEFORE"};
	
	private static String       mFinalSentance;
	private static BigInteger   mTotalCount;
	private static SecureRandom mSecRand;
	
	public static void main(String[] zArgs) {
		//Get a secure random number generator..
		mSecRand = new SecureRandom();
		
		//generate a random seed phrase..
		mFinalSentance = "";
		mTotalCount    = BigInteger.ONE;
		
		//NOw start..
		addFullNoun();
		
		addRandom(VERB);
		
		addFullNoun();
		
		addRandom(CONJ);
		
		addFullNoun();
		
		addRandom(VERB);
		
		addFullNoun();
		
		//All done..
		System.out.println(mFinalSentance.trim());
		
		double sec = Maths.log2BI(mTotalCount);
		
		System.out.println("Total possible : "+mTotalCount);
		System.out.println("Security 2^"+sec);
	}
	
	private static void addFullNoun() {
		addRandom(ARTICLE);
		addRandom(ADJS);
		addRandom(COLOUR);
		addRandom(NOUNS);
	}
	
	private static void addRandom(String[] zWordBlock) {
		mFinalSentance += zWordBlock[mSecRand.nextInt(zWordBlock.length)]+" ";
		mTotalCount    = mTotalCount.multiply(new BigInteger(""+zWordBlock.length));
	}
	
}

package org.minima.utils;

import java.math.BigInteger;
import java.security.SecureRandom;

public class SeedPhrase {

	public static String[] ARTICLE  = 
		{"THE","A"};
	
	public static String[] ADJS     = 
		{"QUICK","SLOW","SLY","FAST","FAT","THIN","GREEDY","WARM","TIRED","SHORT","PLAIN",
		"CUNNING","FIT","CLEAN","SAD","HAPPY","LAZY","HUNGRY","LOVELY","NICE","SHINY","SCALY",
		"FURRY","SHY","CLEVER","POOR","TENDER","FAMOUS","VAST","RICH","GIFTED","SHORT","TALL",
		"BRAVE","GENTLE","CALM","JOLLY","KIND","POLITE","PROUD","SILLY","WITTY","FIERCE",
		"SCARY","WORRIED","CLUMSY","ANGRY","TINY","HELPFUL","HUGE","GREAT"};
	
	public static String[] COLOUR   = 
		{"RED","GREEN","BLUE","YELLOW","BLACK","WHITE","PINK","ORANGE","GREY",
		 "BRONZE","GOLDEN","SILVER","METAL","PURPLE","LEMON","MANGO"};
	
	
	public static String[] NOUNS    = {"CAT","DOG","BIRD","FISH","LION","HIPPO","WOLF","COW","MOUSE","TIGER",
										"FOX","RABBIT","ZEBRA","DEER","MONKEY","GORILLA","APE","PARROT","SLUG","WORM",
										"ELEPHANT","RAT"};
	
	public static String[] VERB     = 
		{"LICKED","TOUCHED","KICKED","LIKED","ATE","EYED","SAW","SNIFFED","PUSHED",
		"PULLED","DRAGGED","DROPPED","CHEWED","PUNCHED","TICKLED","FLICKED","WASHED",
		"TOUCHED","GRABBED","TWISTED","BROKE","PINCHED","STROKED"};
	
	public static String[] CONJ     = {"AND","AS","BUT","WHEN","AFTER","BEFORE"};
	
	private String        mFinalSentance;
	private BigInteger    mTotalCount;
	private SecureRandom  mSecRand;
	
	public SeedPhrase() {}
	
	private void addFullNoun() {
		addRandom(ARTICLE);
		addRandom(ADJS);
		addRandom(COLOUR);
		addRandom(NOUNS);
	}
	
	private void addRandom(String[] zWordBlock) {
		mFinalSentance += zWordBlock[mSecRand.nextInt(zWordBlock.length)]+" ";
		mTotalCount    = mTotalCount.multiply(new BigInteger(""+zWordBlock.length));
	}
	
	public String getNewSeedPhrase() {
		mFinalSentance 	= new String("");
		mSecRand 		= new SecureRandom();
		mTotalCount     = BigInteger.ZERO;
		
		//NOw start..
		addFullNoun();
		
		addRandom(VERB);
		
		addFullNoun();
		
		addRandom(CONJ);
		
		addFullNoun();
		
		addRandom(VERB);
		
		addFullNoun();
		
		return mFinalSentance;
	}
	
	public static void main(String[] zArgs) {
		SeedPhrase sp = new SeedPhrase();
		
		System.out.println(sp.getNewSeedPhrase());
	}
	
}

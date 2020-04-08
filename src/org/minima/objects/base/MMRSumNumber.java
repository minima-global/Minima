package org.minima.objects.base;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;


/**
 * VERY Simple Number class that only does addition for the MMRSum Tree..
 * 
 * Higher precision than MiniNumber
 * 
 * @author spartacusrex
 *
 */
public class MMRSumNumber {

	public static final MathContext mMathContext = new MathContext(64, RoundingMode.DOWN);
	
	/**
	 * The number representation
	 */
	private BigDecimal mNumber;
	
	private MMRSumNumber(BigDecimal zNumber) {
		mNumber = zNumber;
	}
	
	public MMRSumNumber(MiniNumber zNumber) {
		mNumber = new BigDecimal(zNumber.toString(), mMathContext);
	}
	
	public BigDecimal getNumber() {
		return mNumber;
	}
	
	public MMRSumNumber add(MMRSumNumber zNumber) {
		return new MMRSumNumber(mNumber.add(zNumber.getNumber(), mMathContext));
	}
	
	public boolean isEqual(MMRSumNumber zNumber) {
		return zNumber.getNumber().compareTo(mNumber) == 0;
	}
}

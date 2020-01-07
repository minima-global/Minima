package org.minima.utils.tests.numbers;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.text.DecimalFormat;

public class Precision {

	public static MathContext mMathContext = new MathContext(18, RoundingMode.DOWN);
	
	public class mynumber extends BigDecimal {
	
		String mOriginal;
		
		public mynumber(String zNum) {
			super(zNum,mMathContext);
		
			mOriginal = zNum;
		}
		
		public boolean isValid() {
			return toString().equals(mOriginal);
		}
		
		public String toString() {
			return toPlainString();
		}
		
		public String toSigString() {
			return toEngineeringString();
		}
		
		public mynumber add(mynumber zNum){
			BigDecimal bd = add(new BigDecimal(zNum.toString()));
			
			return new mynumber(bd.toPlainString());
		}
		
	}
	
	public Precision() {}
	
	
	public static void main(String[] zArgs) {
		
//		int decimal = 64;
//		
//		MathContext mMathContext = new MathContext(decimal+2, RoundingMode.DOWN);
//		
//		String tt = new String("1.");
//		
//		for(int i=0;i<decimal;i++){
//			tt +="0";
//		}
//		tt +="9";
//		
//		BigDecimal dd1 = new BigDecimal(tt, mMathContext);
//		
//		int len    = dd1.unscaledValue().toByteArray().length;
//		int scale  = dd1.scale();
//		int precis = dd1.precision();
//		
//		
//		System.out.println("Val :\n"+tt+"\n"+dd1.toPlainString()+"\nByte Array Length : "+len+"\nScale : "+scale+"\nPrecision :  "+precis);
		
		Precision pp = new Precision();
		
		
		mynumber num  = pp.new mynumber("1000000000.00000001");
		mynumber num2 = pp.new mynumber("1.003");
		
		System.out.println("Num1 : "+num.toSigString()+" valid:"+num.isValid());
		System.out.println("Num2 : "+num2.toSigString()+" valid:"+num2.isValid());
		
		mynumber num3 = num.add(num2);
		System.out.println("Num3 : "+num3.toSigString()+" valid:"+num3.isValid());
		
		DecimalFormat decform = new DecimalFormat("0.#################E0");
		
		String sig = decform.format(num);
		System.out.println("\n\nSIG: "+sig);
		
		BigDecimal ff = new BigDecimal(sig);
		System.out.println("SIGBD: "+ff.toPlainString());
		
		
	}
	
}

package org.minima.utils;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.minima.objects.base.MiniData;

public class ImageLoader {

	public static byte[] LoadImage(File zFile) {
		
		try {
			BufferedImage img 	= ImageIO.read(zFile);
		
			BufferedImage scaled = resizeImage(img, 64, 64);
			
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			
			ImageIO.write(scaled, "jpg", baos);
			 
			return baos.toByteArray(); 
		
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return null;
	}
	
	static BufferedImage resizeImage(BufferedImage originalImage, int targetWidth, int targetHeight) throws IOException {
	    BufferedImage resizedImage = new BufferedImage(targetWidth, targetHeight, BufferedImage.TYPE_INT_RGB);
	    Graphics2D graphics2D = resizedImage.createGraphics();
	    graphics2D.drawImage(originalImage, 0, 0, targetWidth, targetHeight, null);
	    graphics2D.dispose();
	    return resizedImage;
	}
	
	public static void main(String[] zArgs) throws IOException {
		
		File img = new File("C:\\Users\\spartacusrex\\Pictures\\axeman.PNG");
		
		byte[] data = LoadImage(img);
		
		System.out.println("IMG : "+data.length);
		
		MiniFile.writeDataToFile(new File("image.jpg") , data);
		
	}
}

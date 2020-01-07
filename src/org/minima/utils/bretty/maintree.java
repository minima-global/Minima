package org.minima.utils.bretty;

import org.minima.utils.MinimaLogger;

public class maintree {
	
	public static void main(String[] zArgs) {
		
		TreeNode a = new TreeNode("a");
		TreeNode b = new TreeNode("b");
		TreeNode c = new TreeNode("c");
		TreeNode d = new TreeNode("d");
		TreeNode e = new TreeNode("e");
		TreeNode f = new TreeNode("f");
		TreeNode g = new TreeNode("g");
		TreeNode h = new TreeNode("h");
		TreeNode i = new TreeNode("i");
		TreeNode j = new TreeNode("j");
		TreeNode k = new TreeNode("k");
		TreeNode l = new TreeNode("l");

		a.addChild(b);
		a.addChild(c);
		a.addChild(d);
		b.addChild(e);
		b.addChild(f);
		c.addChild(g);
		c.addChild(h);
		d.addChild(i);
		g.addChild(j);
		g.addChild(k);
		k.addChild(l);
		
		// "a" is the root of the tree constructed above
		String output = TreePrinter.toString(a);
		MinimaLogger.log(output);
		
//		File root = new File("~/Documents");
//		String output = TreePrinter.toString(root, StandardTreeNodeConverters.FILE);
//		SimpleLogger.log(output);
		
//		TreeNodeConverter<File> converter = new TreeNodeConverter<File>() {
//			
//			@Override
//			public String name(File t) {
//				// TODO Auto-generated method stub
//				return t.getName();
//			}
//			
//			@Override
//			public List<? extends File> children(File t) {
//				List<File> files = new ArrayList<>();
//			    if (t.isDirectory()) {
//					files.addAll(Arrays.asList(t.listFiles()));
//			    }
//			    return files;
//			}
//		};
//			
//		String output = TreePrinter.toString(root, converter);
//		SimpleLogger.log(output);
		
		
	}
}

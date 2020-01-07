package org.minima.utils.bretty;

import java.util.ArrayList;
import java.util.List;

public class TreeNode implements PrintableTreeNode{

	private String name;
	private List<TreeNode> children;

	public TreeNode(String name) {
		this.name = name;
		this.children = new ArrayList<>();
	}

	public void addChild(TreeNode child){
		this.children.add(child);
	}
	
	public String name() {
		// return the name of the node that you wish to print later
		return this.name;
	}

	public List<TreeNode> children() {
		// return the list of children of this node
		return this.children;
	}
}
package org.minima.utils;

import java.util.ArrayList;

public class Stack {
	
	ArrayList<Object> mStack;
	
	public Stack() {
		mStack = new ArrayList<>();
	}
	
	/**
	 * Add a node to the top of the Stack
	 * 
	 * @param zNode
	 */
	public void push(Object zObject) {
		mStack.add(zObject);
	}
	
	/**
	 * Pop a node off the top of the stack
	 * @return
	 */
	public Object pop() {
		if(isEmpty()) {
			return null;
		}
		
		return mStack.remove(mStack.size()-1);
	}
	
	/**
	 * Peek at the top node
	 * @return
	 */
	public Object peek() {
		if(isEmpty()) {
			return null;
		}
		
		return mStack.get(mStack.size()-1);
	}

	/**
	 * Empty Stack 
	 * @return
	 */
	public boolean isEmpty() {
		return mStack.size() == 0;
	}
}

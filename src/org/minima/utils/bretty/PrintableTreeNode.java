package org.minima.utils.bretty;

import java.util.List;

/**
 * Your own class that represents a tree node must implement this interface
 */
public interface PrintableTreeNode {
    /**
     * @return the name of the node that you wish to print later
     */
    String name();

    /**
     * @return a list of children of this node
     */
    List<? extends PrintableTreeNode> children();
}

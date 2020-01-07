package org.minima.utils.bretty;

import java.util.List;

/**
 * If you already have a class that represents a tree node (e.g. java {@code File} class), but you couldn't or just
 * don't want to modify the source code of that class to implement the {@link PrintableTreeNode} interface, you can
 * create your own {@link TreeNodeConverter} object that works as an adapter for your tree node class
 *
 * @param <T> The class of your own tree node
 */

public interface TreeNodeConverter<T> {

    /**
     * @param t an object of your tree node class
     * @return the name of the node that you wish to print later
     */
    String name(T t);

    /**
     * @param t the current node
     * @return a list of children of this node
     */
    List<? extends T> children(T t);

}

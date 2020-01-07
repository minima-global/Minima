package org.minima.utils.bretty;

import java.util.List;

/**
 * A static utility class that converts a tree into a formatted string
 */
public class TreePrinter {

    private TreePrinter() {
    }

    private static void process(PrintableTreeNode current, String prefix, boolean isRoot, boolean lastInPeers, StringBuilder stringBuilder) {
        stringBuilder.append(prefix);
        if (!isRoot) {
            stringBuilder.append((lastInPeers ? "└── " : "├── "));
        } else {
            stringBuilder.append(" ── ");
        }
        stringBuilder.append(current.name());
        stringBuilder.append('\n');

        List<? extends PrintableTreeNode> children = current.children();

        String indentation = isRoot ? "    " : "    ";
        for (int i = 0; i < children.size() - 1; ++i) {
            process(children.get(i), prefix + (lastInPeers ? indentation : "│   "), false, false, stringBuilder);
        }

        if (children.size() > 0) {
            process(children.get(children.size() - 1), prefix + (lastInPeers ? indentation : "│   "), false, true, stringBuilder);
        }
    }

    private static <T> void process(T current, TreeNodeConverter<T> treeNodeConverter, String prefix, boolean isRoot, boolean lastInPeers, StringBuilder stringBuilder) {
        stringBuilder.append(prefix);
        if (!isRoot) {
            stringBuilder.append((lastInPeers ? "└── " : "├── "));
        } else {
            stringBuilder.append(" ── ");
        }
        stringBuilder.append(treeNodeConverter.name(current));
        stringBuilder.append('\n');

        List<? extends T> children = treeNodeConverter.children(current);

        String indentation = isRoot ? "    " : "    ";

        for (int i = 0; i < children.size() - 1; ++i) {
            process(children.get(i), treeNodeConverter, prefix + (lastInPeers ? indentation : "│   "), false, false, stringBuilder);
        }

        if (children.size() > 0) {
            process(children.get(children.size() - 1), treeNodeConverter, prefix + (lastInPeers ? indentation : "│   "), false, true, stringBuilder);
        }
    }

    /**
     * Convert a tree to a string
     *
     * @param root - the root node of the tree
     * @return a formatted string that is ready to be printed
     */
    public static String toString(PrintableTreeNode root) {
        StringBuilder stringBuilder = new StringBuilder();

        TreeNodeConverter<PrintableTreeNode> converter = new TreeNodeConverter<PrintableTreeNode>() {
            @Override
            public String name(PrintableTreeNode printableTreeNode) {
                return printableTreeNode.name();
            }

            @Override
            public List<? extends PrintableTreeNode> children(PrintableTreeNode printableTreeNode) {
                return printableTreeNode.children();
            }
        };

        process(root, converter, "", true, true, stringBuilder);
        return stringBuilder.toString();
    }

    /**
     * Convert a tree to a string
     *
     * @param root              the root node
     * @param treeNodeConverter a converter that allows T object works like a tree node
     * @param <T>               the type of the nodes
     * @return a formatted string that is ready to be printed
     */
    public static <T> String toString(T root, TreeNodeConverter<T> treeNodeConverter) {
        StringBuilder stringBuilder = new StringBuilder();
        process(root, treeNodeConverter, "", true, true, stringBuilder);
        return stringBuilder.toString();
    }
}

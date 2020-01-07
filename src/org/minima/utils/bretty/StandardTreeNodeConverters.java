package org.minima.utils.bretty;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * The default implementation of {@link TreeNodeConverter} for common java classes
 */
public class StandardTreeNodeConverters {

    /**
     * The default implement for java {@code File} class
     */
    public static final TreeNodeConverter<File> FILE = new TreeNodeConverter<File>() {
        @Override
        public String name(File file) {
            return file.getName();
        }

        @Override
        public List<? extends File> children(File file) {
            List<File> files = new ArrayList<>();
            if (file.isDirectory()) {
                files.addAll(Arrays.asList(file.listFiles()));
            }
            return files;
        }
    };


}

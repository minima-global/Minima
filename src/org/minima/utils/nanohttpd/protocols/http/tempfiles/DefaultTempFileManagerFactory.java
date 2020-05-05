package org.minima.utils.nanohttpd.protocols.http.tempfiles;

import org.minima.utils.nanohttpd.util.IFactory;

/**
 * Default strategy for creating and cleaning up temporary files.
 */
public class DefaultTempFileManagerFactory implements IFactory<ITempFileManager> {

    @Override
    public ITempFileManager create() {
        return new DefaultTempFileManager();
    }
}

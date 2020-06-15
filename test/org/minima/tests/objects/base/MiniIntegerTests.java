package org.minima.tests.objects.base;

import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.minima.objects.base.MiniInteger;

public class MiniIntegerTests {
    
    @Test
    public void testAssertNotNull() {
      assertNotNull("should not be null", new MiniInteger(0));
    }
    
}
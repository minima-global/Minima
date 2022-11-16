package org.minima.system.params;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.*;

@RunWith(JUnit4.class)
public class ParamConfigurerTest {

    @Test
    public void usingNothing() {

        String[] progArgs = new String[] {};
        ParamConfigurer configurer = new ParamConfigurer()
                .usingConfFile(progArgs)
                .usingEnvVariables(new HashMap<>())
                .usingProgramArgs(progArgs)
                .configure();

        assertEquals(9001, GeneralParams.MINIMA_PORT);
        assertFalse(configurer.isRpcenable());
        assertFalse(configurer.isDaemon());
    }

    @Test
    public void usingProgramArgs() {

        String[] progArgs = new String[] {"-port", "8888",
                "-rpcenable",
                "-daemon",
                "-host", "124.0.1.10",
                "-data", "mydaatafolder",
                "-p2pnode", "124.0.1.9",
                "-genesis",
                "-nop2p",
                "-noconnect",
                "-mobile",
                "-test",
                "-connect", "124.0.1.9:7777"};
        ParamConfigurer configurer = new ParamConfigurer()
                .usingConfFile(progArgs)
                .usingEnvVariables(new HashMap<>())
                .usingProgramArgs(progArgs)
                .configure();

        assertEquals(8888, GeneralParams.MINIMA_PORT);
//        assertEquals("mydaatafolder", GeneralParams.DATA_FOLDER);
        assertEquals("124.0.1.10", GeneralParams.MINIMA_HOST);
        assertTrue(GeneralParams.IS_HOST_SET);
//        assertTrue(GeneralParams.AUTOMINE);
        assertTrue(GeneralParams.NOCONNECT);
        assertTrue(GeneralParams.IS_MOBILE);
        assertFalse(GeneralParams.P2P_ENABLED);
        assertEquals("124.0.1.9", GeneralParams.P2P_ROOTNODE);
        assertEquals("124.0.1.9:7777", GeneralParams.CONNECT_LIST);
        assertTrue(configurer.isRpcenable());
        assertTrue(configurer.isDaemon());
        assertTrue(GeneralParams.CLEAN);
//        assertTrue(GeneralParams.PRIVATE_NETWORK);
        assertTrue( GeneralParams.GENESIS);
//        assertTrue(GeneralParams.AUTOMINE);
        assertTrue(GeneralParams.TEST_PARAMS);
        assertEquals(TestParams.MINIMA_BLOCK_SPEED, GlobalParams.MINIMA_BLOCK_SPEED);

    }

    @Test( expected = ParamConfigurer.UnknownArgumentException.class)
    public void invalidProgramArgs() {

        String[] progArgs = new String[] {"-port", "8888",
                "-rpcenable",
                "-notvalid",
                "-connect", "124.0.1.9:7777"};
        ParamConfigurer configurer = new ParamConfigurer()
                .usingConfFile(progArgs)
                .usingEnvVariables(new HashMap<>())
                .usingProgramArgs(progArgs)
                .configure();

        fail(); // if we get this far
    }

    @Test
    public void usingEnvVars() {

        Map<String, String> envVarMap =new HashMap<>();
        envVarMap.put("MINIMA_CONNECT", "124.0.1.9:7777");
        envVarMap.put("MINIMA_RPCENABLE", "TRUE");
        envVarMap.put("MINIMA_DAEMON", "true");
        envVarMap.put("MINIMA_NOCONNECT", "something");
        envVarMap.put("OTHER_PROG_NOCONNECT", "something");

        String[] progArgs = new String[] {"-port", "8888"};
        ParamConfigurer configurer = new ParamConfigurer()
                .usingConfFile(progArgs)
                .usingEnvVariables(envVarMap)
                .usingProgramArgs(progArgs)
                .configure();

        assertEquals(8888, GeneralParams.MINIMA_PORT);
        assertFalse(GeneralParams.NOCONNECT);
        assertEquals("124.0.1.9:7777", GeneralParams.CONNECT_LIST);
        assertTrue(configurer.isRpcenable());
        assertTrue(configurer.isDaemon());

    }

    @Test
    public void usingConfFile() throws IOException {

        final File confFile = File.createTempFile(UUID.randomUUID().toString(), "conf");

        FileWriter fw1 = new FileWriter( confFile );
        BufferedWriter bw1 = new BufferedWriter( fw1 );
        bw1.write( "port=999\nhost=19.0.1.6");
        bw1.close();

        Map<String, String> envVarMap =new HashMap<>();
        envVarMap.put("MINIMA_CONNECT", "124.0.1.9:7777");

        String[] progArgs = new String[] {"-port", "8888", "-conf", confFile.getAbsolutePath()};
        ParamConfigurer configurer = new ParamConfigurer()
                .usingConfFile(progArgs)
                .usingEnvVariables(envVarMap)
                .usingProgramArgs(progArgs)
                .configure();

        assertEquals(8888, GeneralParams.MINIMA_PORT);
        assertEquals("19.0.1.6", GeneralParams.MINIMA_HOST);
    }
}
package gitlet;

import ucb.junit.textui;
import org.junit.Test;
import static org.junit.Assert.*;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.junit.Before;
import org.junit.Test;

//This is copied, but the gitlet(whatever) works, while in the Unit Tests it doesn't....



/** The suite of all JUnit tests for the gitlet package.
 *  @author
 */
public class UnitTest {

    /** Run the JUnit tests in the loa package. Add xxxTest.class entries to
     *  the arguments of runClasses to run other JUnit tests. */
    public static void main(String[] ignored) {
        textui.runClasses(UnitTest.class);
    }

    private static final String gitlet_directory = ".gitlet/";
    private static final String directorytest = "testfiles/";
    private static final String splitline = "\r\n|[\r\n]";
    @Before
    public void setUp() {
        File f = new File(gitlet_directory);
        if (f.exists()) {
            deleteagain(f);
        }
        f = new File(directorytest);
        if (f.exists()) {
            deleteagain(f);
        }
        f.mkdirs();
    }

    @Test
    public void init_test() throws IOException {
        gitlet("init");
        File f = new File(gitlet_directory);
        assertTrue(f.exists());
    }

    @Test
    public void testCheckout() throws IOException {
        String applefile = directorytest + "apple.txt";
        String appletext = "apple";
        makefile(applefile, appletext);
        gitlet("init");
        gitlet("add", applefile);
        gitlet("commit", "first apple");
        writeFile(applefile, "apple 2.0");
        gitlet("checkout", applefile);
        assertEquals(appletext, getText(applefile));
        
        writeFile(applefile, "apple 2.0");
        gitlet("add", applefile);
        gitlet("commit", "apple 2");
        gitlet("branch", "apple branch");
        gitlet("checkout", "apple branch");
        
        gitlet("checkout", "2 " + applefile);
        assertEquals("apple 2.0", getText(applefile));
        
    }

    @Test
    public void testBasicLog() throws IOException {
        gitlet("init");
        String commitMessageA = "initial commit";

        String wugFile = directorytest + "wug.txt";
        String wugText = "This is a wug.";
        makefile(wugFile, wugText);
        gitlet("add", wugFile);
        String commitMessageB = "added wug";
        gitlet("commit", commitMessageB);

        String logContent = gitlet("log");
        assertArrayEquals(new String[] { commitMessageB, commitMessageA },
                getmessages(logContent));
    }
    
    @Test
    public void testGlobalLog() throws IOException {
        String fileA = directorytest + "test.txt";
        String content = "test";
        makefile(fileA, content);
        gitlet("init");
        gitlet("add", fileA);
        gitlet("commit", "first test");
        writeFile(fileA, "test 2.0");
        gitlet("checkout", fileA);
        assertEquals(content, getText(fileA));
        
        gitlet("reset", "1");
        String a = "test 3";
        String b = "test 2";
        String c = "first apple";1
        String d = "initial commit";
        
        String logContent = gitlet("global-log");
        String[] x = new String[] { d, c, b, a};
        assertArrayEquals(x, getmessages(logContent));
    }
    
//Extra Methods to help with My Tests

    private static String getText(String name) {
        try {
            byte[] encoded = Files.readAllBytes(Paths.get(name));
            return new String(encoded, StandardCharsets.UTF_8);
        } catch (IOException e) {
            return "";
        }
    }
    private static void makefile(String name, String content) {
        File f = new File(name);
        if (!f.exists()) {
            try {
                f.createNewFile();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        writeFile(name, content);
    }

    private static void writeFile(String name, String content) {
        FileWriter filewritten = null;
        try {
            File f = new File(name);
            filewritten = new FileWriter(f, false);
            filewritten.write(content);
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                filewritten.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private static void deleteagain(File d) {
        if (d.isDirectory()) {
            for (File f : d.listFiles()) {
                deleteagain(f);
            }
        }
        d.delete();
    }

    private static String[] getmessages(String logOutput) {
        String[] logChunks = logOutput.split("====");
        int numMessages = logChunks.length - 1;
        String[] messages = new String[numMessages];
        for (int i = 0; i < numMessages; i++) {
            System.out.println(logChunks[i + 1]);
            String[] logLines = logChunks[i + 1].split(splitline);
            messages[i] = logLines[3];
        }
        return messages;
    }

}



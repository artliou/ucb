package gitlet;

import java.util.HashMap;
import java.util.List;
import java.io.File;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.io.Serializable;


/** Class for commits, which stores commit information and contents.
* @author Arthur Liou and Zoey Kenny */
public class Commit implements Serializable {

    /** Constructor for the initial commit. */
    public Commit() {
        this.timestamp = LocalDateTime.now();
        this.message = "initial commit";
        this.commitid = Utils.sha1(message);
    }

    /** Constructor for other commits.
    *@param m
            The message for this commit.
    *@param all
    *        All blobs contained in this commit.
    */
    public Commit(String m, HashMap<String, byte[]> all) {
        timestamp = LocalDateTime.now();
        this.message = m;
        this.blobs = all;
        List<Object> files = new ArrayList<Object>();
        for (String key : blobs.keySet()) {
            byte[] contents = blobs.get(key);
            String words = new String(contents);
            files.add(words);
        }
        files.add(message);
        this.commitid = Utils.sha1(files);
    }

    /** Returns the message associated with this commit. */
    public String getmessage() {
        return message;
    }

    /** Returns the date associated with this commit. */
    public LocalDateTime gettime() {
        return timestamp;
    }

    /** Returns the id associated with this commit. */
    public String commitid() {
        return commitid;
    }

    /** Returns the blobs contained in this commmit. */
    public HashMap<String, byte[]> blobs() {
        return blobs;
    }

    /** The ID of this commit. */
    private String commitid;

    /** The time at which this commit was created. */
    private LocalDateTime timestamp;

    /** The commit message saved with this commit. */
    private String message;

    /** All blobs contained in this commit. */
    private HashMap<String, byte[]> blobs;

}

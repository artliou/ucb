package gitlet;

import java.util.HashMap;
import java.util.List;
import java.util.TreeMap;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.util.Arrays;


/** The class that creates a new instance of gitlet.
* @author Arthur Liou and Zoey Kenny
*/

public class NewGit implements Serializable {

    /** Constructor for a new gitlet. Is empty until init() is called. */
    public NewGit() {   }

    /** Initializes the directory and creates a master branch. */
    public void init() {
        File dir = new File(".gitlet");
        if (!dir.exists()) {
            currentbranch = "master";
            currentcommit = new CommitChain(new Commit(), null);
            addedfiles = new HashMap<String, byte[]>();
            removedfiles = new ArrayList<String>();
            branches = new TreeMap<String, CommitChain>();
            allcommits = new HashMap<String, Commit>();
            branches.put(currentbranch, currentcommit);
            String m = currentcommit.head().getmessage();
            allcommits.put(m, currentcommit.head());
            dir.mkdir();
        } else {
            String m1 = "A gitlet version control system already ";
            String m2 = "exists in the current directory.";
            System.out.println(m1 + m2);
        }
    }


    /** Marks a file to be added in the next commit.
    @param name
    *       The name of the file being added.
    */
    public void add(String name) {
        File newfile = new File(name);
        if (!newfile.exists() || newfile.isDirectory()) {
            System.out.println("File does not exist.");
            return;
        }
        byte[] contents = Utils.readContents(newfile);
        Commit last = currentcommit.head();
        if (!last.getmessage().equals("initial commit")) {
            if (last.blobs().containsKey(name)) {
                byte[] content = last.blobs().get(name);
                if (Arrays.equals(content, contents)) {
                    return;
                }
            }
        }
        addedfiles.put(name, contents);

        // File file = new File("testing\\src\\" + name);
        // byte[] content = Utils.readContents(file);
        // String s = new String(content);
        // System.out.println("file 1 : " + s);

        // File file1 = new File("test");
        // Utils.writeContents(file1, Utils.readContents(file));
        // byte[] contents = Utils.readContents(file1);
        // String t = new String(contents);
        // System.out.println("file 2 : " + t);
    }

    /** Commits the current new files with the message given.
    *@param message
    *       The message associated with the commit. */
    public void commit(String message) {
        if (addedfiles.isEmpty() && removedfiles.isEmpty()) {
            System.out.println("No changes added to the commit.");
            return;
        } else {
            HashMap<String, byte[]> newmap = new HashMap<String, byte[]>();
            Commit last = currentcommit.head();
            if (!last.getmessage().equals("initial commit")) {
                HashMap<String, byte[]> lastcommit = currentcommit.head().blobs();
                newmap.putAll(lastcommit);
                for (String key1 : addedfiles.keySet()) {
                    for (String key2 : newmap.keySet()) {
                        if (key1.equals(key2)) {
                            newmap.remove(key2);
                        }
                    }
                }   
            }
            for (String key : addedfiles.keySet()) {
                newmap.put(key, addedfiles.get(key));
            }
            for (String key : removedfiles) {
                newmap.remove(key);
            }
            Commit updated = new Commit(message, newmap);
            currentcommit = new CommitChain(updated, currentcommit);
            allcommits.put(message, updated);
            branches.remove(currentbranch);
            branches.put(currentbranch, currentcommit);
            for (String file : newmap.keySet()) {
                File dir = new File(".gitlet");
                File child = new File(dir, file);
            }
            addedfiles.clear();
            removedfiles.clear();
        }
    }

    /** Removes file from the working directory or the list of added files.
    *@param file
    *       The file to be removed.
    */
    public void remove(String file) {
        Commit last = currentcommit.head();
        if (last.getmessage().equals("initial commit")){
            if (!addedfiles.containsKey(file)) {
                System.out.println("No reason to remove the file.");
                return;
            }
        } else {
            if (!last.blobs().containsKey(file) && !addedfiles.containsKey(file)) {
                System.out.println("No reason to remove the file.");
                return;
            }
            if (last.blobs().containsKey(file)) {
                try {
                    File toremove = new File(file);
                    Files.delete(toremove.toPath());
                } catch (IOException e) {
                    ;
                }
            }
        }
        if (addedfiles.containsKey(file)) {
            addedfiles.remove(file);
        }
        removedfiles.add(file);
    }

    /** Prints a log all of all the commits made prior to the current one. */
    public void printlog() {
        CommitChain pointer = currentcommit;
        while (pointer != null) {
            Commit thiscommit = pointer.head();
            System.out.println("===");
            System.out.println("Commit " + thiscommit.commitid());
            String committime = thiscommit.gettime().toString();
            String[] time = committime.split("T");
            int nano = time[1].indexOf(".");
            System.out.println(time[0] + " " + time[1].substring(0, nano));
            System.out.println(thiscommit.getmessage());
            if (pointer.hasnext()) {
                System.out.println();
            }
            pointer = pointer.next();
        }
    }

    /** Prints a log of all the commits ever made in this directory. */
    public void printgloballog() {
        Commit current;
        for (String key : allcommits.keySet()) {
            current = allcommits.get(key);
            System.out.println("===");
            System.out.println("Commit " + current.commitid());

            String committime = current.gettime().toString();
            String[] time = committime.split("T");
            int nano = time[1].indexOf(".");
            System.out.println(time[0] + " " + time[1].substring(0, nano));
            System.out.println(current.getmessage());
            System.out.println();
        }
    }

    /** Creates a new branch.
    *@param name
            The name of the new branch.
    */
    public void newbranch(String name) {
        if (branches.containsKey(name)) {
            System.out.println("A branch with that name already exists.");
        } else {
            branches.put(name, currentcommit);
        }
    }

    /** Deletes a branch from the map of branches.
    *@param name
    *       The name of the branch to be removed.
    */
    public void removebranch(String name) {
        if (branches.containsKey(name)) {
            String m = "A branch with that name does not exist.";
            System.out.println(m);
        }
        if (currentbranch.equals(name)) {
            System.out.println("Cannot remove the current branch.");
        } else {
            branches.remove(name);
        }
    }


    /** Tests to see if a commit with the given id exists.
    *@param id
    *       The id given.
    *@return True if the commit exists, otherwise false.
    */
    public boolean commitexists(String id) {
        for (String key : allcommits.keySet()) {
            if (allcommits.get(key).commitid().equals(id)) {
                return true;
            }
        }
        return false;
    }

    /** Returns the commit associated with the id given, if it exists.
    * @param id
    *        The commit id to be searched for.
    */
    public Commit findcommit(String id) {
        for (String key : allcommits.keySet()) {
            if (allcommits.get(key).commitid().equals(id)) {
                return allcommits.get(key);
            }
        }
        return null;
    }

    /** Prints current status of this directory, such as which branch
    * it is currently on and what files are to be added next.
    */
    public void status() {
        System.out.println("=== Branches ===");
        for (String key : branches.keySet()) {
            if (key.equals(currentbranch)) {
                System.out.println("*" + key);
            } else {
                System.out.println(key);
            }
        }
        System.out.println();
        System.out.println("=== Staged Files ===");
        for (String key : addedfiles.keySet()) {
            System.out.println(key);
        }
        System.out.println();
        System.out.println("=== Removed Files ===");
        for (String key : removedfiles) {
            System.out.println(key);
        }
        System.out.println();
        System.out.println("=== Modifications Not Staged For Commit ===");
        System.out.println();
        System.out.println("=== Untracked Files ===");
        System.out.println();
    }

    /** Merges the indicated branch with the current one.
    * @param branch
    *        The branch being merged.
    */
    public void merge(String branch) {
        return;
    }

    /** The reset function.
    *@param s
            The name of the branch to reset to.
    */
    public void reset(String s) {
        return;
    }

    /** Call the required checkout function.
    *@param operands
    *        The arguments given for checkout.
    */
    public void checkout(String[] operands) {
        if (operands.length == 1) {
            checkoutbranch(operands[0]);
        }
        if (operands.length == 2) {
            checkoutfile(operands[1]);
        }
        if (operands.length == 3) {
            checkoutcommit(operands[0], operands[2]);
        }
    }

    /** The checkout function for file name.
    *@param file
    *        The file being changed.
    */
    public void checkoutfile(String file) {
        Commit lastcommit = currentcommit.head();
        HashMap<String, byte[]> commitfiles = lastcommit.blobs();
        if (!commitfiles.containsKey(file)) {
            String m = "File does not exist in the most recent commit";
            System.out.println(m);
            return;
        }
        File newfile = new File(file);
        byte[] overwrite = commitfiles.get(file);
        Utils.writeContents(newfile, overwrite);
    }

    /** The checkout function for branch.
    *@param branch
    *       The branch being reverted to.
    */
    public void checkoutbranch(String branch) {
        if (!branches.containsKey(branch)) {
            System.out.println("No such branch exists.");
            return;
        }
        if (currentbranch.equals(branch)) {
            System.out.println("No need to checkout the current branch.");
            return;
        }
        currentbranch = branch;
        currentcommit = branches.get(currentbranch);
        HashMap<String, byte[]> files = currentcommit.head().blobs();

        String workingdir = System.getProperty("user.dir");
        File dir = new File(workingdir);
        File[] content = dir.listFiles();
        //test if there are untracked files in danger
        for (File file : content) {
            if (!files.containsKey(file.toString())) {
                String m = "There is an untracked file in the way;";
                String n = " delete it or add it first.";
                System.out.println(m + n);
                return;
            }
        }
        //replace files with same name
        try {
            for (String name : files.keySet()) {
                for (File file : content) {
                    if (file.toString().equals(name)) {
                        Utils.writeContents(file, files.get(name));
                    } else {
                        File newf = new File(name);
                        newf.createNewFile();
                        Utils.writeContents(newf, files.get(name));
                    }
                }
            }
        } catch (IOException e) {
            ;
        }
        addedfiles.clear();
        removedfiles.clear();
        return;
    }

    /** The checkout function for commit id and file name.
    *@param id
    *       The commit id being searched for.
    *@param file
    *       The file being changed.
    */
    public void checkoutcommit(String id, String file) {
        if (!commitexists(id)) {
            System.out.println("No commit with that ID exists.");
        } else {
            Commit commit = findcommit(id);
            HashMap<String, byte[]> commitfiles = commit.blobs();
            if (!commitfiles.containsKey(file)) {
                System.out.println("File does not exist in that commit.");
            } else {
                String workingdir = System.getProperty("user.dir");
                File dir = new File(workingdir);
                File[] content = dir.listFiles();
                for (File fil : content) {
                    if (fil.toString().equals(file)) {
                        byte[] newcontent = commitfiles.get(file);
                        Utils.writeContents(fil, newcontent);
                    } else {
                        File newf = new File(file);
                        try {
                            newf.createNewFile();
                            Utils.writeContents(newf, commitfiles.get(file));
                        } catch (IOException e) {
                            ;
                        }
                    }
                }
            }
        }
    }

    /** Searches for a commit based on its message.
    *@param message
    *       The message of the commit being search for.
    */
    public String find(String message) {
        //fix me :(
        return "rip";
    }



    /** Returns branches. */
    public TreeMap<String, CommitChain> getbranches() {
        return branches;
    }

    /** Returns the current branch. */
    public String getcurrentbranch() {
        return currentbranch;
    }

    /** A hashmap containing the files marked to be added in the next commit. */
    private HashMap<String, byte[]> addedfiles;

    /** A List containing the names of files to be removed. */
    private List<String> removedfiles;

    /** A chain of commits, for which the head is the most current commit. */
    private CommitChain currentcommit;

    /** The branch that gitlet is currently on. */
    private String currentbranch;

    /**A TreeMap containing all the branches of this directory. */
    private TreeMap<String, CommitChain> branches;

    /**A HashMap containing all commits made in this version of gitlet. */
    private HashMap<String, Commit> allcommits;
}

package gitlet;


import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;
import java.io.Serializable;

/** Driver class for Gitlet, the tiny stupid version-control system.
 *  @author Arthur Liou and Zoey Kenny
 */
public class Main implements Serializable {

    /** Usage: java gitlet.Main ARGS, where ARGS contains
     *  <COMMAND> <OPERAND> .... */
    public static void main(String[] args) {
        String command = args[0];
        String[] operands = new String[args.length - 1];
        System.arraycopy(args, 1, operands, 0, operands.length);
        NewGit minigit = loadGitlet();
        // try {
            switch (command) {
            case "init":
                minigit.init();
                break;
            case "add":
                minigit.add(args[1]);
                break;
            case "commit":
                if (operands.length == 0) {
                    System.out.println("Please enter a commit message.");
                } else {
                    minigit.commit(args[1]);
                }
                break;
            case "rm":
                minigit.remove(args[1]);
                break;
            case "log":
                minigit.printlog();
                break;
            case "global-log":
                minigit.printgloballog();
                break;
            case "checkout":
                minigit.checkout(operands);
                break;
            case "branch":
                minigit.newbranch(operands[0]);
                break;
            case "rm-branch":
                minigit.removebranch(operands[0]);
                break;
            case "status":
                minigit.status();
                break;
            case "reset":
                if (!minigit.commitexists(operands[0])) {
                    System.out.println("No commit with that ID exists.");
                } else {
                    minigit.reset(operands[0]);
                }
                break;
            case "merge":
                minigit.merge(operands[0]);
                break;
            case "find":
                minigit.find(operands[0]);
            default:
                break;
            }
            saveGitlet(minigit);
        // } catch (NullPointerException | ArrayIndexOutOfBoundsException e) {
        //     System.out.println(e.getMessage());
        // }
    }

    /** Loads the existing verson of gitlet.
    *@return The loaded gitlet, if it exists.
    */
    private static NewGit loadGitlet() {
        NewGit git = new NewGit();
        File saved = new File("gitlet.serialized");
        if (saved.exists()) {
            try {
                FileInputStream fileIn = new FileInputStream(saved);
                ObjectInputStream objectIn = new ObjectInputStream(fileIn);
                git = (NewGit) objectIn.readObject();
                fileIn.close();
                objectIn.close();
            } catch (ClassNotFoundException e) {
                System.out.println("No previous gitlet found.");
            } catch (FileNotFoundException e) {
                System.out.println("No previous gitlet found.");
            } catch (IOException e) {
                System.out.println("No previous gitlet found.");
            }
        }
        return git;
    }

    /** Saves the current state of gitlet.
    * @param git
    *        current version of git
    */
    private static void saveGitlet(NewGit git) {
        if (git == null) {
            return;
        }
        try {
            File saved = new File("gitlet.serialized");
            FileOutputStream fileOut = new FileOutputStream(saved);
            ObjectOutputStream objectOut = new ObjectOutputStream(fileOut);
            objectOut.writeObject(git);
            fileOut.close();
            objectOut.close();
        } catch (IOException e) {
            System.out.print("IO exception while saving.");
        }
    }
}

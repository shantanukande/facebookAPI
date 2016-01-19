/**
 * Created by Shantanu on 12/13/2015.
 */
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.util.Base64;
import javax.crypto.Cipher;
import java.security.Key;
import java.security.Security;
import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
public class EncryptionUtil{

    public static final String ALGORITHM = "RSA";
    //location where the user's public and private keys are stored: "D:/keys/user"
    public String PRIVATE_KEY_FILE = "D:/keys/user";
    public String PUBLIC_KEY_FILE = "D:/keys/user";


    public String generateKey(int myID) {
        //generation of public and private keys for each user
        PRIVATE_KEY_FILE = PRIVATE_KEY_FILE+myID+"/private.key";
        PUBLIC_KEY_FILE = PUBLIC_KEY_FILE+myID+"/public.key";

        System.out.println(PRIVATE_KEY_FILE);
        System.out.println(PUBLIC_KEY_FILE);

        try {
            final KeyPairGenerator keyGen = KeyPairGenerator.getInstance(ALGORITHM);
            keyGen.initialize(2048);                // RSA - 2048 has been used
            final KeyPair key = keyGen.generateKeyPair();
            File privateKeyFile = new File(PRIVATE_KEY_FILE);
            File publicKeyFile = new File(PUBLIC_KEY_FILE);


            // Create files to store public and private key
            if (privateKeyFile.getParentFile() != null) {
                privateKeyFile.getParentFile().mkdirs();
            }
            privateKeyFile.createNewFile();
            if (publicKeyFile.getParentFile() != null) {
                publicKeyFile.getParentFile().mkdirs();
            }
            publicKeyFile.createNewFile();

            // Saving the Private key in a file
            ObjectOutputStream privateKeyOS = new ObjectOutputStream(
                    new FileOutputStream(privateKeyFile));
            privateKeyOS.writeObject(key.getPrivate());
            privateKeyOS.close();

            // Saving the Public key in a file
            ObjectOutputStream publicKeyOS = new ObjectOutputStream(
                    new FileOutputStream(publicKeyFile));
            publicKeyOS.writeObject(key.getPublic());
            publicKeyOS.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
        return PRIVATE_KEY_FILE+" "+PUBLIC_KEY_FILE;
    }

    /* if public and private keys are already existing, i.e. they are already created in the session, then they need to be handled or used.
    the method checks whether keys are already existing in the system and then handles them*/
        public boolean areKeysPresent() {
            File privateKey = new File(PRIVATE_KEY_FILE);
            File publicKey = new File(PUBLIC_KEY_FILE);

        if (publicKey.exists() && privateKey.exists()) {
            return true;
        }
        return false;
    }

    /*encryption using RSA-2048 (note that it is an existing implementation and we have just borrowed the libraries so that the security protocols
    are universal and properly adhered to as mentioned in the lectures)*/

    public byte[] encrypt(String text, String keypath) {
        byte[] cipherText = null;
        try {
            ObjectInputStream inputStream = null;
            inputStream = new ObjectInputStream(new FileInputStream(keypath));
            final PublicKey key = (PublicKey) inputStream.readObject();
            // get an RSA cipher object and print the provider
            final Cipher cipher = Cipher.getInstance(ALGORITHM);
            // encrypt the plain text using the public key
            cipher.init(Cipher.ENCRYPT_MODE, key);
            cipherText = cipher.doFinal(text.getBytes());
            System.out.println("original text : " + text);
            System.out.print("coded data : " + cipherText.toString());

        } catch (Exception e) {
            e.printStackTrace();
        }
        return cipherText;
    }

    /*decryption using RSA-2048 (note that it is an existing implementation and we have just borrowed the libraries so that the security protocols
    are universal and properly adhered to as mentioned in the lectures)*/
    public String decrypt(byte[] text, String keypath) {
        byte[] decryptedText = null;
        try {
            ObjectInputStream inputStream = null;
            inputStream = new ObjectInputStream(new FileInputStream(keypath));
            final PrivateKey key = (PrivateKey) inputStream.readObject();
            // get an RSA cipher object and print the provider
            final Cipher cipher = Cipher.getInstance(ALGORITHM);

            // decrypt the text using the private key
            cipher.init(Cipher.DECRYPT_MODE, key);
            decryptedText = cipher.doFinal(text);
            System.out.println("original data from another class: "+new String(decryptedText));

        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return new String(decryptedText);
    }


    public  String GenerateKeyFun(int myID) {
        System.out.println("User number is : "+myID);
        try {
            // Check if the public and private keys are present else generate those.
            if (!areKeysPresent()) {
                // Method generates a pair of keys using the RSA algorithm and stores them in their respective files
                String ret = generateKey(myID);
                return ret;
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
        return PRIVATE_KEY_FILE+" "+PUBLIC_KEY_FILE;
    }
}

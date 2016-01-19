/**
 * Created by Shantanu on 12/15/2015.
 */
import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import org.apache.commons.codec.binary.Base64;

/*encryption using AES-256 (note that it is an existing implementation and we have just borrowed the libraries so that the security protocols
    are universal and properly adhered to as mentioned in the lectures)*/
public class Encryptor {
    public String encrypt(String key1, String value) {
        try {
            IvParameterSpec iv = new IvParameterSpec(key1.getBytes("UTF-8"));
            SecretKeySpec skeySpec = new SecretKeySpec(key1.getBytes("UTF-8"),"AES");
            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
            cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv);
            byte[] encrypted = cipher.doFinal(value.getBytes());
            System.out.println("encrypted string:"
                    + Base64.encodeBase64String(encrypted));
            return Base64.encodeBase64String(encrypted);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return null;
    }

    /*decryption using AES-256 (note that it is an existing implementation and we have just borrowed the libraries so that the security
    protocols are universal and properly adhered to as mentioned in the lectures)*/
    public String decrypt(String key1, String encrypted) {
        try {
            IvParameterSpec iv = new IvParameterSpec(key1.getBytes("UTF-8"));
            SecretKeySpec skeySpec = new SecretKeySpec(key1.getBytes("UTF-8"),"AES");
            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
            cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv);
            byte[] original = cipher.doFinal(Base64.decodeBase64(encrypted));
            return new String(original);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return null;
    }
}

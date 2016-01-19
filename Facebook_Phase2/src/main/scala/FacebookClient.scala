/**
 * Created by Shantanu on 12/13/2015.
 */

import akka.actor._
import scala.collection.mutable._
import spray.http._
import spray.client.pipelining._
import scala.concurrent.duration._
import spray.http.MediaTypes
import org.json4s.native.{Serialization, Json}
import scala.util.Random
import spray.http.Uri
import java.security.MessageDigest
import org.apache.commons.codec.binary.Hex


case class Start(system : ActorSystem)
case class Welcome()
case class SignUp(i : Int)
case class GetUser()
case class WritePost()
case class PostPhotofun()
case class PostOnFrndWall()
case class GetUserprofile()
case class GetPage()
case class login(i : Int)
case class sendmsg(i : Int)
case class getmgs(i : Int)

object FacebookClient {

  def main(args: Array[String]) {
    val system = ActorSystem("ClientSystem")
    val client_actor =system.actorOf(Props(new ClientActor(system)),name="ClientActor")
    client_actor ! Start(system)
  }

  def stringToBytes(msg : String) : Array[Byte] = {
    Hex.decodeHex(msg.toCharArray)
  }

  def byteToString(bytes : Array[Byte]): String = {
    Hex.encodeHexString(bytes)
  }

  class ClientActor(system : ActorSystem) extends Actor
  {

    //to store all active actors of class User
    var clientBuffer= new ArrayBuffer[ActorRef]()

    def receive =
    {
      case Start(system) =>
      {
        //Number of client-actors simulated by client, it can be increased
        //val actorCount: Int = Runtime.getRuntime().availableProcessors()*250

        //val actorCount: Int = Runtime.getRuntime().availableProcessors()

        // we have reduced the number of client actors just for testing purposes
        val actorCount: Int = 2
        Thread.sleep(5000)
        for(i <-0 until actorCount)
        {
          //testing to get data in Json format from server
          clientBuffer += context.actorOf(Props(new Users(system,i)),name="User"+i)
          println("Actor "+ clientBuffer(i)+" started")
          //clientBuffer(i) ! Welcome()   uncomment it later
        }

        Thread.sleep(5000)
        for(i <- 0 until actorCount){
          //call registered new user and send all required information to server
         Thread.sleep(500)
          clientBuffer(i) ! SignUp(i)
        }


        Thread.sleep(20000)
        for(i <- 0 until actorCount) {
          //call registered new user and send all required information to server
          clientBuffer(i) ! login(i)
        }


        Thread.sleep(5000)
          //get data of perticular user, it will return profile of user
          clientBuffer(0) ! GetUser()


        /*Thread.sleep(5000)
        for(i <- 0 until actorCount){
          clientBuffer(i) ! WritePost()
        }*/

        Thread.sleep(5000)
        clientBuffer(0) ! sendmsg(1)


        Thread.sleep(2000)
        //call registered new user and send all required information to server
        clientBuffer(1) ! getmgs(0)
        /*Thread.sleep(5000)
        for(i <- 0 until actorCount){
          //simulated activity of user based on study available
          //this is post functionality
          var noOfPost = 0;
          val rnd = new scala.util.Random
          if(i%100 < 7){
            println("Not much active user of Facebook")
          }else if(i%100 >= 7 && i%100 < 35){
            //less active user
            val range = 1 to 1
            noOfPost = range(rnd.nextInt(range length))
            for(j <- 0 to noOfPost){
              clientBuffer(i) ! WritePost()
            }
          }else if(i%100 >= 35 && i%100 < 49){
            val range = 1 to 2
            noOfPost = range(rnd.nextInt(range length))
            for(j <- 0 to noOfPost){
              clientBuffer(i) ! WritePost()
            }
          }else if(i%100 >= 49 && i%100 < 75){
            val range = 1 to 6
            noOfPost = range(rnd.nextInt(range length))
            for(j <- 0 to noOfPost){
              clientBuffer(i) ! WritePost()
            }
          }else if(i%100 >= 75 && i%100 < 88){
            val range = 1 to 14
            //moderately active users
            noOfPost = range(rnd.nextInt(range length))
            for(j <- 0 to noOfPost){
              clientBuffer(i) ! WritePost()
            }
          }else{
            //very active users
            val range = 1 to 20
            noOfPost = range(rnd.nextInt(range length))
            for(j <- 0 to noOfPost){
              clientBuffer(i) ! WritePost()
            }
          }

        }

        Thread.sleep(5000)
        for(i <- 0 until actorCount){
          //post photo by user on his own wall
          clientBuffer(i) ! PostPhotofun()
        }

        Thread.sleep(5000)
        for(i <- 0 until actorCount){
          //post message on friends wall
          clientBuffer(i) ! PostOnFrndWall()
        }

        Thread.sleep(5000)
        for(i <- 0 until actorCount){
          //getting user's profile i.e, wall in json format
          clientBuffer(i) ! GetUserprofile()
        }

        Thread.sleep(5000)
        for(i <- 0 until actorCount){
          //get page information like title, about, subscribers, likes and all post
          clientBuffer(i) ! GetPage()
        }*/
      }
    }
  }

  class Users(system:ActorSystem, ID :Int) extends Actor {

    import system.dispatcher

    var publicpath : String = "not init"
    var privatepath : String = "not init"
    var myID: Int = ID
    val pipeline1 = sendReceive
    val pipeline2 = sendReceive
    val r = scala.util.Random
    val securePipeline = addCredentials(BasicHttpCredentials("adam", "1234")) ~> sendReceive
    val password = Random.alphanumeric.take(6).mkString
    val salt = Random.alphanumeric.take(6).mkString

    //get and post methods, sending request and getting data in json format
    def receive = {
      case Welcome() => {
        val result = securePipeline(Get("http://localhost:8080/hello"))
        result.foreach { response =>
          println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
        }
      }

      case login(i) => {
        println(salt+"   "+password)
        val sha = MessageDigest.getInstance("SHA-256")
        var hashvalue:String = sha.digest((password+""+salt).getBytes).foldLeft("")((s:String, b: Byte) => s + Character.forDigit((b & 0xf0) >> 4, 16) +Character.forDigit(b & 0x0f, 16))

        for(i <- 0 until 10){
          hashvalue = sha.digest((hashvalue).getBytes).foldLeft("")((s:String, b: Byte) => s + Character.forDigit((b & 0xf0) >> 4, 16) +Character.forDigit(b & 0x0f, 16))
        }
        println(hashvalue)

        val result  = pipeline1(Post("http://localhost:8080/login?userID="+i+"&hash="+hashvalue))

        result.foreach { response =>
          println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
        }
      }

      case GetUser() => {
        //var temp : Int = r.nextInt(39)

        //var temp : Int = r.nextInt(3)
        var temp : Int = 3
        println("information requested for : "+temp)
        val result = pipeline1(Post("http://localhost:8080/user?userID="+temp+"&reqBy="+myID))
        val s = new EncryptionUtil()

        //println(privatepath)
        result.foreach { response =>
          //println(s.decrypt(response.entity.asInstanceOf[Array[Byte]],"D:\\keysClient\\user0\\private.key"))

          /*var data : Array[String] = response.entity.asString.split("\\.")
          var len : Int = data.length
          println("size of array : "+data.length)
          var b = new Array[Byte](len-1)
          for(i <- 1 until len){
              b(i-1) = data(i).asInstanceOf[Byte]
          }*/

          var x : Array[Byte] = stringToBytes(response.entity.asString)
          println("testing : "+x.length)
          val re : String = s.decrypt(x,privatepath)
          println("decrypted data from server: "+re)
          //println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
          //val re : String = s.decrypt({response.entity.asString}.getBytes,"D:\\keysClient\\user0\\private.key")
          //println("Final data at client : "+re)
        }
      }

      case GetUserprofile() => {
      //var temp : Int = r.nextInt(39)

        var temp : Int = r.nextInt(3)
        val result  = pipeline1(Post("http://localhost:8080/profile?userID="+temp))
        result.foreach { response =>
          println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
        }
      }

      case GetPage() => {
        var temp : Int = r.nextInt(9)
        val result  = pipeline1(Post("http://localhost:8080/page?pageID="+temp))
        result.foreach { response =>
          println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
        }
      }

      case PostPhotofun() => {
        //var plentyOfAmber = Amber.ambers
        //var ID = r.nextInt(39)

        var temp : Int = r.nextInt(3)
        //var image : String = "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"
        var image : String = "dfasdfas"

        val result  = pipeline1(Post("http://localhost:8080/userphoto?userID="+ID+"&image="+image))
        result.foreach { response =>
          println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
        }
      }

      case WritePost() => {
        //var temp = r.nextInt(39)

        var temp : Int = r.nextInt(3)

        var AESkey: String = Random.alphanumeric.take(16).mkString
        var post: String = Random.alphanumeric.take(10).mkString

        println("Original post : "+post)

        var obj = new Encryptor()
        var encryptedData : String = obj.encrypt(AESkey, post)

        var decrypted_post : String = obj.decrypt(AESkey, encryptedData)
        println("decrypted data : "+decrypted_post)

        val result  = pipeline1(Post("http://localhost:8080/post?userID="+temp+"&posts="+post))
        result.foreach { response =>
          println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
        }
      }


      case sendmsg(i) => {
        //var temp = r.nextInt(39)

        var temp : Int = i

        var AESkey: String = Random.alphanumeric.take(16).mkString
        var mgs: String = "Hello World"

        println("Original post : "+mgs)

        var obj = new Encryptor()
        var obj1 = new EncryptionUtil()
        var encryptedData : String = obj.encrypt(AESkey, mgs)
        var rep : String = encryptedData.replaceAll("==","")

        println("after replacing : "+rep)
        var publickeyloc : String = null

        val result  = pipeline1(Post("http://localhost:8080/getpublic?userID="+(temp+4)))
        result.foreach { response =>
          publickeyloc = response.entity.asString
          println(publickeyloc)
          var enc_AES : Array[Byte] = obj1.encrypt(AESkey, publickeyloc)


          println("size of array : "+enc_AES.length)
          var encrypted_AES : String = byteToString(enc_AES)
          val result1  = pipeline1(Post ("http://localhost:8080/sendmgs?userID="+temp+"&byID="+myID+"&encryptedData="+rep+"&encrypted_AES="+encrypted_AES))
          result1.foreach { response =>
            println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
          }
        }
      }

      case getmgs(i) => {
        var temp : Int = i
        println("control received")
        val result = pipeline1(Post("http://localhost:8080/gettoken?UserID="+myID))
        result.foreach { response =>
          var token : String = response.entity.asString
          //println(token)
          var s = new EncryptionUtil()
          var decode : Array[Byte] = stringToBytes(token)
          var deString : String = s.decrypt(decode, privatepath)
          println("decoded string : "+deString)

          val result  = pipeline1(Post("http://localhost:8080/getmgs?forUser="+myID+"&byUser="+(temp+4)+"&token="+deString))
          result.foreach { response =>
            var data : Array[String] = response.entity.asString.split("\\.")

            var key : String = data(1)
            var keyd : Array[Byte] = stringToBytes(key)

            println(keyd.length)

            var s = new EncryptionUtil()
            var AES : String = s.decrypt(keyd,privatepath)

            var tempdata : String = data(0)+"=="
            var e = new Encryptor()
            var message : String = e.decrypt(AES,tempdata)

            println("Finally message is : "+message )

          }


        }


        /*result.foreach { response =>
          val result  = pipeline1(Post("http://localhost:8080/getmgs?forUser="+myID+"&byUser="+(temp+4)))
          result.foreach { response =>
            var data : Array[String] = response.entity.asString.split("\\.")

            var key : String = data(1)
            var keyd : Array[Byte] = stringToBytes(key)

            println(keyd.length)

            var s = new EncryptionUtil()
            var AES : String = s.decrypt(keyd,privatepath)

            var tempdata : String = data(0)+"=="
            var e = new Encryptor()
            var message : String = e.decrypt(AES,tempdata)

            println("Finally message is : "+message )

          }
        }*/
      }
      case PostOnFrndWall() => {
        //var temp = r.nextInt(39)

        var temp : Int = r.nextInt(3)
        var post: String = Random.alphanumeric.take(10).mkString
        val result  = pipeline1(Post("http://localhost:8080/postonfrnd?userID="+temp+"&posts="+post))
        result.foreach { response =>
          println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
        }
      }

      case SignUp(i) =>

        println(salt+"   "+password)
        val s = new EncryptionUtil()
        val keyslocation = s.GenerateKeyFun(i+4)
        val detail = keyslocation.split(" ")

        val sha = MessageDigest.getInstance("SHA-256")
        var hashvalue:String = sha.digest((password+""+salt).getBytes).foldLeft("")((s:String, b: Byte) => s + Character.forDigit((b & 0xf0) >> 4, 16) +Character.forDigit(b & 0x0f, 16))

        for(i <- 0 until 10){
          hashvalue = sha.digest((hashvalue).getBytes).foldLeft("")((s:String, b: Byte) => s + Character.forDigit((b & 0xf0) >> 4, 16) +Character.forDigit(b & 0x0f, 16))
        }

        println(hashvalue)

        privatepath = detail(0)
        publicpath = detail(1)

        var fname: String = Random.alphanumeric.take(6).mkString
        var lname: String = Random.alphanumeric.take(6).mkString
        var email: String = fname + "@facebook.com"
        var about: String = Random.alphanumeric.take(10).mkString
        val r = scala.util.Random
        var birthday: String = (r.nextInt(30)+1) + "-" + (r.nextInt(12)+1) + "-" + (r.nextInt(35) + 1965)
        var year : Int = 1990;
        val rnd = new scala.util.Random
        //simulator implemented according to statistics provided in the pdf file
        if(i%100 < 19){
          val range = 1991 to 1997
          year = range(rnd.nextInt(range length))
          birthday = (r.nextInt(30)+1) + "-" + (r.nextInt(12)+1) + "-" + year
        }else if(i%100 >= 19 && i%100 < 42){
          val range = 1981 to 1990
          year = range(rnd.nextInt(range length))
          birthday = (r.nextInt(30)+1) + "-" + (r.nextInt(12)+1) + "-" + year
        }else if(i%100 >= 42 && i%100 < 61){
          val range = 1971 to 1980
          year = range(rnd.nextInt(range length))
          birthday = (r.nextInt(30)+1) + "-" + (r.nextInt(12)+1) + "-" + year
        }else if(i%100 >= 61 && i%100 < 78){
          val range = 1961 to 1970
          year = range(rnd.nextInt(range length))
          birthday = (r.nextInt(30)+1) + "-" + (r.nextInt(12)+1) + "-" + year
        }else if(i%100 >= 78 && i%100 < 91){
          val range = 1951 to 1960
          year = range(rnd.nextInt(range length))
          birthday = (r.nextInt(30)+1) + "-" + (r.nextInt(12)+1) + "-" + year
        }else{
          val range = 1940 to 1950
          year = range(rnd.nextInt(range length))
          birthday = (r.nextInt(30)+1) + "-" + (r.nextInt(12)+1) + "-" + year
        }

        println(fname+" "+lname+" now going to register with Facebook.")
        val result  = pipeline1(Post("http://localhost:8080/register?fname="+fname+"&lname="+lname+"&email="+email+"&about="+about+"&bday="+birthday+"&publicKey="+detail(1)+"&hashvalue="+hashvalue))
        result.foreach { response =>
          println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
        }
    }
  }
}
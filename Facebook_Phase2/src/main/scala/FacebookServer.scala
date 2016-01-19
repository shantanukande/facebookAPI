/**
 * Created by Shantanu on 12/13/2015.
 */

import java.io.{FileInputStream, FileOutputStream, File}
import akka.actor.{Actor, ActorSystem, Props}
import spray.http.MediaTypes
import spray.routing._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import java.security.MessageDigest
import org.apache.commons.codec.binary.Hex
import scala.util.control.Breaks._


case class MasterGo(i : Int , actorCount : Int , realpost : String)
case class reverseAdd(myId : Int)
case class myposts(postid : Int, reakpost : post)
case class frndposts(postid : Int, realpost : post)
case class postingOnfrnd(myid: Int, realpost : post)
case class Likes(likeby : Int)
case class notifyfrnd(realpost: post)
case class Go(numberofFrnds : Int)
case class addpost(myID : Int,postID : Int,myID1 : Int ,post: String)
case class postPhoto(photoID : Int, photo : String)
case class notifyphoto(pic : photo)
case class notifyallphoto(pic : photo)
case class creatAlbum(globalAlbumID : Int, globalphotoID : Int , photoAb : ArrayBuffer[String])
case class notifyfrndalbum(tempalbum : album)
case class singupUser(currID : Int, fname : String, lname: String, email :String, about:  String, bday: String, numberofFrnds : Int)
case class postThePost(ID : Int, posts : String)
case class postThePhoto(ID : Int, image : String)
case class postTheFrndPost(ID : Int, image : String)
case class MastersingupUser (currID : Int, fname : String, lname: String, email :String, about:  String, bday: String, s : ArrayBuffer[String])
case class createPage(newamber : officialPage, totaluser : Int)
case class GoPage()
case class NotifyPostByPage(pageid : Int)
//extended to amber class to get data into json format
case class IndUser(UserID : Int, fname1 : String, lname1: String, email1 :String, about1:  String, bday1: String, mypost : ArrayBuffer[String], publicKey : String) extends Amber
case class postdata(postd : ArrayBuffer[String]) extends Amber
case class officialPage(pageID : Int, title : String, Pageinfo : String, subscribers : ArrayBuffer[Int], mypost : ArrayBuffer[String], likes : ArrayBuffer[Int]) extends Amber


object FacebookServer extends App with SimpleRoutingApp{
  override def main(args: Array[String]) {
    implicit val system = ActorSystem("facebook")

    //global arraylist to store UserID, also helpful to assign distinct userID to new user
    var alluserID = new ArrayBuffer[Int]()

    //bootstrapping clients
    val actorCount: Int = Runtime.getRuntime().availableProcessors()*1
    println("total number of actor : "+actorCount)

    var SuperString : String = null
    var token : String = null
    //getting list from amber class
    var plentyOfAmber = Amber.ambers
    var mypages = Amber.ambers
    var hashvalues = new ArrayBuffer[String]()


    lazy val master = system.actorOf(Props(new Facebook(actorCount)), name = "master")
    lazy val master1 = system.actorOf(Props(new FacebookPage()), name = "master1")
    val r = scala.util.Random

    //Generate user with randomly generated data
    GenerateUsers(actorCount)

    def byteToString(bytes : Array[Byte]): String = {
      Hex.encodeHexString(bytes)
    }

    def Generatepage(pagecount: Int) {
      for (i <- 0 until pagecount) {
        //randomly generrated page's infomation
        var title1 : String = Random.alphanumeric.take(6).mkString
        var about: String  = Random.alphanumeric.take(20).mkString
        var myposts1 = new  ArrayBuffer[String]
        var subscibers1 = new  ArrayBuffer[Int]
        var likes1 = new  ArrayBuffer[Int]
        val newAmber = officialPage(pageID = i, title = title1, Pageinfo = about, subscribers = subscibers1, mypost = myposts1, likes = likes1)
        mypages = newAmber :: mypages
        master1 ! createPage(newAmber, alluserID.length)
      }
    }


    def GenerateUsers(actorCount: Int) {
      for (i <- 0 until actorCount) {
        alluserID += i
        val s = new EncryptionUtil()
        val keyslocation = s.GenerateKeyFun(i)
        val detail = keyslocation.split(" ")
        val src = new File(detail(1))
        val loc : String = "D:/server/"+i+"public.key"
        val dest = new File(loc)
        new FileOutputStream(dest) getChannel() transferFrom(new FileInputStream(src) getChannel, 0, Long.MaxValue )
        val password = Random.alphanumeric.take(6).mkString
        val salt = Random.alphanumeric.take(6).mkString
        val sha = MessageDigest.getInstance("SHA-256")
        var hashvalueNew:String = sha.digest((password+""+salt).getBytes).foldLeft("")((s:String, b: Byte) => s + Character.forDigit((b & 0xf0) >> 4, 16) +Character.forDigit(b & 0x0f, 16))
        for(i <- 0 until 11){
          hashvalueNew = sha.digest((hashvalueNew).getBytes).foldLeft("")((s:String, b: Byte) => s + Character.forDigit((b & 0xf0) >> 4, 16) +Character.forDigit(b & 0x0f, 16))
        }
        hashvalues += hashvalueNew
        //randomly generated user's infomation
        var fname : String = Random.alphanumeric.take(6).mkString
        var lname: String  = Random.alphanumeric.take(6).mkString
        var email : String  = fname + "@facebook.com"
        var about : String = Random.alphanumeric.take(20).mkString
        val r = scala.util.Random
        var birthday: String =  (r.nextInt(30)+1) + "-" + (r.nextInt(12)+1) + "-" + (r.nextInt(35) + 1965)
        var myposts = new  ArrayBuffer[String]
        val newAmber = IndUser(UserID = i, fname1 = fname, lname1 = lname, email1 = email, about1 = about, bday1 = birthday, mypost = myposts, publicKey = loc)
        plentyOfAmber = newAmber :: plentyOfAmber
        var numberofFrnds : Int = r.nextInt(10)
        master ! singupUser (i,fname, lname, email, about, birthday,numberofFrnds)
      }
    }

    Thread.sleep(3000)

    //generating random 20 pages
    Generatepage(20)

    def authenticate(id : Int, hashval : String) : Boolean = {
      if(hashvalues(id).equals(hashval))
        return true
      else
        return false;
    }

    //this method handles request from client to register new users with system
    def register(fname : String, lname : String , email : String , about : String , bday:String, publicKey :String, hashvalueNew : String): Unit ={

      hashvalues += hashvalueNew
      var currID = alluserID.length
      println("total hasvalues : "+hashvalues.length)
      val src = new File(publicKey)
      val loc : String  = "D:/server/"+currID+"public.key"
      //println(loc)
      val dest = new File(loc)
      new FileOutputStream(dest) getChannel() transferFrom(new FileInputStream(src) getChannel, 0, Long.MaxValue )
      var numberofFrnds : Int = 0;

      //based on study, depending on activity level person can have different friends
      val rnd = new scala.util.Random
      if(currID%100 < 13){
        val range = 0 to 5
        numberofFrnds = range(rnd.nextInt(range length))
        // println(currID+" total frnds : " + numberofFrnds)
      }else if(currID%100 >= 13 && currID%100 < 47){
        val range = 6 to 25
        numberofFrnds = range(rnd.nextInt(range length))
        // println(currID+" total frnds : " + numberofFrnds)
      }else if(currID%100 >= 47 && currID%100 < 79){
        val range = 26 to 50
        numberofFrnds = range(rnd.nextInt(range length))
        // println(currID+" total frnds : " + numberofFrnds)
      }else if(currID%100 >= 79 && currID%100 < 92){
        val range = 51 to 100
        numberofFrnds = range(rnd.nextInt(range length))
        // println(currID+" total frnds : " + numberofFrnds)
      }else{
        val range = 101 to 500
        numberofFrnds = range(rnd.nextInt(range length))
        // println(currID+" total frnds : " + numberofFrnds)
      }
      alluserID += currID

      //storing post information in oject form
      var myposts = new ArrayBuffer[String]
      val newAmber = IndUser(UserID = currID, fname1 = fname, lname1 = lname, email1 = email, about1 = about, bday1 = bday, mypost = myposts, publicKey = loc)
      plentyOfAmber = newAmber :: plentyOfAmber
      master ! singupUser (currID, fname, lname, email, about, bday,numberofFrnds)
    }

    def getJson(route: Route) = get {
      respondWithMediaType(MediaTypes.`application/json`) { route }
    }

    def postOnWall(ID : Int, posts : String) : Unit ={
      //getting post from client and posting on his wall
      val s = plentyOfAmber(ID).asInstanceOf[IndUser]
      s.mypost += posts
      master ! postThePost(ID, posts)
    }

    def postOnFrndWall(ID : Int, posts : String) : Unit ={
      //posting
      val s = plentyOfAmber(ID).asInstanceOf[IndUser]
      s.mypost += posts
      master ! postThePost(ID, posts)
    }

    def postPhotoOnWall(ID : Int, image : String) : Unit ={
      //posting image on wall
      master ! postTheFrndPost(ID, image)
    }

    startServer(interface = "localhost", port = 8080) {
      get {
        path("hello") {
          complete{
            Amber.toJson(plentyOfAmber)
          }
        }
      }~
        post {
          path("profile") {
            parameters("userID".as[Int]) { (ID) =>
              println(ID+" information is requested by client.")
              var d =plentyOfAmber(plentyOfAmber.length - ID).asInstanceOf[IndUser]
              var postdata1 = d.mypost
              var plentyOfAmber1 = Amber.ambers
              var s = new postdata(postdata1)
              plentyOfAmber1 = s :: plentyOfAmber1
              complete {
                Amber.toJson(plentyOfAmber1)
              }
            }
          }
        }~
        post {
          path("gettoken") {
            parameters("UserID".as[Int]) { (ID) =>
              println(ID+" requested token.")
             token = Random.alphanumeric.take(10).mkString
              println("original token : "+token)
              var ret : String = null
              var len1 : Int = plentyOfAmber.length
              breakable {
                for(i <- 0 until len1) {
                  if (plentyOfAmber(i).asInstanceOf[IndUser].UserID == ID+actorCount) {
                    ret = plentyOfAmber(i).asInstanceOf[IndUser].publicKey
                    break
                  }
                }
              }

              println(ret)
              var e = new EncryptionUtil()
              var enc_token : Array[Byte] = e.encrypt(token, ret)

              var send : String = byteToString(enc_token)

              complete {
                send
              }
            }
          }
        }~
        post {
          path("login") {
            parameters("userID".as[Int],"hash".as[String]) { (ID,hash) =>
              println((plentyOfAmber.length - ID)+" is requested to authenticate.")
              val sha = MessageDigest.getInstance("SHA-256")
              var hashvalueNew:String = sha.digest((hash).getBytes).foldLeft("")((s:String, b: Byte) => s + Character.forDigit((b & 0xf0) >> 4, 16) +Character.forDigit(b & 0x0f, 16))
              println("new hash : "+hashvalueNew)
              var aut : Boolean = authenticate(ID+actorCount, hashvalueNew)
              complete {
                if(aut)
                Amber.toJson(plentyOfAmber(plentyOfAmber.length - ID - 1))
                else
                  "incorrect login"
              }
            }
          }
        }~
        post {
          path("page") {
            parameters("pageID".as[Int]) { (ID) =>
              println(ID+" page's information is requested by client.")
              complete {
                Amber.toJson(mypages(mypages.length - (ID+1)))
              }
            }
          }
        }~
        post {
          path("getpublic") {
            parameters("userID".as[Int]) { (ID) =>
              println(ID+"'s public key is requested by client.")

              var ret : String = null
              var len1 : Int = plentyOfAmber.length
              breakable {
                for(i <- 0 until len1) {
                  if (plentyOfAmber(i).asInstanceOf[IndUser].UserID == ID) {
                    ret = plentyOfAmber(i).asInstanceOf[IndUser].publicKey
                        break
                  }
                }
              }
              complete {
                ret
              }
            }
          }
        }~
        post {
          path("register") {
            parameters("fname".as[String], "lname".as[String], "email".as[String], "about".as[String], "bday".as[String], "publicKey".as[String],"hashvalue".as[String]) { (fname, lname, email, about, bday, publicKey, hashvalue) =>
              println(fname+" "+lname+" requested to register with system.")
              val sha = MessageDigest.getInstance("SHA-256")
              var hashvalueNew:String = sha.digest((hashvalue).getBytes).foldLeft("")((s:String, b: Byte) => s + Character.forDigit((b & 0xf0) >> 4, 16) +Character.forDigit(b & 0x0f, 16))
              println("hashvalue old : "+hashvalueNew)
              register(fname, lname, email, about, bday, publicKey,hashvalueNew)
              complete {
                "Hi you are "+fname+" registered with system."
              }
            }
          }
        }~
        post {
          path("sendmgs") {
            parameters("userID".as[Int], "byID".as[Int], "encryptedData".as[String], "encrypted_AES".as[String]) { (ToID, ID, data, key) =>
              println(ID+" has sent message to "+ToID)
              SuperString = data+"."+key
              complete {
                "Message is sent.."
              }
            }
          }
        }~
        post {
          path("getmgs") {
            parameters("forUser".as[Int],"byUser".as[Int],"token".as[String]) {(ToID, ID, token1) =>
              complete {
                if(token.equalsIgnoreCase(token1)){
                  SuperString
                }else{
                  "Not authenticated.."
                }

              }
            }
          }
        }~
        post {
          path("user") {
            parameters("userID".as[Int], "reqBy".as[Int]) { (ID, byID) =>
              println(ID+" information is requested by client.")
              val s = new EncryptionUtil();
              var ret : String = null
              var len1 : Int = plentyOfAmber.length
              breakable {
              for(i <- 0 until len1) {
                if (plentyOfAmber(i).asInstanceOf[IndUser].UserID == (byID+4)) {
                  ret = plentyOfAmber(i).asInstanceOf[IndUser].publicKey
                  break
                }
              }
              }
              println("return : "+ret)
              val d : Array[Byte] = s.encrypt(Amber.toJson(plentyOfAmber(plentyOfAmber.length - (ID+1))), ret)
              var t : String = byteToString(d)
              complete {
               t
              }
            }
          }
        }~
        post {
          path("post") {
            parameters("userID".as[Int],"posts".as[String]) { (ID,post) =>
              println(ID+" is posting "+post+" on his wall.")
              postOnWall(ID,post)
              complete {
                ID+" is posted on his on wall"
              }
            }
          }
        }~
        post {
          path("userphoto") {
            parameters("userID".as[Int],"image".as[String]) { (ID,image) =>
              println(ID+" has posted new photo.")
              postPhotoOnWall(ID,image)
              complete {
                ID+" is posted new photo on his on wall"
              }
            }
          }
        }~
        post {
          path("postonfrnd") {
            parameters("userID".as[Int],"posts".as[String]) { (ID,post) =>
              println(ID+" is posting "+post+" on his wall.")
              postOnFrndWall(ID,post)
              complete {
                ID+" is posted on his on wall"
              }
            }
          }
        }

    }
  }
}

class post(myID : Int, postID : Int,myID1 : Int, post1 : String) {
  var postedBy : Int  = myID
  var currpostID : Int = postID
  var postedOn : Int = myID1
  var likes = new ArrayBuffer[Int]()
  var post : String = post1
}


class photo(myID : Int, photoID : Int, photodata : String, tagged: ArrayBuffer[Int]) {
  var postedBy : Int  = myID
  var picID : Int = photoID
  var photo : String = photodata
  var likes = new ArrayBuffer[Int]()
  var tags : ArrayBuffer[Int]  = tagged
}

class album(myID : Int, albumID : Int, albumName : String, photoIDs : ArrayBuffer[Int]){
  var postedBy : Int  = myID
  var album : Int = albumID
  var name : String = albumName
  var photos : ArrayBuffer[Int]  = photoIDs
}


class FacebookPage() extends Actor {
  def receive = {
    case createPage(newamber, totaluser) => {
      context.actorOf(Props(new IndividualPage(newamber, totaluser)), name = "page"+String.valueOf(newamber.pageID)) ! GoPage
    }
  }
}

class IndividualPage(newamber : officialPage, totaluser : Int) extends Actor{
  val r = scala.util.Random
  def receive = {
    case GoPage =>
      var numsub = r.nextInt(totaluser)
      for(i <- 0 until numsub){
        newamber.subscribers += i
      }

      var numlike = r.nextInt(totaluser)
      for(i <- 0 until numlike){
        newamber.likes += i
      }

      for(i <- 0 until 5){
        var realpost: String = Random.alphanumeric.take(10).mkString
        newamber.mypost += realpost
        for(i <- 0 until numsub){
          context.system.actorSelection("/user/master/" + newamber.subscribers(i)) ! NotifyPostByPage(newamber.pageID)
        }
      }
  }
}

class Facebook(actorCount: Int) extends Actor {
  var postID: Int = 0
  //var allpost : Map[Int,String] = Map()
  var allpost = new ArrayBuffer[post]()
  var globalphotoID : Int = 0
  var globalAlbumID : Int = 0
  val r = scala.util.Random

  //Temp data
  var randomphoto = new ArrayBuffer[String]()
  //Base64 encoded pictures
  randomphoto += "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"
  randomphoto += "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"
  randomphoto += "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"
  randomphoto += "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"
  randomphoto += "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"
  randomphoto += "/9j/4AAQSkZJRgABAQEASABIAAD//gAXQ3JlYXRlZCB3aXRoIFRoZSBHSU1Q/9sAQwAIBgYHBgUIBwcHCQkICgwUDQwLCwwZEhMPFB0aHx4dGhwcICQuJyAiLCMcHCg3KSwwMTQ0NB8nOT04MjwuMzQy/9sAQwEJCQkMCwwYDQ0YMiEcITIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIy/8AAEQgAzwCnAwEiAAIRAQMRAf/EABwAAAICAwEBAAAAAAAAAAAAAAABAgQD\nBQYHCP/EADcQAAEEAQMCBQIEBAUFAAAAAAEAAgMRBAUhMRJBBhNRYXEikQcygaEUUrHBIyRC0eFDU2Ny8P/EABoBAAIDAQEAAAAAAAAAAAAAAAABAgMEBQb/xAAtEQACAgEEAQMBBwUAAAAAAAAAAQIRAwQSITETIkFRYQUycZGhsdEUQoHh8P/aAAwDAQACEQMRAD8A9hTQRuhIkLclMphB5SASaEBACJNqPupFJAAUk+yoz6lDj5kePJY6x+bsD2B/dJiLu6iSoefGPzOA9CTygyN6w3qFkbBAEikkHAkgchNIBWnewSKOyABK0WgoGF7JWhCAI3uhFIQBcSAF2mUBTEMJE7phI8pAIpE9LSSaATKi8Wwg+iAMc84ib1EjpHK43XvHsWmGRsLG2zb6uXH29h6q5r+qx4UJaXPcOekLxnXc52XmlznHgAfVf7qF26A6LI/FLXZXERPgjBPaO6RN+IWXmYr48nGiOQ78srCW0RVbfIXAFrhZN2PVDXSGyLJApTcUxWdrm+O8/KhfDJG2Nr+SCbHws+P431BrIi1rbidbXXfrsf8A7suIgnkB6SCWnkFWtxGS0P8A/UPKg4okj0bF/ESeNlyhvWQG9VGvcrq9F8aaXn1HLmRskPHX9IP3Xg5yxG4NkZI0jumMpzbId1sPqEKDQNo+nQ4Ob1NIIPcIXjfgrx1Lp+QzCz5XOwX7NL7JiPt7L2GGaPIibLE8PY4WCO6f0ZEmi0EoKBgkhCBiJ3QkhAi6kOE0BTEAQeUwonlIAOwXJeK/E7dOhfj4o83JA+oA7M+ff2XVSE+W6jW3K5DVsODSNKyc8tHmg9Yvcgk/1UZXXBJHkutapn5UzjkTOJPLboBa/A0+fVJ/KiiLnc2OyyZTXZOQ7cueTbifdeoeD9GiwdKYWtHmP+pzvUqjLl8ceOzRgw+SXPRw0fgjUZCOpoF91fi8BTsY7re2+1L1JuLYG26bsVvBG6xvUZX7m9abEvY8R1XQ8nT/AM0R6m7h3YhagSyiZobQB4FcL3TU9Jiy8R8cjA4FpC46HwxBPjRyOjb1EUdu4VkNXS9aKp6Pc/SzzrIBNiRg6hz/ALhYA1kLgATvvyuw1LQCzNyIAz6ugPj9wTwtDl6HPiACaMgGv0WrHnhLizJPTTXNFRjtx01vwvW/w31/+Kx36fPKDLH+QE7lv915JJjSYsnlvBobrceHNTGDqsDnUx7Xgtk7g/7HurG01aKWmuGfQiar4k4yYGyCqIvZZymQBBSCEDFaEIQBdR3QUBTEMKJolSSIQBAixRXNeNoXzaGY2D6S4F3wN/7Lp1rtbh8/RM2No+owvA+aKTXAWeCYsfm6n0N/L5m33Xs2lY4hw42+gXkegRGXX8aLm3W6/wB16Xk+IYsNvlwtDqNFxOy5+o5kdLS8Rs6QDbagUnOPoufw/GGnyPEc07I3ns41a3jMzGnaCyRhvuCs7XBqTTYPojcLUOxhG4RgUxq3NN5sUqsxi8zqaRYG6pkrLYujR5WJHPqcdtBDIiSfk7KlqWlx5eO5jwLrY0t3II2vc8GnO2tVpaEZOxCqbafBNJHmviLBEeJ5gH1tHQ5czE4Oe0HZwOxXc+K3DyukcOXB9XS8fzA3a7Gkk5Y+Tk61JZOD6E8K5DcrQcWVtfkDXUb3C3hXM+BYDD4WxiW9Jfbvn3XS1stEejC+wQUVtaRTAEJcFCYF4pd0JAqYie5JKieUBI8pAC1XiXKkwvDedPELkbEQ0e52/utrutZrUQycaPFN1LILr0G6hklti2WYoeSaieOeEYi/xZHd7Ndz8LvtRjwcXpg/g3ZGQ/iNo5KqaP4bkxfGGTk9NQMJ6TfNhdVkYDZZBICWyNNhw7LBOW52jo4se1UzyjOzIDqRwnaLG2QuLfpkBOx9l1OiYjxCDB5jW/yvN0r+T4bxjqjtRLA7Ked3Dv71wtpjwMxWUxnTe53Vc5J8Lovxwa5fZXzPOgxy7ruha4XU9W1FrnGKYRnsSV3OqvJx+m+Sufk02DIlY50dgcji1mUkp8mlwbicvianrkz+kZkMo/kLqP7rax5udjg+bG8HuCbBVbWfDsmXr7HwMijxNuQQW7AEbfB+6tY+Nl4+Q+F7nTYt/wCG535m+191flcKtUZsSndOzSeKJvMwmTNBAK4tn1S3dknld54th6NKHY9S4zT8Z+VmQwsFue8NC2aNrxWYddfko+h/D2K/E8P4MEjg57YhZC2u3T7rDjx+VjRR/wArA37BZOy0owkuAfdRRd7ITAX7oQQhAFxG1JFMKYDCR5QEFIAWu1MP6W9F9Qaemv0WwWLKh86AVfU02CFVni5QaRfppqGVNmr0yKOOFzv+q9/U++VsH1X6Ko6QNma3YyHY16BXAPpWCK9NHUk/VZSeByQq7vrP077rPkOokDlaxmoSxiUzY3ltafpf1WHKtlsfkp6m89VHgbqGH0vjo77rXZ+uQPkDeg811VsrenSemyySVOzV7G1ETdxQPyFhnga1peaPopueGCwVr8nLt3TdqL5KjnvE2mS6lFDFCQKfbr9Fq9P8MSYcb8wPaJIm9TCNwSuthb1B7ni7FALT4LJGx5GP1ExBrm89zwr4ZpqG1PgSwQlPe1yd/oE0mRoeJJIbcWblbJxAr3NKtpcAxtLx4h/pYFbXZx/cVnn8zTySr5FRCLTPKiplYjyhNCALaYRSKUxAAjupDhI1aAIlG9Jo7JDMbwOlx6RdcrC9309ICsOFghUnEhu/PCzZ0bdK+0VZGkk8LX5sXXCWBwsni1kycCKcOL3v6jwQ4ilz2dF/CuLGzv6vXlc6XR1scU32Sn0+OJzS1ork16rNAwNdtstOyHI6riy5Os82LtX4WZUH1TTNkafaiFmmvqXNNcF2Wa2kKi8AAvcaAF2sof1bgqL4f4uoRw4gEeqjFWyEnSK+JP54aZWdAd9cfWK6m9iPVbjA0aTIna98ZZBYcTVdS6eLHjigjjDG1GAGiuFm7Lqw0MVK2zlT+0pONRVMiKA24Qn+qRW45oiknXvskUwFdFCD3QgZdQEFMBTEMJHlMIKQCPOyXZNI8JARIVTIAa8ejuVZfKxgt7mtHqTSpOyMfKd/gzRyhuzixwNH0VOatho017ytNH6HlarK04yblw37ELY5MzsYG2lzPUC6+Vo36lHPkAGWuk2d1zMsaOximSGIGEg1x6KrPG42SR0jsjL13Hjk6GuB9KWvl1N07v8ADAIWVwdl+9Fl8giZQ3cVf0dnVqEDT/NZ/QWtRA0k9bjbitpp+diaflsyMuZkMLAQXvNAWKCliS8kV9SvNaxyf0Z2nZHoquLqWDnNvFzIJh/45AVav0XfR5v3\nDcDhJMkkAWolMYE2FFP4SQAvdCRQgC+i0kXspgSBQTuqmZqOJp8Jly8iOJnq48/A7ri9X/EeCK49\nNhMjv+5JsPsoOSXZZjwzn0jt8zMx8HGfkZErY42CySvMdY8d6vq2RPjeH4XMiiaXOkq3Ft1fsuZ1\nXxDqGqTdeZkvezszhrf0T8M6wNE1m5t8advlyH2Pf9FTkySUW4ou8Ci0pf5N5p/hbVtXkE2uahM4\nHfyWyE/crvdJ0/G0zGGPixNjjb2Hc+pWGKaOmmNzXNduHA8hbGIUFhjllkds3rFHGvSScA5vHPNr\nQ61pUcrHSMY0P9aW8c4cKlkPIa4O3apvoVcnCN0gma5NgOwWxjw4Y2igp5WWyWUsisAHdYw+uSsE\n3Jvk2wikuDI0BhO2yrajhR6hhSY0thj+aO6sNNpmyPjhVpuLtE5JSVM851LRc/QJRkY87jFe0jCW\nub80us8J/iPk48jMPWS6WLgT/wCpvz6hbXLx48zF8l7b6zVfG68y1GJsOpujj7HddfS6l5Vtl2jj\n6rTRxyTj0z6LxszHzIGzY8rJI3cOabWW1886Zredp8hfiZMkRB2AO32XcaX+JkzA1mo44kA5kjNH\n7LVvrsz/ANO2riemlIlaXTfFOk6qAIMprZDzHIel37rb9QIsFSTTKZRcXTQ0KJKEyJk1DVMPS4DN\nlztjb2B5PwFwWs/iLM8ui02Lym8eY8W4/p2XJ6pquVqeU6fKlL3n7AegWtO+97qqWRvo6uLSRjzL\nlmfM1HJzZTLkTPkcd7cbVNzj6pn3USN7ULNFETuOk8LHfSOiT8p4KylqRYC3pItNMrnj3crs3eg+\nJZtJmbj5Li/FOwPdnwvVdO1CHNxWSxPDgRyDyvCrDD5cn5ex9FsdL1zP0RwMDuuAndh4/wCFVPFz\nuiUxm1w+l+a/0e1PJuwsOQ9joiDXC5bS/GmJmMDHyCOQ8tk23+VtJtQHl/Ww9J4cBYKzOe10+C6K\n3crkoZkUTHl5c0Ko2ncLFkywyyiiTvwrOPA6QDpY4/AWXJ2a4PgTRXdWA0FqUkTcZpfkyMhaO8jg\n1aTUPGWn4LSzBYcyccGqYD/UqEYSyOoKxTzRh95m41OVukaLNnvY5zw3pYAOOraz6ei8mmlc90mQ\n825xNK/n6xn58ks2blP6ZaDowaaQDYFei17WunkDnCmA7Cl1NLg8MW5dv/qOdlm8sv2/klC0tjAW\nQA+qmG7fsgitu6vbsujClQMcRweFutN8UarpjgIMpxYNuh56m/G60oFdkdhyokmk1TPUdJ/EPFnb\n0ajEYXgfnYLaf07IXmTSb2Qpb2il6XG3Zfk5WK6WR7hdWsbqJ2UTaQv1T2tQJTvdIVmUAE8fKiW+\nyY43CRNIGY3sa+wRY91XMcsW8ZsfylXT2UDzfdNSaITxRlz7lF00bjUsZa71GysQ5+TjD/K6hPF7\nNkI/upvY121D9Qqz8WMm+lSuL7M0sMk/n9P2Ng3xHrTCOnVX7dyQT+6U3iPWphUuszgHs2Tp/otX\n/Bxna3fdAw4x2JPyo+PF8L8kQ2ZPj9WKbJbI7rnyJJnernWf3UBK920EYaPVWWY0TOGNtZQwA0pb\nkuiccMvfj8P5KjMSz1Snrd6FWG1Y2G37KZquFHYbqLk2XRgo9BY70opjjdI+6RIXAQK425Rsd+6l\nxSAJA36ISBAKEBZkyJCLo1SnC/zY77qtmO6bNgLLguuJxv8AVTr02RUvXRJxN0Am2yeNlAkHYd1n\nibe2yiTXLMoFDi1iP5uFld6LCSLG6RYyfbhQJoqQv17+qiePlICJs7bqJ39fdDqP9Er7XTUyLD0t\nKgD/AHTBs80kgQ+yRPyjnvsldmgUgFyFE77UpbCt1H9UyLD7pd/ZMDbY/uo8HlIYI+6XKHbAcWmh\nMASShR7CihSohZHUzUZo72rGIQ3CF9+yx6pF1Ru9lPGb/lGC+3qp/wBhWnWV/gTjHU7ZXWNDWgKM\nMHS0E91mLeyrZrgqRhkIBJJWK91le09RHoohh2PtfKQNhfokffZT6bUa2KBmE7n/AJQVMtu9ykWm\ntigizGbsWUwL2B+6Oj6tzv7oDSeO6CIEe6QGxNhSrte6On6e6KCzG4ikgRtSk9h+/um1m3KKFfJjJ23/AKqNWbCzFhrlHRvR/qihtmKiRSxyGhzSsmOwSq0zS0kWfumkQk+Bb+qFGjdWd0KdFVn/2Q=="
  randomphoto += "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"
  randomphoto += "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"
  randomphoto += "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"
  randomphoto += "/9j/4AAQSkZJRgABAQEASABIAAD//gAXQ3JlYXRlZCB3aXRoIFRoZSBHSU1Q/9sAQwAIBgYHBgUIBwcHCQkICgwUDQwLCwwZEhMPFB0aHx4dGhwcICQuJyAiLCMcHCg3KSwwMTQ0NB8nOT04MjwuMzQy/9sAQwEJCQkMCwwYDQ0YMiEcITIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIy/8AAEQgAzwCnAwEiAAIRAQMRAf/EABwAAAICAwEBAAAAAAAAAAAAAAABAgQD\nBQYHCP/EADcQAAEEAQMCBQIEBAUFAAAAAAEAAgMRBAUhMRJBBhNRYXEikQcygaEUUrHBIyRC0eFDU2Ny8P/EABoBAAIDAQEAAAAAAAAAAAAAAAABAgMEBQb/xAAtEQACAgEEAQMBBwUAAAAAAAAAAQIRAwQSITETIkFRYQUycZGhsdEUQoHh8P/aAAwDAQACEQMRAD8A9hTQRuhIkLclMphB5SASaEBACJNqPupFJAAUk+yoz6lDj5kePJY6x+bsD2B/dJiLu6iSoefGPzOA9CTygyN6w3qFkbBAEikkHAkgchNIBWnewSKOyABK0WgoGF7JWhCAI3uhFIQBcSAF2mUBTEMJE7phI8pAIpE9LSSaATKi8Wwg+iAMc84ib1EjpHK43XvHsWmGRsLG2zb6uXH29h6q5r+qx4UJaXPcOekLxnXc52XmlznHgAfVf7qF26A6LI/FLXZXERPgjBPaO6RN+IWXmYr48nGiOQ78srCW0RVbfIXAFrhZN2PVDXSGyLJApTcUxWdrm+O8/KhfDJG2Nr+SCbHws+P431BrIi1rbidbXXfrsf8A7suIgnkB6SCWnkFWtxGS0P8A/UPKg4okj0bF/ESeNlyhvWQG9VGvcrq9F8aaXn1HLmRskPHX9IP3Xg5yxG4NkZI0jumMpzbId1sPqEKDQNo+nQ4Ob1NIIPcIXjfgrx1Lp+QzCz5XOwX7NL7JiPt7L2GGaPIibLE8PY4WCO6f0ZEmi0EoKBgkhCBiJ3QkhAi6kOE0BTEAQeUwonlIAOwXJeK/E7dOhfj4o83JA+oA7M+ff2XVSE+W6jW3K5DVsODSNKyc8tHmg9Yvcgk/1UZXXBJHkutapn5UzjkTOJPLboBa/A0+fVJ/KiiLnc2OyyZTXZOQ7cueTbifdeoeD9GiwdKYWtHmP+pzvUqjLl8ceOzRgw+SXPRw0fgjUZCOpoF91fi8BTsY7re2+1L1JuLYG26bsVvBG6xvUZX7m9abEvY8R1XQ8nT/AM0R6m7h3YhagSyiZobQB4FcL3TU9Jiy8R8cjA4FpC46HwxBPjRyOjb1EUdu4VkNXS9aKp6Pc/SzzrIBNiRg6hz/ALhYA1kLgATvvyuw1LQCzNyIAz6ugPj9wTwtDl6HPiACaMgGv0WrHnhLizJPTTXNFRjtx01vwvW/w31/+Kx36fPKDLH+QE7lv915JJjSYsnlvBobrceHNTGDqsDnUx7Xgtk7g/7HurG01aKWmuGfQiar4k4yYGyCqIvZZymQBBSCEDFaEIQBdR3QUBTEMKJolSSIQBAixRXNeNoXzaGY2D6S4F3wN/7Lp1rtbh8/RM2No+owvA+aKTXAWeCYsfm6n0N/L5m33Xs2lY4hw42+gXkegRGXX8aLm3W6/wB16Xk+IYsNvlwtDqNFxOy5+o5kdLS8Rs6QDbagUnOPoufw/GGnyPEc07I3ns41a3jMzGnaCyRhvuCs7XBqTTYPojcLUOxhG4RgUxq3NN5sUqsxi8zqaRYG6pkrLYujR5WJHPqcdtBDIiSfk7KlqWlx5eO5jwLrY0t3II2vc8GnO2tVpaEZOxCqbafBNJHmviLBEeJ5gH1tHQ5czE4Oe0HZwOxXc+K3DyukcOXB9XS8fzA3a7Gkk5Y+Tk61JZOD6E8K5DcrQcWVtfkDXUb3C3hXM+BYDD4WxiW9Jfbvn3XS1stEejC+wQUVtaRTAEJcFCYF4pd0JAqYie5JKieUBI8pAC1XiXKkwvDedPELkbEQ0e52/utrutZrUQycaPFN1LILr0G6hklti2WYoeSaieOeEYi/xZHd7Ndz8LvtRjwcXpg/g3ZGQ/iNo5KqaP4bkxfGGTk9NQMJ6TfNhdVkYDZZBICWyNNhw7LBOW52jo4se1UzyjOzIDqRwnaLG2QuLfpkBOx9l1OiYjxCDB5jW/yvN0r+T4bxjqjtRLA7Ked3Dv71wtpjwMxWUxnTe53Vc5J8Lovxwa5fZXzPOgxy7ruha4XU9W1FrnGKYRnsSV3OqvJx+m+Sufk02DIlY50dgcji1mUkp8mlwbicvianrkz+kZkMo/kLqP7rax5udjg+bG8HuCbBVbWfDsmXr7HwMijxNuQQW7AEbfB+6tY+Nl4+Q+F7nTYt/wCG535m+191flcKtUZsSndOzSeKJvMwmTNBAK4tn1S3dknld54th6NKHY9S4zT8Z+VmQwsFue8NC2aNrxWYddfko+h/D2K/E8P4MEjg57YhZC2u3T7rDjx+VjRR/wArA37BZOy0owkuAfdRRd7ITAX7oQQhAFxG1JFMKYDCR5QEFIAWu1MP6W9F9Qaemv0WwWLKh86AVfU02CFVni5QaRfppqGVNmr0yKOOFzv+q9/U++VsH1X6Ko6QNma3YyHY16BXAPpWCK9NHUk/VZSeByQq7vrP077rPkOokDlaxmoSxiUzY3ltafpf1WHKtlsfkp6m89VHgbqGH0vjo77rXZ+uQPkDeg811VsrenSemyySVOzV7G1ETdxQPyFhnga1peaPopueGCwVr8nLt3TdqL5KjnvE2mS6lFDFCQKfbr9Fq9P8MSYcb8wPaJIm9TCNwSuthb1B7ni7FALT4LJGx5GP1ExBrm89zwr4ZpqG1PgSwQlPe1yd/oE0mRoeJJIbcWblbJxAr3NKtpcAxtLx4h/pYFbXZx/cVnn8zTySr5FRCLTPKiplYjyhNCALaYRSKUxAAjupDhI1aAIlG9Jo7JDMbwOlx6RdcrC9309ICsOFghUnEhu/PCzZ0bdK+0VZGkk8LX5sXXCWBwsni1kycCKcOL3v6jwQ4ilz2dF/CuLGzv6vXlc6XR1scU32Sn0+OJzS1ork16rNAwNdtstOyHI6riy5Os82LtX4WZUH1TTNkafaiFmmvqXNNcF2Wa2kKi8AAvcaAF2sof1bgqL4f4uoRw4gEeqjFWyEnSK+JP54aZWdAd9cfWK6m9iPVbjA0aTIna98ZZBYcTVdS6eLHjigjjDG1GAGiuFm7Lqw0MVK2zlT+0pONRVMiKA24Qn+qRW45oiknXvskUwFdFCD3QgZdQEFMBTEMJHlMIKQCPOyXZNI8JARIVTIAa8ejuVZfKxgt7mtHqTSpOyMfKd/gzRyhuzixwNH0VOatho017ytNH6HlarK04yblw37ELY5MzsYG2lzPUC6+Vo36lHPkAGWuk2d1zMsaOximSGIGEg1x6KrPG42SR0jsjL13Hjk6GuB9KWvl1N07v8ADAIWVwdl+9Fl8giZQ3cVf0dnVqEDT/NZ/QWtRA0k9bjbitpp+diaflsyMuZkMLAQXvNAWKCliS8kV9SvNaxyf0Z2nZHoquLqWDnNvFzIJh/45AVav0XfR5v3\nDcDhJMkkAWolMYE2FFP4SQAvdCRQgC+i0kXspgSBQTuqmZqOJp8Jly8iOJnq48/A7ri9X/EeCK49\nNhMjv+5JsPsoOSXZZjwzn0jt8zMx8HGfkZErY42CySvMdY8d6vq2RPjeH4XMiiaXOkq3Ft1fsuZ1\nXxDqGqTdeZkvezszhrf0T8M6wNE1m5t8advlyH2Pf9FTkySUW4ou8Ci0pf5N5p/hbVtXkE2uahM4\nHfyWyE/crvdJ0/G0zGGPixNjjb2Hc+pWGKaOmmNzXNduHA8hbGIUFhjllkds3rFHGvSScA5vHPNr\nQ61pUcrHSMY0P9aW8c4cKlkPIa4O3apvoVcnCN0gma5NgOwWxjw4Y2igp5WWyWUsisAHdYw+uSsE\n3Jvk2wikuDI0BhO2yrajhR6hhSY0thj+aO6sNNpmyPjhVpuLtE5JSVM851LRc/QJRkY87jFe0jCW\nub80us8J/iPk48jMPWS6WLgT/wCpvz6hbXLx48zF8l7b6zVfG68y1GJsOpujj7HddfS6l5Vtl2jj\n6rTRxyTj0z6LxszHzIGzY8rJI3cOabWW1886Zredp8hfiZMkRB2AO32XcaX+JkzA1mo44kA5kjNH\n7LVvrsz/ANO2riemlIlaXTfFOk6qAIMprZDzHIel37rb9QIsFSTTKZRcXTQ0KJKEyJk1DVMPS4DN\nlztjb2B5PwFwWs/iLM8ui02Lym8eY8W4/p2XJ6pquVqeU6fKlL3n7AegWtO+97qqWRvo6uLSRjzL\nlmfM1HJzZTLkTPkcd7cbVNzj6pn3USN7ULNFETuOk8LHfSOiT8p4KylqRYC3pItNMrnj3crs3eg+\nJZtJmbj5Li/FOwPdnwvVdO1CHNxWSxPDgRyDyvCrDD5cn5ex9FsdL1zP0RwMDuuAndh4/wCFVPFz\nuiUxm1w+l+a/0e1PJuwsOQ9joiDXC5bS/GmJmMDHyCOQ8tk23+VtJtQHl/Ww9J4cBYKzOe10+C6K\n3crkoZkUTHl5c0Ko2ncLFkywyyiiTvwrOPA6QDpY4/AWXJ2a4PgTRXdWA0FqUkTcZpfkyMhaO8jg\n1aTUPGWn4LSzBYcyccGqYD/UqEYSyOoKxTzRh95m41OVukaLNnvY5zw3pYAOOraz6ei8mmlc90mQ\n825xNK/n6xn58ks2blP6ZaDowaaQDYFei17WunkDnCmA7Cl1NLg8MW5dv/qOdlm8sv2/klC0tjAW\nQA+qmG7fsgitu6vbsujClQMcRweFutN8UarpjgIMpxYNuh56m/G60oFdkdhyokmk1TPUdJ/EPFnb\n0ajEYXgfnYLaf07IXmTSb2Qpb2il6XG3Zfk5WK6WR7hdWsbqJ2UTaQv1T2tQJTvdIVmUAE8fKiW+\nyY43CRNIGY3sa+wRY91XMcsW8ZsfylXT2UDzfdNSaITxRlz7lF00bjUsZa71GysQ5+TjD/K6hPF7\nNkI/upvY121D9Qqz8WMm+lSuL7M0sMk/n9P2Ng3xHrTCOnVX7dyQT+6U3iPWphUuszgHs2Tp/otX\n/Bxna3fdAw4x2JPyo+PF8L8kQ2ZPj9WKbJbI7rnyJJnernWf3UBK920EYaPVWWY0TOGNtZQwA0pb\nkuiccMvfj8P5KjMSz1Snrd6FWG1Y2G37KZquFHYbqLk2XRgo9BY70opjjdI+6RIXAQK425Rsd+6l\nxSAJA36ISBAKEBZkyJCLo1SnC/zY77qtmO6bNgLLguuJxv8AVTr02RUvXRJxN0Am2yeNlAkHYd1n\nibe2yiTXLMoFDi1iP5uFld6LCSLG6RYyfbhQJoqQv17+qiePlICJs7bqJ39fdDqP9Er7XTUyLD0t\nKgD/AHTBs80kgQ+yRPyjnvsldmgUgFyFE77UpbCt1H9UyLD7pd/ZMDbY/uo8HlIYI+6XKHbAcWmh\nMASShR7CihSohZHUzUZo72rGIQ3CF9+yx6pF1Ru9lPGb/lGC+3qp/wBhWnWV/gTjHU7ZXWNDWgKM\nMHS0E91mLeyrZrgqRhkIBJJWK91le09RHoohh2PtfKQNhfokffZT6bUa2KBmE7n/AJQVMtu9ykWm\ntigizGbsWUwL2B+6Oj6tzv7oDSeO6CIEe6QGxNhSrte6On6e6KCzG4ikgRtSk9h+/um1m3KKFfJjJ23/AKqNWbCzFhrlHRvR/qihtmKiRSxyGhzSsmOwSq0zS0kWfumkQk+Bb+qFGjdWd0KdFVn/2Q=="




  def receive = {
    case postThePhoto(currID, image) =>{
      var realphoto: String = image
      context.system.actorSelection("/user/master/" + currID) ! postPhoto(globalphotoID+1, image)
      globalphotoID += 1
    }

    case postThePost(currID, posts) =>{
      var realpost: String = posts
      val temp = new post(currID, allpost.length, currID, realpost)
      context.system.actorSelection("/user/master/" + currID) ! myposts(allpost.length, temp)
      allpost += temp
      postID += 1
    }

    case postTheFrndPost(currID, posts) =>{
      var realpost: String = posts
      val temp = new post(currID, allpost.length, currID, realpost)
      context.system.actorSelection("/user/master/" + currID) ! frndposts(allpost.length, temp)
      allpost += temp
      postID += 1
    }

    case singupUser(currID , fname, lname, email, about, bday, numberofFrnds) =>{
      println(fname+" "+lname+" "+email+" "+about+" "+bday)
      context.actorOf(Props(new User(currID ,fname, lname, email, about, bday,currID )), name = String.valueOf(currID)) ! Go(numberofFrnds)
    }

    case MastersingupUser(currID , fname, lname, email, about, bday, s) =>{
      println(fname+" "+lname+" "+email+" "+about+" "+bday)
      context.actorOf(Props(new User(currID ,fname, lname, email, about, bday,currID )), name = String.valueOf(currID)) ! Go
      var realpost1: String = Random.alphanumeric.take(2).mkString
      s += realpost1
      val temp = new post(currID, postID, currID, realpost1)
      allpost += temp
      context.system.actorSelection("/user/master/" + currID) ! myposts(postID, temp)
      postID += 1
    }


    case MasterGo(i, actorCount, realpost) =>
      println("Connection Begins...")
      var realpost1: String = realpost
      val temp = new post(i, postID, i, realpost1)
      allpost += temp
      context.system.actorSelection("/user/master/" + i) ! myposts(postID, temp)
      postID += 1


      Thread.sleep(100)

      for (j <- 0 until 1) {
        var realpost: String = Random.alphanumeric.take(3).mkString
        val temp = new post(i, postID, i, realpost)
        allpost += temp
        context.system.actorSelection("/user/master/" + i) ! frndposts(postID, temp)
        postID += 1
      }


      Thread.sleep(100)

      for (j <- 0 until 1) {
        var pd = r.nextInt(randomphoto.length)
        Thread.sleep(100)
        context.system.actorSelection("/user/master/" + i) ! postPhoto(globalphotoID, randomphoto(pd))
        globalphotoID += 1
      }


      Thread.sleep(100)

      for (j <- 0 until 1) {
        var photoAb = new ArrayBuffer[String]()
        var pd = r.nextInt(randomphoto.length)
        for (j <- 0 until pd-1) {
          photoAb += randomphoto(j)
          globalphotoID += 1
        }
        context.system.actorSelection("/user/master/" + i) ! creatAlbum(globalAlbumID, globalphotoID-1, photoAb)
        globalAlbumID
      }

  }
}
class User(UserID: Int, firstname : String, lastname : String, emailID : String, aboutMe : String, bday: String, totaluser : Int) extends Actor {


  val myID = UserID
  var fname: String = firstname
  var lname: String = lastname
  var email: String = emailID
  var about: String = aboutMe
  val r = scala.util.Random
  var birthday: String = bday
  var friendList = new ArrayBuffer[Int]()
  var mypost = new ArrayBuffer[post]()
  var frndpost = new ArrayBuffer[post]()
  var Allphotos = new ArrayBuffer[photo]()
  var AlbumArray = new ArrayBuffer[album]()


  def receive = {
    case Go(numberofFrnds) =>
      var noOfFrnds = numberofFrnds
      for (i <- 1 to noOfFrnds) {
        if(totaluser!=0){
          var fd = r.nextInt(totaluser)
          if (fd != myID && (!friendList.contains(fd))) {
            friendList += fd
            Thread.sleep(100)
            context.system.actorSelection("/user/master/" + fd) ! reverseAdd(myID)
          }
        }
      }

    case reverseAdd(myID1) =>
      if ((!friendList.contains(myID1))) {
        friendList += myID1
        //println(myID + " " + friendList)
      }

    case myposts(postID, realpost) =>
      mypost += realpost
      var go = friendList.length
      for (i <- 1 to go) {
        context.system.actorSelection("/user/master/" + i) ! notifyfrnd(realpost)
      }

    //println("posted by :"+myID+" postID : "+postID+" post is  "+post)

    case frndposts(postID, temp) =>
      try{
        mypost += temp
        var totalfrnd = friendList.length
        var fd1 = r.nextInt(totalfrnd)
        var fd = friendList(fd1)
        temp.postedOn = fd
        context.system.actorSelection("/user/master/" + fd) ! postingOnfrnd(myID, temp)
      }catch{
        case e: Exception =>
          println("Add friends in your list")
      }


    case postingOnfrnd(frndid, temp) =>
      //frndpost += (postID -> post)
      frndpost += temp
      println("notification :" + frndid + " has posted " + temp.post + " on " + myID + " wall")

    case notifyfrnd(realpost) =>
      println("Hey " + myID + ", Your friend " + realpost.postedBy + " has posted " + realpost.post + " on his wall")
      val lk = r.nextInt(2)
      if (lk == 1) {
        realpost.likes += myID
        println(myID + " has liked " + realpost.post + " and total likes are :" + realpost.likes.length)
      }

    case NotifyPostByPage(pageid) =>
      println("Hey " + myID + ", the page you subscribed " + pageid + " has posted.")

    case postPhoto(globalphotoID, randomphoto) =>
      try{
        var totalfrnd = friendList.length
        var fd1 = r.nextInt(totalfrnd)
        var tag = new ArrayBuffer[Int]()
        for(i <- 1 to fd1){
          tag += friendList(i)
        }
        val temp = new photo(myID, globalphotoID, randomphoto, tag)
        Allphotos += temp
        //println("Hey " + myID + ", you have posted new photo on your wall")

        Thread.sleep(100)
        var tagfd = tag.length
        for(j <- 0 to fd1-1){
          context.system.actorSelection("/user/master/" + tag(j)) ! notifyphoto(temp)
        }

        Thread.sleep(100)
        for(k <- 0 to totalfrnd-1){
          context.system.actorSelection("/user/master/" + friendList(k)) ! notifyallphoto(temp)
        }
      }catch{
        case e: Exception =>
          println("Add friends in your list")
      }


    case notifyphoto(pic) =>
      println("Hey " + myID + ", Your friend " + pic.postedBy + " has tagged you in "+pic.picID+" photo")

    case notifyallphoto (pic) =>
      println("Hey " + myID + ", Your friend " + pic.postedBy + " has posted new photo "+pic.picID)
      val lk = r.nextInt(2)
      if (lk == 1) {
        pic.likes += myID
        println(myID + " has liked picture with ID " + pic.picID + " and total likes are : " + pic.likes.length)
      }

    case creatAlbum(globalAlbumID, globalphotoID, photoAb) =>
      var totalpic = photoAb.length
      var startID = globalphotoID-totalpic
      var tag = new ArrayBuffer[Int]()
      var picsinAlb = new ArrayBuffer[Int]()
      for(k <- 0 to totalpic-1){
        val temp = new photo(myID, startID, photoAb(k), tag)
        picsinAlb += startID
        startID += 1
        Allphotos += temp
      }
      var albumNane : String = Random.alphanumeric.take(6).mkString

      val tempalbum = new album(myID, globalAlbumID, albumNane, picsinAlb)
      AlbumArray += tempalbum
      var go = friendList.length
      for (i <- 1 to go) {
        context.system.actorSelection("/user/master/" + i) ! notifyfrndalbum(tempalbum)
      }

    case notifyfrndalbum(tempalbum) =>
      println("Hey "+myID + ", your frnd "+tempalbum.postedBy+" has posted " + tempalbum.photos.length + " photos in :" + tempalbum.name+" album")
  }
}


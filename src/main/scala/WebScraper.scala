import java.io.{File, PrintWriter}
import scala.collection.JavaConverters._
import org.jsoup.Jsoup

object WebScraper extends App {
  //Container to hold each marvel record
  case class MarvelRecord(name:String="",
                          iden:String="",
                          align:String="",
                          eye:String="",
                          hair:String="",
                          sex:String="",
                          alive:String="",
                          appearrences:String="",
                          timeline:String){
    def toCSV:String = {
      s"$name,$iden,$align,$eye,$hair,$sex,$alive,$appearrences,$timeline\n"
    }
  }

  val marvelList = List[MarvelRecord]()

  val site = scala.io.Source.fromURL("http://marvel-ironman.surge.sh")
  //done it for testing purpose
  //val site = scala.io.Source.fromFile("d://docs//marvel.html")
  val d =  Jsoup.parse(site.mkString)
  // println(d.select("p:contains(Identity)").eachText())
  // val e = d.select("p").eachText().asScala

  val marvelRecList = d.select(".col-lg-8")
    .eachText.asScala
    .map(transformLine)
    //.map(printRecords) // test output to console
    .map(toMarvelRecord)
    .toList

  def transformLine(rec:String):String = {
    var str = rec
    List("Identity :","Align :","Eye :", "Hair :","Sex :","Alive :","Appearances :","First Appearance : ","Year : ").foreach(s =>{
      str = str.replace(s,"|")
    }
    )
    str
  }

  def printRecords(rec:String):String = {
    println(rec)
    rec
  }

  // Transform each line into MarvelRecord
  def toMarvelRecord(rec:String):MarvelRecord = {

    //split the record and replace 'None' with 'not-available'
    val values = rec.split('|')
      .map(_.trim)
      .map( s => if(s.equalsIgnoreCase("None"))  "not-available" else s)
    //Marvel(colleen wing (earth-616) ,public identity ,good characters ,blue eyes ,auburn hair ,living characters ,175.0 ,nov-74 ,1974.0,)
    val firstapp = values(8)
    val year = values(9)
    var time=""
    if(!"not-available".equalsIgnoreCase(firstapp) || !"not-available".equalsIgnoreCase(year)) {
     val timeline = year.dropRight(2) + "-" + monthToNumeric(firstapp.split("-")(0)) + "-01"
      time = timeline
    }else {
      time = "not-available"
    }

    val m = MarvelRecord(values(0),values(1),values(2),values(3),values(4),values(5),values(6),values(7),time)
    m
  }

  //Write the marvel records as csv (data.csv)
  val writer = new PrintWriter(new File("data.csv"))
  val dataHeader="name,identity,align,eye,hair,sex,alive,appearances,timeline"
  writer.write(dataHeader+"\n")
  val writeRecordAsCsv: MarvelRecord => Unit = rec => writer.write(rec.toCSV)
  marvelRecList.map(writeRecordAsCsv)
  writer.close

  val nameswriter = new PrintWriter(new File("names.csv"))
  val writeNamesToFile : String => Unit = s => nameswriter.write(s+"\n")
  nameswriter.close

  //Write the names in ascending order for the given condition
  marvelRecList.filter(rec =>  (rec.timeline > "1975-01-01" && rec.timeline < "2015-08-01"))
    .map(_.name)
    .sortWith(_ < _)
    .map(writeNamesToFile)

  //Data Analysis
  //Q1
  println(s"Q1 : ${marvelRecList.map(_.timeline).distinct.size}") //677
  //Q2
  println(s"Q2 : ${marvelRecList.filter(rec => (rec.timeline == "1963-03-01" && rec.iden == "public identity")).map(_.name)}") //List(ho yinsen (earth-616), carl zante (earth-616))
  //Q3
  println(s"Q3  : ${marvelRecList.filter(_.eye == "blue eyes").size}") //265
  //Q4
  val goodLivingSecretFilter : MarvelRecord => Boolean = rec => (rec.align == "good characters" && rec.alive == "living characters" && rec.iden == "secret identity")
  println(s"Q4  : ${marvelRecList.filter(goodLivingSecretFilter).size}") //172
  //Q5
  val goodcharc = marvelRecList.filter(_.align== "good characters").size
  val badcharc = marvelRecList.filter(_.align== "bad characters").size

  val gap_bet_good_bad = goodcharc - badcharc
  println(s"Q5 : $gap_bet_good_bad") //-285

  //Q6
  println(s"Q6 : ${marvelRecList.filter(_.timeline.split("-")(1) == "10").size}") //205

  //Q7
  println(s"Q7 : ${marvelRecList.filter(_.timeline > "1970-03-01").size}") //1549

  //Q8
  val appSorted = marvelRecList
    .filter(rec => rec.align == "bad characters" && rec.alive == "living characters")
    .map(_.appearrences)
    .map(_.toDouble)
    .sortWith(_ < _)
  //println(appSorted.size)
  val(high,low) = appSorted.splitAt(appSorted.size/2)
  val median = (high.last+low.head)/2
  println(s"Q8 :$median") //3.0

  //Q9
  println(s"Q9 :${marvelRecList.filter(_.sex == "female characters").size}") //480
  println(s"Q9 :${marvelRecList.filter(_.sex == "male characters").size}") //1423

  //Q10
  println(s"Q10 :${marvelRecList.filterNot(_.name.contains("earth-616")).size}") //7

  def monthToNumeric(s:String)= {
    s match {
      case "jan" => "01"
      case "feb" => "02"
      case "mar" => "03"
      case "apr" => "04"
      case "may" => "05"
      case "jun" => "06"
      case "jul" => "07"
      case "aug" => "08"
      case "sep" => "09"
      case "oct" => "10"
      case "nov" => "11"
      case "dec" => "12"
      case _  =>
    }
  }

}

package music

import music.Music._
import music.Song

import scala.io.Source

object Music {


  // TODO: Implement required methods as defined in the HW Doc



  /**
   * You may use this helper method to read a file and return a List of Strings containing the lines of the file
   *
   * @param filename The file to be read
   * @return The lines of the file as a List of Strings with 1 String per line
   */
  def filenameToListOfLines(filename: String): List[String] = {
    val file = Source.fromFile(filename)
    val lines: List[String] = file.getLines().toList
    file.close()
    lines
  }

  // TODO: Uncomment after implementing the required methods
  // Uses your methods to read all the data in a file with song ratings
//    def readSongsFromFile(filename: String): List[Song] = {
//      val songsWithDuplicates: List[Song] = readSongsFromFileWithoutDuplicates(filename)
//      val songMap: Map[String, Song] = songListToMap(songsWithDuplicates)
//      songMap.values.toList
//    }

  // Can be used to test your application objective
  def songIncubator(songs: List[Song]): List[Double] => Song = {
    genes: List[Double] => {
      val geneSong: Int = (genes.head.abs * songs.length).toInt % songs.length
      songs(geneSong)
    }
  }

  // Can be used to test your application objective
  def playlistIncubator(songs: List[Song]): List[Double] => Playlist = {
    genes: List[Double] => {
      val incubatorForSongs: List[Double] => Song = songIncubator(songs)
      new Playlist(genes.map((gene: Double) => incubatorForSongs(List(gene))))
    }
  }

  def songCostFunction(surating: Map[String, Int]): Song => Double ={

    (ratedsong: Song) =>{
      val spUserrating = surating.getOrElse(ratedsong.youtubeId, 3)
      if (spUserrating == 1 || spUserrating ==2){
        1000.0
      }
      else {
        val baysianaver = ratedsong.bayesianRating(2,3)
        val cost: Double = 1.0/(baysianaver * spUserrating)
        cost
      }

    }


  }


//  def filenameToListOfLines(filename: String): List[String] = {
//    val file = Source.fromFile(filename)
//    val lines: List[String] = file.getLines().toList
//    file.close()
//    lines
//  }

  def recuring(a: List[String]): Map[String, Int] = {
    if (a == List()){
      Map()
    }
    else {
      val list1: String = a(0)
      val youtubeId: String = list1.split(",")(0)
      val rating: Int = list1.split(",")(3).toInt
      val resong: List[String] = a.drop(1)
      val n = recuring(resong)
      val finalst = Map(youtubeId -> rating) ++ n
      finalst
    }

  }



  def readUserRatingsFromFile(songRatingfilename: String ) : Map[String,Int] = {
    val filedata: List[String] = filenameToListOfLines(songRatingfilename)
    recuring(filedata)
  }



  def recuring1(a: List[String]): List[Song] = {
    if (a == List()){
      List()
    }
    else {
      val list1: List[String] = a(0).split(",").toList
      val title: String = list1(2)
      val youtubeId: String = list1(0)
      val rating: Int = list1(3).toInt
      val artists: String = list1(1)
      val songrating: SongRating = new SongRating(list1(3).toInt, list1(4).toInt)
      val newSong: Song = new Song(title, artists,youtubeId, List(songrating))
      val resong: List[String] = a.drop(1)
      val n = recuring1(resong)
      val finalst = List(newSong)::: n
      finalst
    }

  }
  def readSongsFromFileWithoutDuplicates(songRatingfilename: String): List[Song] = {
    val filedata: List[String] = filenameToListOfLines(songRatingfilename)
    recuring1(filedata)

  }





  def recuring2(songLst: List[Song], mapSong: Map[String, Song]): Map[String, Song]={
    if (songLst == List()){
      mapSong
    }
    else {
      val song1: Song = songLst(0)

      val youtubeId: String = song1.youtubeId
      val songMap: Map[String, Song] =
        if (mapSong.contains(youtubeId)){
        mapSong + (youtubeId -> mapSong(youtubeId).addMultipleRatings(song1.ratings))
      }
      else {
        mapSong + (youtubeId -> song1)
      }
      recuring2(songLst.drop(1), songMap)
    }

  }



  def songListToMap(songLst: List[Song]): Map[String, Song] = {
    recuring2(songLst, Map())


  }









}







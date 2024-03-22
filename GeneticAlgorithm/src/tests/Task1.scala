package tests

import music.{Music, Song, SongRating}
import org.scalatest._
import statistics.Statistics

class Task1 extends FunSuite {

  def compareDoubles(d1: Double, d2: Double): Boolean = {
    Math.abs(d1 - d2) < 0.001
  }


  test("your test") {

    //def average[T](data: List[T], f: T => Double): Double

    val list1: List[Int] = List(2, 4, 6)  // this is our DATA parameter
    val func: Int => Double = _.toDouble  // This is our 'f' param

    // example of using "_" the underscore
    // this function, func2, returns 2 * the input Integer
    val func2: Int => Int = {
      2 * _
    }

    val output: Double = Statistics.average[Int](list1, func)
    val expected: Double = 4

    assert(compareDoubles(output, expected))
  }

  test("Song Rating"){

    val song1 = new SongRating(5, 10)
    val song2 = new SongRating(2, 8)
    val DragosSong = new SongRating(1, 1)
    val ReesSong = new SongRating(4, 20)
    val Happier = new SongRating(3,7)

    val songs: List[SongRating] = List(song1, song2, DragosSong, ReesSong, Happier)

    //def average[T](data: List[T], f: T => Double): Double

    val output: Double  = Statistics.average[SongRating](songs, (avgSong: SongRating) => avgSong.energyLevel)
    val expected: Double = 9.2

    assert(compareDoubles(output, expected))
    //10+8+1+20+7 / 5
  }

  test("length test"){

    val strLis: List[String] = List("Shazzad", "James", "Max", "Bagdagul", "Hema")
    val output: Double = Statistics.average[String](strLis, (f1: String) => f1.length)
    val expected: Double = 5.4
    assert(compareDoubles(output, expected))
  }

  //topK
  //def topK[T](data: List[T], f: T => Double, k: Int): List[T]

  test("topK test 1"){

    val lis0: List[Int] = List(1,2,3)

    val output: List[Int] = Statistics.topK(lis0, (f1: Int) => f1, 3)
    val expected: List[Int] = List(3, 2, 1)

    assert(expected == output)

  }

  test("topK test 2"){

    val lis1: List[Int] = List(1,2,3)

    val output: List[Int] = Statistics.topK(lis1, (f1: Int) => f1, 10)
    val expected: List[Int] = List(3, 2, 1)

    assert(expected == output)
  }


  test("duplicates"){

    val dupe: List[String] = List("Feburaray", "December", "Feburaray", "March", "May", "E", "", "April")

    val output: List[String] = Statistics.topK(dupe, (f1: String) => f1.length, 2)
    val expected: List[String] = List("Feburaray", "Feburaray")

    assert(expected == output)

  }

  test("here is my first test"){

    val songrating1 = new SongRating(3,3)
    val song1 = new Song("Thunder", "Imagine Dragons", "fKopy74weus", List(songrating1))

    val user1 : Map[String, Int]= Map("fKopy74weus" -> 1)
    val user2 : Map[String, Int]= Map("fKopy74weus" -> 2)

    assert(compareDoubles(Music.songCostFunction(user1)(song1), 1000.0))
    assert(compareDoubles(Music.songCostFunction(user2)(song1), 1000.0))

  }

  test("here is my second test"){
    val songrating2 = new SongRating(5,5)
    val song2 = new Song("Thunderstruck", "AC/DC", "v2AC41dglnM", List(songrating2))

    val user1 : Map[String, Int]= Map("v2AC41dglnM" -> 3)
    val user2 : Map[String, Int]= Map("v2AC41dglnM" -> 4)
    val user3 : Map[String, Int]= Map("v2AC41dglnM" -> 5)

    println(Music.songCostFunction(user1)(song2))
    println(Music.songCostFunction(user2)(song2))
    println(Music.songCostFunction(user3)(song2))
    assert(compareDoubles(Music.songCostFunction(user1)(song2), 0.090909))
    assert(compareDoubles(Music.songCostFunction(user2)(song2), 0.0681818))
    assert(compareDoubles(Music.songCostFunction(user3)(song2), 0.0545454))
  }

  test("here is my third test"){
    val songrating3 = new SongRating(2,3)
    val song3 = new Song("Wellerman", "The Longest Johns", "FXfZPbeJqyw", List(songrating3))

    val user1: Map[String, Int] = Map("abcd" -> 5)

    println(Music.songCostFunction(user1)(song3))
    assert(compareDoubles(Music.songCostFunction(user1)(song3),0.125))
  }




}

package tests

import music.{Music, Song, SongRating}
import org.scalatest.FunSuite

class Task3 extends FunSuite{

  def compareDoubles(d1: Double, d2: Double): Boolean = {
    Math.abs(d1 - d2) < 0.001
  }

  val rating1 = new SongRating(1,1)
  val rating2 = new SongRating(2,2)
  val rating3 = new SongRating(3,3)
  val rating4 = new SongRating(4,4)
  val rating5 = new SongRating(5,5)

  val rating6 = new SongRating(2, 3)

  val song1 = new Song("weapon", "Against The Current", "W6FixoUyQQQ", List(rating1))
  val song2 = new Song("weapon", "Against The Current", "W6FixoUyQQQ", List(rating2))
  val song3 = new Song("weapon", "Against The Current", "W6FixoUyQQQ", List(rating3))
  val song4 = new Song("weapon", "Against The Current", "abcd", List(rating4))
  val song5 = new Song("weapon", "Against The Current", "abcdef", List(rating5))

  val song6 = new Song("weapon", "Against The Current", "W6FixoUyQQQ", List(rating6))

  test("1"){

    val maptoLst = List(song1, song1, song2, song3, song4,song3, song5, song6,song3,song3,song3)

    //println(music.Music.songListToMap(maptoLst).keys)

    assert(Music.songListToMap(maptoLst).keys==Set("W6FixoUyQQQ", "abcd", "abcdef"))
    //println(maptoLst.size)
    assert(Music.songListToMap(maptoLst).size == 3)

    println(Music.songListToMap(maptoLst)("W6FixoUyQQQ").averageEnergyRating())

    assert(compareDoubles(Music.songListToMap(maptoLst)("W6FixoUyQQQ").averageRating(),2.3333))
    assert(compareDoubles(Music.songListToMap(maptoLst)("W6FixoUyQQQ").averageEnergyRating(),2.4444))








    val multiRating: List[SongRating] = List(new SongRating(4,4), new SongRating(3,3))

    song1.addMultipleRatings(multiRating)

  }


  test("2"){

    val newRating = song1.addRating(new SongRating(5,5))
    assert(newRating.youtubeId == "W6FixoUyQQQ")
    println(newRating.averageEnergyRating())
    assert(newRating.title == "weapon")
    assert(newRating.artist == "Against The Current")
    assert(newRating.ratings.length == 2)
    assert(compareDoubles(newRating.averageRating(),3.0))
    assert(compareDoubles(newRating.averageEnergyRating(),3.0))
  }

  test("3"){
    val newRating = song1.addMultipleRatings(List(new SongRating(5,5),new SongRating(3,4), new SongRating(1,2), new SongRating(0,0)))
    assert(newRating.youtubeId == "W6FixoUyQQQ")
    println(newRating.averageEnergyRating())
    assert(newRating.title == "weapon")
    assert(newRating.artist == "Against The Current")
    assert(newRating.ratings.length == 5)
    assert(compareDoubles(newRating.averageRating(),2.0))
    assert(compareDoubles(newRating.averageEnergyRating(),2.4))

  }

}

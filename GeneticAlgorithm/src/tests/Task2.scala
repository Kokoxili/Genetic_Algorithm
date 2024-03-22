package tests

import music.Music
import org.scalatest.FunSuite

class Task2 extends FunSuite{

//  println(Music.readUserRatingsFromFile("data/song_ratings_2021.csv"))
  println(Music.readSongsFromFileWithoutDuplicates("data/song_ratings_2021.csv"))


}

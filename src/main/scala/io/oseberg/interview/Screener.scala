package io.oseberg.interview

import scala.io.Source
import scala.collection.immutable.ListMap

object Screener extends App {

  val filename = "./resources/DoI.txt"
  val wordToFind = "People"

  def LineCount(file: String) {
    val file_name = Source.fromFile(file)
    val count_lines = file_name.getLines.size
    println("Line Count: " + count_lines)
  }

  def WordCount(file: String) {
    val file_name = Source.fromFile(file).getLines.flatMap(_.split("([a-z.,-:;'\"])")).filter(!_.isEmpty()).toList
    var wordcount = 0

    for (word <- file_name) //loop through all words and increase counter
      wordcount += 1
    println("Total Word Count: " + wordcount)
  }

  def WordFind(file: String, wordToFind: String) {
    val file_name = Source.fromFile(file).getLines.flatMap(_.split("([.,\" \"])+")).filter(!_.isEmpty()).toList
    var count = 0

    for (word <- file_name) //loop through all words, increase counter when the word matches word to find
      if (word.toLowerCase == wordToFind.toLowerCase)
        count += 1
    println("Count for: " + wordToFind + ": " + count)
  }

  def top3WordsByFreq(file: String) {
    val filename = Source.fromFile(file).getLines.flatMap(_.split("([.,\" \"])+")).filter(!_.isEmpty()).toList
    //separate file into words
    val Map = filename.map(word => (word, 1)).groupBy(_._1).map(word => (word._1, word._2.foldLeft(0)((sum, c) => sum + c._2)))
    //count and group by each word. Order by count, limit the results to 3
    val top3words = ListMap(Map.map(item => item.swap).toSeq.sortWith(_._1 > _._1): _*).take(3).values.toList //swap (k,v), sort by the count; take 3 most common words
    println("Top Three Words by Freq: " + top3words)
  }

  println("Stats for file:" + filename)
  LineCount(filename)
  WordCount(filename)
  WordFind(filename, wordToFind)
  top3WordsByFreq(filename)
}
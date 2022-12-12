package example

import scala.collection.immutable.{ListMap, SortedMap}

object HelloWorld extends App {

  val mapdata = Map(
    "SK1" -> List(9, 7, 2, 0, 7, 3, 7, 9, 1, 2, 8, 1, 9, 6, 5, 3, 2, 2, 7, 2, 8, 5, 4, 5, 1, 6, 5, 2, 4, 1),
    "SK2" -> List(0, 7, 6, 3, 3, 3, 1, 6, 9, 2, 9, 7, 8, 7, 3, 6, 3, 5, 5, 2, 9, 7, 3, 4, 6, 3, 4, 3, 4, 1),
    "SK3" -> List(8, 7, 1, 8, 0, 5, 8, 3, 5, 9, 7, 5, 4, 7, 9, 8, 1, 4, 6, 5, 6, 6, 3, 6, 8, 8, 7, 4, 0, 6),
    "SK4" -> List(2, 9, 5, 7, 0, 8, 6, 6, 7, 9, 0, 1, 3, 1, 6, 0, 0, 1, 3, 8, 5, 4, 0, 9, 7, 1, 4, 5, 2, 9),
    "SK5" -> List(2, 6, 8, 0, 3, 5, 5, 2, 5, 9, 4, 5, 3, 5, 7, 8, 8, 2, 5, 9, 3, 8, 6, 7, 8, 7, 4, 1, 2, 3),
    "SK6" -> List(2, 7, 5, 9, 1, 9, 8, 4, 1, 7, 3, 7, 0, 8, 4, 5, 9, 2, 4, 4, 8, 7, 9, 2, 2, 7, 9, 1, 6, 9),
    "SK7" -> List(6, 9, 5, 0, 0, 0, 0, 5, 8, 3, 8, 7, 1, 9, 6, 1, 5, 3, 4, 7, 9, 5, 5, 9, 1, 4, 4, 0, 2, 0),
    "SK8" -> List(2, 8, 8, 3, 1, 1, 0, 8, 5, 9, 0, 3, 1, 6, 8, 7, 9, 6, 7, 7, 0, 9, 5, 2, 5, 0, 2, 1, 8, 6),
    "SK9" -> List(7, 1, 8, 8, 4, 4, 2, 2, 7, 4, 0, 6, 9, 5, 5, 4, 9, 1, 8, 6, 3, 4, 8, 2, 7, 9, 7, 2, 6, 6)
  )


  def currentStockPrice(lmap: Map[String, List[Int]]) {
    val mp = ListMap(lmap.toSeq.sortWith(_._1 < _._1): _*)
    mp foreach { case (key, value) => println("The current stock price of " + key + " is : " + value.last) }
  }

  currentStockPrice(mapdata)

  def maxMinPrices(lmap: Map[String, List[Int]]) {
    val mp = ListMap(lmap.toSeq.sortWith(_._1 < _._1): _*)
    mp foreach { case (key, value) => println(key + " : " + "Highest price : " + value.max + " Lowest Price : " + value.min) }
  }

  maxMinPrices(mapdata)

  def findMedian(lmap: Map[String, List[Int]]): Unit = {
    val mp = ListMap(lmap.toSeq.sortWith(_._1 < _._1): _*)
    mp foreach {
      case (key, value)
      => println(key + " => " + value.sorted.drop(value.length / 2).head)
        if (value.sorted.size % 2 == 0) {
          println(value.sorted)
          val (up, down) = value.sorted.splitAt(value.size / 2)
          println(((up.last + down.head) / 2).toDouble)
        }
    }
  }

  findMedian(mapdata)

  def getSymbolForWeek(lmap: Map[String, List[Int]]): Unit = {
    val mp = ListMap(lmap.toSeq.sortWith(_._1 < _._1): _*)
    var xy = SortedMap[String, Int]()
    mp foreach {
      case (key, value)
      => val week = value.drop(23)
        val rateChange = (week.last - week.head)
        xy = SortedMap(key -> rateChange) ++ xy
    }
    println("The stock that has shown largest rise over the last week is : " + (xy.maxBy(stock => stock._2)))
  }

  getSymbolForWeek(mapdata)

  def getUsersChoice(lmap : Map[String, List[Int]]) = {
    println("Please select two stocks from the following options :")
    var x : Boolean = true
    var optNum = 0
    val mp = ListMap(lmap.toSeq.sortWith(_._1 < _._1): _*)
    mp foreach { case (key, value)
    =>
      optNum += 1
      println(optNum + ". " + key)
    }
    println("Select the first stock : ")
    val firstStock = scala.io.StdIn.readLine().toUpperCase()
    println("Second stock :")
    val secStock = scala.io.StdIn.readLine().toUpperCase()
    println("You have chosen options " + firstStock + " and " + secStock)
    println("The average price for " + firstStock + " is : " + getAverage(mp(firstStock)))
    println("The average price for " + secStock + " is : " + getAverage(mp(secStock)))
    var larger,smaller = ""
    if(firstStock > secStock){
      larger = firstStock
      smaller = secStock }
    else{
      larger = secStock
      smaller = firstStock
    }
    var timesHigher = getAverage(mp(larger)) / getAverage(mp(smaller))
    timesHigher = scala.BigDecimal(timesHigher).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    println("This means that on average the value of " + larger + " is " + timesHigher + " times greater than " + smaller )
  }

  def getAverage(list : List[Int]):Double = {
    var average = list.foldLeft(0.0)(_+_) / list.foldLeft(0)((r,c)=>r+1)
    average = scala.BigDecimal(average).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    return average
  }

  getUsersChoice(mapdata)

  // Let the user input their own portfolio.
  def createPortfolio() = {
    var xy = SortedMap[String,Int]()
  }

}

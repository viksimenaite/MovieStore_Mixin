package Mixin

import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.Calendar.DAY_OF_MONTH
import java.util.Date


object Main{
  def main(args: Array[String]): Unit = {
    val movieStore = new LowPriceFamilyMovieStore
    val movieRental = new LowPriceFamilyLongPeriodMovieRental
    val client = Client("John", "Smith", "john2000", "123", new SimpleDateFormat("dd/MM/yyyy").parse("01/04/2019"), 20, new SimpleDateFormat("dd/MM/yyyy").parse("09/02/2000"))
    val movie = Movie("Up", new SimpleDateFormat("dd/MM/yyyy").parse("01/01/2009"), 1200, PG(), 10.1)
    //Console.println(movieStore.estimate(client, movie))
    Console.println(movieRental.estimateRentalPeriod(client,movie))
  }
}



// enum
sealed trait MPAARating
case class G() extends MPAARating
case class PG() extends MPAARating
case class PG_13() extends MPAARating
case class R() extends MPAARating
case class NC_17() extends MPAARating



case class Client(name:String, surname:String, userName:String, password:String, registrationDate:Date, totalNoOfOrders:Int, dateOfBirth:Date)
case class Movie(name:String, releaseDate:Date, totalNoOfPurchases:Int, ageRating:MPAARating, basePrice:Double)



//"Strategy", abstraction, variation
trait MoviePricing{
  def determinePrice(movie:Movie):Double
  def getDiscount(client:Client, movie:Movie):Double
  def countFees(movie:Movie):Double
}

trait MovieAvailability{
  def isAppropriateAge(client:Client, movie:Movie):Boolean
  def isLegal(movie:Movie):Boolean
  def isAlreadyInTheMarket(movie:Movie):Boolean
}

trait MovieRentalPeriod{
  def determineBaseRentalPeriod(movie:Movie):Calendar
  def determineBonusOfRentalPeriod(client:Client, movie:Movie):Int
  def determineReductionOfRentalPeriod(movie:Movie):Int
}




//Concrete implementations

trait FamilyFriendlyMovieAvailability extends MovieAvailability{
  override def isAppropriateAge(client:Client, movie:Movie):Boolean = movie.ageRating match{
    case G() => true
    case PG() => true
    case _ => false
  }
  override def isLegal(movie:Movie):Boolean = true
  override def isAlreadyInTheMarket(movie:Movie):Boolean = {
    val today = Calendar.getInstance()
    val yearFormat = new SimpleDateFormat("yyyy")
    val currentYear = today.get(Calendar.YEAR)
    val releaseYear = Integer.parseInt(yearFormat.format(movie.releaseDate))
    if((currentYear - releaseYear) > 1){
      true
    }else{
      false
    }
  }
}

trait AdultsMovieAvailability extends MovieAvailability{
  override def isAppropriateAge(client:Client, movie:Movie):Boolean = {
    val today = Calendar.getInstance()
    val yearFormat = new SimpleDateFormat("yyyy")
    val monthFormat = new SimpleDateFormat("MM")
    val dayFormat = new SimpleDateFormat("dd")
    val currentYear = today.get(Calendar.YEAR)
    val currentMonth = today.get(Calendar.MONTH)
    val currentDay = today.get(DAY_OF_MONTH)
    val birthYear = Integer.parseInt(yearFormat.format(client.dateOfBirth))
    val birthMonth = Integer.parseInt(monthFormat.format(client.dateOfBirth))
    val birthDay = Integer.parseInt(dayFormat.format(client.dateOfBirth))

    var age = currentYear-birthYear
    if(currentMonth<birthMonth || currentMonth==birthMonth && currentDay<birthDay){
      (age-1)>18
    }else{
      age>18
    }
  }
  override def isLegal(movie:Movie):Boolean = true
  override def isAlreadyInTheMarket(movie:Movie):Boolean = {
      val today = Calendar.getInstance()
      val yearFormat = new SimpleDateFormat("yyyy")
      val currentYear = today.get(Calendar.YEAR)
      val releaseYear = Integer.parseInt(yearFormat.format(movie.releaseDate))
      if(currentYear >= releaseYear){
        true
      }else{
        false
      }
  }
}

trait LuxuryMovieStorePricing extends MoviePricing {
  override def determinePrice(movie:Movie):Double = if(movie.totalNoOfPurchases>1500){
    movie.basePrice + 12.5
  }else{
    movie.basePrice + 8.5
  }
  override def getDiscount(client:Client, movie:Movie):Double = if(client.totalNoOfOrders>30){
    0.10
  }else {
    0
  }
  override def countFees(movie:Movie):Double = movie.basePrice * 0.21
}

trait LowPriceMovieStorePricing extends MoviePricing {
  override def determinePrice(movie:Movie):Double = {
    val today = Calendar.getInstance()
    val yearFormat = new SimpleDateFormat("yyyy")
    val currentYear = today.get(Calendar.YEAR)
    val releaseYear = Integer.parseInt(yearFormat.format(movie.releaseDate))

    if(currentYear - releaseYear < 2  ){
      movie.basePrice + 4.5
    }else{
      movie.basePrice
    }
  }
  override def getDiscount(client:Client, movie:Movie):Double = if(client.totalNoOfOrders>30){
    0.20
  }else if(movie.totalNoOfPurchases < 1000) {
    0.10
  }else{
    0
}
  override def countFees(movie:Movie):Double = movie.basePrice * 0.21
}

trait LimitedEditionMovieRentalPeriod extends MovieRentalPeriod{
  override def determineBaseRentalPeriod(movie:Movie):Calendar=Calendar.getInstance()

  override def determineBonusOfRentalPeriod(client:Client, movie:Movie):Int={
    if(client.totalNoOfOrders>30){
      2
    }else {
      0
    }
  }
  override def determineReductionOfRentalPeriod(movie:Movie):Int={
    if(movie.totalNoOfPurchases>1500){
      2
    }else{
      0
    }
  }
}

trait LongPeriodEditionMovieRentalPeriod extends MovieRentalPeriod{
 /* override def determineBaseRentalPeriod(movie:Movie):Date={
    val returnDate = Calendar.getInstance
    returnDate.add(Calendar.MONTH, 1)
    val dateFormat = new SimpleDateFormat("dd/MM/yyyy")
    dateFormat.parse(returnDate.toString)
  }*/
  override def determineBaseRentalPeriod(movie:Movie):Calendar=Calendar.getInstance()

  override def determineBonusOfRentalPeriod(client:Client, movie:Movie):Int={
    if(client.totalNoOfOrders>20){
      7
    }else {
      0
    }
  }
  override def determineReductionOfRentalPeriod(movie:Movie):Int = 0
}





//Main entity, template
abstract class MovieStore extends MoviePricing with MovieAvailability{
  def estimate(client:Client, movie:Movie):Double = {
    if(isAppropriateAge(client, movie) && isLegal(movie) && isAlreadyInTheMarket(movie)){
      (determinePrice(movie) + countFees(movie)) * (1 - getDiscount(client, movie))
    }else{
      -1
    }
  }
}

abstract class MovieRental extends MoviePricing with MovieRentalPeriod with MovieAvailability{

  def estimatePrice(client:Client, movie:Movie):Double = {
    if(isAppropriateAge(client, movie) && isLegal(movie) && isAlreadyInTheMarket(movie)){
      (determinePrice(movie) + countFees(movie)) * (1 - getDiscount(client, movie))
    }else{
      -1
    }
  }
  def estimateRentalPeriod(client:Client, movie:Movie):Date={
    val rentalPeriod = Calendar.getInstance
    rentalPeriod.add(DAY_OF_MONTH, determineBonusOfRentalPeriod(client, movie))
    rentalPeriod.add(DAY_OF_MONTH, (-1)*determineReductionOfRentalPeriod(movie))
    val dateFormat = new SimpleDateFormat("dd/MM/yyyy")
    dateFormat.parse(rentalPeriod.toString)
  }
}



class LuxuryAdultsMovieStore extends MovieStore with LuxuryMovieStorePricing with AdultsMovieAvailability{}
class LowPriceFamilyMovieStore extends MovieStore with LowPriceMovieStorePricing with FamilyFriendlyMovieAvailability{}
class LuxuryAdultsLimitedEditionMovieRental extends MovieRental with LuxuryMovieStorePricing with LimitedEditionMovieRentalPeriod with AdultsMovieAvailability{}
class LowPriceFamilyLongPeriodMovieRental extends MovieRental with LowPriceMovieStorePricing with LongPeriodEditionMovieRentalPeriod with FamilyFriendlyMovieAvailability{}
package Mixin

import java.time.LocalDate


object Main{
  def main(args: Array[String]): Unit = {
    val movieStore =  new LowPriceFamilyMovieStore
    val movieRental = new LowPriceFamilyLongPeriodMovieRental
    val client = Client("John", "Smith", "john2000", "123", 2019, 20, "2000-02-09")
    val movie = Movie("Up", 2009, 1200, PG(), 10.1)

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



case class Client(name:String, surname:String, userName:String, password:String, registrationYear:Int, totalNoOfOrders:Int, dateOfBirth:String)
case class Movie(name:String, releaseYear:Int, totalNoOfPurchases:Int, ageRating:MPAARating, basePrice:Double)



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
  def determineBaseRentalPeriod(movie:Movie):LocalDate
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
    val currentYear = LocalDate.now.getYear
    if((currentYear - movie.releaseYear) > 1){
      true
    }else{
      false
    }
  }
}

trait AdultsMovieAvailability extends MovieAvailability {
  override def isAppropriateAge(client: Client, movie: Movie): Boolean = {
    val dtf = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val birthDate = java.time.LocalDate.parse(client.dateOfBirth, dtf)

    var age = LocalDate.now.getYear - birthDate.getYear
    if (LocalDate.now.getMonth.getValue < birthDate.getMonth.getValue || (LocalDate.now.getMonth.getValue == birthDate.getMonth.getValue && LocalDate.now.getDayOfMonth < birthDate.getDayOfMonth)) {
      age = age - 1
    }
    age > 18
  }
  override def isLegal(movie: Movie): Boolean = true
  override def isAlreadyInTheMarket(movie: Movie): Boolean = LocalDate.now.getYear >= movie.releaseYear
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
    if(LocalDate.now.getYear - movie.releaseYear < 2  ){
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
  override def determineBaseRentalPeriod(movie:Movie):LocalDate = LocalDate.now.plusDays(7)

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

trait LongPeriodMovieRentalPeriod extends MovieRentalPeriod{
  override def determineBaseRentalPeriod(movie:Movie):LocalDate = LocalDate.now.plusDays(21)

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
  def estimateRentalPeriod(client:Client, movie:Movie):LocalDate= {
    val rentalPeriod = determineBaseRentalPeriod(movie).plusDays(determineBonusOfRentalPeriod(client, movie)).minusDays(determineReductionOfRentalPeriod(movie))
    if (rentalPeriod.isAfter(LocalDate.now)) {
       rentalPeriod
    }else{
      LocalDate.MIN
    }
  }
}



class LuxuryAdultsMovieStore extends MovieStore with LuxuryMovieStorePricing with AdultsMovieAvailability{}
class LowPriceFamilyMovieStore extends MovieStore with LowPriceMovieStorePricing with FamilyFriendlyMovieAvailability{}
class LuxuryAdultsLimitedEditionMovieRental extends MovieRental with LuxuryMovieStorePricing with LimitedEditionMovieRentalPeriod with AdultsMovieAvailability{}
class LowPriceFamilyLongPeriodMovieRental extends MovieRental with LowPriceMovieStorePricing with LongPeriodMovieRentalPeriod with FamilyFriendlyMovieAvailability{}
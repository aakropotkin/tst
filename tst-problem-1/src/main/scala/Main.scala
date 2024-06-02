/* Types */

case class Rate( rateCode: String, rateGroup: String )

case class CabinPrice( cabinCode: String, rateCode: String, price: BigDecimal )

case class BestGroupPrice( cabinCode: String,
                           rateCode:  String,
                           price:     BigDecimal,
                           rateGroup: String
                         )


/* Helpers */

object RateUtils:
  def getBestGroupPrices( rates:  Seq[Rate],
                          prices: Seq[CabinPrice]
                        ): Seq[BestGroupPrice] =

    /* Maps Rate Codes to Rate Groups */
    val rateGroups: Map[String, String] =
      rates.groupMapReduce( rate => rate.rateCode )
                          ( rate => rate.rateGroup )
                          ( ( acc, x ) => x )

    /* Maps Rate Codes to "Best Cabin Prices" */
    val bests: Map[String, CabinPrice] =
      prices.groupMapReduce( cabinPrice => cabinPrice.rateCode )( x => x )(
        ( best, cabinPrice ) => if cabinPrice.price < best.price
                                then cabinPrice
                                else best
      )

    /* Covert CabinPrices to BestCabinPrices by looking up Rate Groups. */
    bests.values.map( cabinPrice =>
      BestGroupPrice( cabinPrice.cabinCode,
                      cabinPrice.rateCode,
                      cabinPrice.price,
                      rateGroups( cabinPrice.rateCode )
                    )
    ).toSeq


/* Entry */

@main def problem1(): Unit =
  var rates = Seq(
    Rate( "M1", "Military" ),
    Rate( "M2", "Military" ),
    Rate( "S1", "Senior" ),
    Rate( "S2", "Senior" )
  )
  var prices = Seq(
    CabinPrice( "CA", "M1", 200.00 ),
    CabinPrice( "CA", "M2", 250.00 ),
    CabinPrice( "CA", "S1", 225.00 ),
    CabinPrice( "CA", "S2", 260.00 ),
    CabinPrice( "CB", "M1", 230.00 ),
    CabinPrice( "CB", "M2", 260.00 ),
    CabinPrice( "CB", "S1", 245.00 ),
    CabinPrice( "CB", "S2", 270.00 )
  )
  var bestGroupPrices = RateUtils.getBestGroupPrices( rates, prices )
  bestGroupPrices.foreach( println )

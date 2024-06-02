/**
 * @file Main.scala
 *
 * @brief Find the best prices for a rate group in a set of cabin prices.
 */

/* ========================================================================== */

/* Types */

/**
 * @brief A short code referring to a _Rate Group_.
 * Example codes are "M1", "M2", "S1", or "S2".
 */
type RateCode = String;

/**
 * @brief A human readable name for a _Rate Group_.
 * Example group names are "Senior" or "Military".
 */
type RateGroup = String;

/**
 * @brief A short code used to uniquely identify cabins.
 * Example codes are "CA" or "CB".
 */
type CabinCode = String;

/**
 * @brief Indicates the cost of a cabin.
 * The Currency is unspecified or irrelevant.
 */
type Price = BigDecimal;

/** @brief A mapping from a _rate_'s _code name_ to its human readable name. */
case class Rate( rateCode: RateCode, rateGroup: RateGroup );

/** @brief The price of a cabin at a given rate. */
case class CabinPrice( cabinCode: CabinCode,
                       rateCode:  RateCode,
                       price:     Price
                     );

/** @brief The lowest available price for a cabin in a _rate group_. */
case class BestGroupPrice( cabinCode: CabinCode,
                           rateCode:  RateCode,
                           price:     Price,
                           rateGroup: RateGroup
                         );


/* -------------------------------------------------------------------------- */

/** @brief Find the best prices for a rate group in a set of cabin prices. */
def getBestGroupPrices( rates:  Seq[Rate],
                        prices: Seq[CabinPrice]
                      ): Seq[BestGroupPrice] =
{
  /* Convert list of `Rate's to a lookup table. */
  val rateCodesToGroups: Map[RateCode, RateGroup] =
    rates.groupMapReduce( rate => rate.rateCode )( rate => rate.rateGroup )(
      /* Ensure each `RateCode' is defined exactly once. */
      ( acc, x ) => assert( false )
    );

  /* Group prices by rates collect the lowest price. */
  val bests: Map[RateCode, CabinPrice] =
    prices.groupMapReduce( cabinPrice => cabinPrice.rateCode )( x => x )(
      ( best, cabinPrice ) => if cabinPrice.price < best.price
                              then cabinPrice
                              else best
    );

  /* Covert `CabinPrice's to `BestCabinPrice's by looking up the human
   * readable name for its `RateCode'. */
  return bests.values.map( cabinPrice =>
    BestGroupPrice( cabinPrice.cabinCode,
                    cabinPrice.rateCode,
                    cabinPrice.price,
                    rateCodesToGroups( cabinPrice.rateCode )
                  )
  ).toSeq;
}


/* -------------------------------------------------------------------------- */

/** @brief Program entry. */
@main def problem1(): Unit =
{
  var rates = Seq( Rate( "M1", "Military" ),
                   Rate( "M2", "Military" ),
                   Rate( "S1", "Senior" ),
                   Rate( "S2", "Senior" )
                 );
  var prices = Seq( CabinPrice( "CA", "M1", 200.00 ),
                    CabinPrice( "CA", "M2", 250.00 ),
                    CabinPrice( "CA", "S1", 225.00 ),
                    CabinPrice( "CA", "S2", 260.00 ),
                    CabinPrice( "CB", "M1", 230.00 ),
                    CabinPrice( "CB", "M2", 260.00 ),
                    CabinPrice( "CB", "S1", 245.00 ),
                    CabinPrice( "CB", "S2", 270.00 )
                  );

  getBestGroupPrices( rates, prices ).foreach( println );
}


/* -------------------------------------------------------------------------- *
 *
 * Author: Alex Ameen <alex.ameen.tx@gmail.com>
 * Last Update: Sun Jun  2 05:58:07 PM CDT 2024
 *
 *
 * ========================================================================== */

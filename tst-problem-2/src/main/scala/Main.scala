/**
 * @file Main.scala
 *
 * @brief Find optimal combinations of promotions.
 */

/* ========================================================================== */

/* Types */

/** @brief A unique identifier for a promotion. */
type PromoCode = String

/** @brief A promotion indicating other incompatible promotions. */
case class Promotion( code:              PromoCode
                    , notCombinableWith: Seq[PromoCode]
                    );

/** @brief A collection of multiple compatible promotions. */
case class PromotionCombo( promotionCodes: Seq[PromoCode] );


/* -------------------------------------------------------------------------- */

/**
 * @brief Emit a set of all combinations of codes ignoring whether they
 *        are truly combinable.
 *
 * This is a helper for @a allCombinablePromotions used to produce a base set
 * of collections for filtering.
 */
def combinationsAnyLength( allPromotions: Seq[Promotion]
                         ) : Seq[Seq[PromoCode]] =
{
  val keys   = allPromotions.map( promotion => promotion.code ).distinct.toSeq;
  var combos = Seq[Seq[PromoCode]]();
  for ( length <- 1 to allPromotions.length )
    {
      combos = combos ++ keys.combinations( length ).toSeq;
    }
  return combos.distinct;
}


/* -------------------------------------------------------------------------- */

/**
 * @brief Predicate which validates a set of promotion codes as
 *        being _combinable_.
 */
def isValidCombo( allPromotions: Seq[Promotion]
                , combo:         Seq[PromoCode]
                ): Boolean =
{
  val byCode: Map[PromoCode, Seq[PromoCode]] =
    allPromotions
      .groupMapReduce( promotion         => promotion.code )
                     ( promotion         => promotion.notCombinableWith )
                     ( ( acc, nonCombo ) => assert( false ) );
  var keeps = combo.toSet;
  for ( promoCode <- combo ) { keeps = keeps -- byCode( promoCode ); }
  return keeps.toList == combo;
}


/* -------------------------------------------------------------------------- */

/**
 * @brief Generate a set of all valid promotion combinations, including many
 *        redundant_ entries which are sub-optimal.
 */
def allCombinablePromotionsWithRedundancy( allPromotions: Seq[Promotion]
                                    ): Seq[PromotionCombo] =
{
  return combinationsAnyLength( allPromotions )
    .filter( combo => isValidCombo( allPromotions, combo ) )
    .map( combo => PromotionCombo( combo ) );
}


/* -------------------------------------------------------------------------- */

/**
 * @brief Predicate which checks if @param comboA is completely contained by the
 *        larger combination @param comboB.
 */
def comboIsSubsetOf( comboA: PromotionCombo, comboB: PromotionCombo ): Boolean =
{
  val setA = comboA.promotionCodes.toSet;
  val setB = comboB.promotionCodes.toSet;
  return ( setA.size < setB.size ) && setA.subsetOf( setB );
}


/* -------------------------------------------------------------------------- */

/**
 * @brief Generate a set of all valid promotion combinations, including only
 *        _optimal_ sets of promotions.
 *
 * This means no _redundant_ smaller combinations will be returned.
 */
def allCombinablePromotions( allPromotions: Seq[Promotion]
                           ): Seq[PromotionCombo] =
{
  val combos = allCombinablePromotionsWithRedundancy( allPromotions );
  var keeps  = Seq[PromotionCombo]();
  /* Filter out any combos which are a subset of a larger combo. */
  for ( a <- combos )
    {
      var keep = true;
      for ( b <- combos if keep && ( a != b ) )
        {
          keep &= ! comboIsSubsetOf( a, b );
        }
      if ( keep ) { keeps = keeps.appended( a ); }
    }
  return keeps;
}


/* -------------------------------------------------------------------------- */

/**
 * @brief Generate a set of valid promotion combinations containing
 *        @param promotionCode, including only _optimal_ sets of promotions.
 *
 * This means no _redundant_ smaller combinations will be returned.
 */
def combinablePromotions( promotionCode: PromoCode
                        , allPromotions: Seq[Promotion]
                        ): Seq[PromotionCombo] =
{
  /* Filter out any that aren't immediately combinable with `promotionCode'. */
  val valids = allPromotions.filter( promotion =>
    ( promotionCode == promotion.code ) ||
    isValidCombo( allPromotions, Seq( promotionCode, promotion.code ) )
  );
  return allCombinablePromotions( valids );
}


/* -------------------------------------------------------------------------- */

/** @brief Program entry. */
@main def problem2(): Unit =
{
  val promos = Seq( Promotion( "P1", Seq( "P3" ) )
                  , Promotion( "P2", Seq( "P4", "P5" ) )
                  , Promotion( "P3", Seq( "P1" ) )
                  , Promotion( "P4", Seq( "P2" ) )
                  , Promotion( "P5", Seq( "P2" ) )
                  );

  println( "allCombinablePromotions:" )
  allCombinablePromotions( promos ).foreach( println );

  println("\ncombinablePromotions with P1:")
  combinablePromotions( "P1", promos ).foreach( println );

  println("\ncombinablePromotions with P3:")
  combinablePromotions( "P3", promos ).foreach( println );
}


/* -------------------------------------------------------------------------- *
 *
 * Author: Alex Ameen <alex.ameen.tx@gmail.com>
 * Last Update: Sun Jun  4 06:03 PM CDT 2024
 *
 *
 * ========================================================================== */

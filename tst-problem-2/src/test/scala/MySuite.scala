/* ========================================================================== *
 *
 * For more information on writing tests, see
 * https://scalameta.org/munit/docs/getting-started.html
 *
 * NOTE: Don't make tests depend on sorted order of sequence members.
 *       Use `contains` and `sorted` when sequences contain multiple elements.
 *
 * -------------------------------------------------------------------------- */

/** Common set of promotions used by several tests. */
val promos = Seq( Promotion( "P1", Seq( "P3" ) )
                , Promotion( "P2", Seq( "P4", "P5" ) )
                , Promotion( "P3", Seq( "P1" ) )
                , Promotion( "P4", Seq( "P2" ) )
                , Promotion( "P5", Seq( "P2" ) )
                );


/* -------------------------------------------------------------------------- */

class MySuite extends munit.FunSuite {

/* -------------------------------------------------------------------------- */

  test( "combinationsAnyLength works for a trivially small set" ) {
    val obtained = combinationsAnyLength( Seq( Promotion( "A", Seq() ),
                                               Promotion( "B", Seq() )
                                             )
                                        );
    val expected = Seq( Seq("A"), Seq( "B" ), Seq( "A", "B" ) );
    assertEquals( obtained, expected)
  }


/* -------------------------------------------------------------------------- */

  test( "combinationsAnyLength works for empty set" ) {
    val obtained = combinationsAnyLength( Seq() );
    val expected = Seq();
    assertEquals( obtained, expected );
  }


/* -------------------------------------------------------------------------- */

  test( "isValidCombo detects trivial positives" ) {
    assert( isValidCombo( promos, Seq( "P1", "P2" ) ) );
  }


/* -------------------------------------------------------------------------- */

  test( "isValidCombo detects duplicates" ) {
    assert( ! isValidCombo( promos, Seq( "P2", "P2" ) ) );
  }


/* -------------------------------------------------------------------------- */

  test( "isValidCombo detects trivial negatives" ) {
    assert( ! isValidCombo( promos, Seq( "P1", "P3" ) ) );
  }


/* -------------------------------------------------------------------------- */

  test( "allCombinablePromotionsWithRedundancy works on trivially small set" ) {
    val obtained = allCombinablePromotionsWithRedundancy(
      Seq( Promotion( "A", Seq( "B" ) )
         , Promotion( "B", Seq() )
         , Promotion( "C", Seq() )
      )
    ).map( promotion =>
      /* Sort combo members so later equality check works. */
      PromotionCombo( promotion.promotionCodes.sorted )
    );
    val expected = Seq(
      PromotionCombo( Seq( "A" ) )
    , PromotionCombo( Seq( "B" ) )
    , PromotionCombo( Seq( "C" ) )
    , PromotionCombo( Seq( "A", "C" ) )
    , PromotionCombo( Seq( "B", "C" ) )
    );
    assert( expected.forall( combo => obtained.contains( combo ) ) );
  }


/* -------------------------------------------------------------------------- */

  test( "allCombinablePromotions works on a trivially small set" ) {
    val obtained = allCombinablePromotions(
      Seq( Promotion( "A", Seq( "B" ) )
         , Promotion( "B", Seq() )
         , Promotion( "C", Seq() )
      )
    ).map( promotion =>
      /* Sort combo members so later equality check works. */
      PromotionCombo( promotion.promotionCodes.sorted )
    );
    val expected = Seq(
      PromotionCombo( Seq( "A", "C" ) )
    , PromotionCombo( Seq( "B", "C" ) )
    );
    assert( expected.forall( combo => obtained.contains( combo ) ) );
  }


/* -------------------------------------------------------------------------- */

  test( "combinablePromotions works on a trivially small set" ) {
    val obtained = combinablePromotions(
      "A"
    , Seq( Promotion( "A", Seq( "B" ) )
         , Promotion( "B", Seq() )
         , Promotion( "C", Seq() )
      )
    );
    val expected = Seq( PromotionCombo( Seq( "A", "C" ) ) );
    assertEquals( obtained, expected );
  }


/* -------------------------------------------------------------------------- */

}  // End `class MySuite ...'


/* -------------------------------------------------------------------------- *
 *
 * Author: Alex Ameen <alex.ameen.tx@gmail.com>
 *
 *
 * ========================================================================== */

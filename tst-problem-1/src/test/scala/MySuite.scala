/* ========================================================================== *
 *
 * For more information on writing tests, see
 * https://scalameta.org/munit/docs/getting-started.html
 *
 *
 * -------------------------------------------------------------------------- */

class MySuite extends munit.FunSuite {

/* -------------------------------------------------------------------------- */

  test( "bestGroupPrices works on singleton input" ) {
    val obtained = getBestGroupPrices( Seq( Rate( "M1", "Military" ) )
                                     , Seq( CabinPrice( "CA", "M1", 1.0 ) )
                                     );
    val expected = Seq( BestGroupPrice( "CA", "M1", 1.0, "Military" ) );
    assertEquals( obtained, expected );
  }


/* -------------------------------------------------------------------------- */

  test( "bestGroupPrices works on trivially small inputs" ) {
    val obtained = getBestGroupPrices( Seq( Rate( "M1", "Military" ) )
                                     , Seq( CabinPrice( "CA", "M1", 1.0 )
                                          , CabinPrice( "CB", "M1", 2.0 )
                                          )
                                     );
    val expected = Seq( BestGroupPrice( "CA", "M1", 1.0, "Military" ) );
    assertEquals( obtained, expected );
  }


/* -------------------------------------------------------------------------- */

  test( "bestGroupPrices isn't just taking the first option" ) {
    val obtained = getBestGroupPrices( Seq( Rate( "M1", "Military" ) )
                                     , Seq( CabinPrice( "CA", "M1", 2.0 )
                                          , CabinPrice( "CB", "M1", 1.0 )
                                          )
                                     );
    val expected = Seq( BestGroupPrice( "CB", "M1", 1.0, "Military" ) );
    assertEquals( obtained, expected );
  }


/* -------------------------------------------------------------------------- */

  test( "bestGroupPrices produces throws for undefined groups" ) {
    val exception = ( intercept[NoSuchElementException] {
      getBestGroupPrices( Seq( Rate( "M2", "Military" ) )
                        , Seq( CabinPrice( "CA", "M1", 2.0 )
                        , CabinPrice( "CB", "M1", 1.0 )
                        )
                        );
    } ).getMessage;
    assertEquals( exception, "key not found: M1" );
  }


/* -------------------------------------------------------------------------- */

}  // End `class MySuite ...'


/* -------------------------------------------------------------------------- *
 *
 * Author: Alex Ameen <alex.ameen.tx@gmail.com>
 * Last Update: Sun Jun  4 06:17 PM CDT 2024
 *
 *
 * ========================================================================== */

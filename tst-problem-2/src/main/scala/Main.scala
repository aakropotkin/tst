/**
 * @file Main.scala
 *
 * @brief ???
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

def combinablePromotions( promotionCode: PromoCode,
                        , allPromotions: Seq[Promotion]
                        ): Seq[PromotionCombo] =
{
  // TODO
}


/* -------------------------------------------------------------------------- */

def allCombinablePromotions( allPromotions: Seq[Promotion]
                           ): Seq[PromotionCombo] =
{
  // TODO
}


/* -------------------------------------------------------------------------- */

/** @brief Program entry. */
@main def problem2(): Unit =
{
  // TODO
}


/* -------------------------------------------------------------------------- *
 *
 * Author: Alex Ameen <alex.ameen.tx@gmail.com>
 * Last Update: Sun Jun  2 06:13 PM CDT 2024
 *
 *
 * ========================================================================== */

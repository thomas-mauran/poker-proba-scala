import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{an, shouldBe, be, should, shouldEqual}
import Poker.*

class ProbaTest extends AnyFlatSpec  {
  "getAllCards" should "be defined" in {
    val res = getAllCards()

    val as = res.filter(p => p match {
      case PlayingCard.Group(_, x) => x == Numero.As
    })
    val trefle = res.filter(p => p match {
      case PlayingCard.Group(x, _) => x == Couleurs.Trefle
    })

    res.length shouldEqual 52
    as.length shouldEqual 4
    trefle.length shouldEqual 13
  }

  "cartePossibleMain" should "be defined" in {
    val main = PlayingCard.Group(Couleurs.Carreau, Numero.As) :: PlayingCard.Group(Couleurs.Carreau, Numero.Huit) :: PlayingCard.Group(Couleurs.Carreau, Numero.Six) :: PlayingCard.Group(Couleurs.Carreau, Numero.Quatre) :: Nil
    val res = cartePossibleMain(main, MainsPossible.Paire)
    info(res.toString())
    info(getProbaMain(main, MainsPossible.Paire).toString())
  }
}

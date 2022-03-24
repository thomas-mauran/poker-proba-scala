import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{an, shouldBe, be, should, shouldEqual}
import Poker.*

class ProbaTest extends AnyFlatSpec  {
  "unionProba" should "be defined" in {

    val proba1 = setProba(emptyProba, MainsPossible.PlusHaute, 0.5)
    val proba2 = setProba(emptyProba, MainsPossible.PlusHaute, 0.5)

    val res = unionProba(proba1, proba2)
    res(MainsPossible.PlusHaute) shouldEqual 1.0
  }


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

  "getProbaMain" should "be defined" in {
    val main = PlayingCard.Group(Couleurs.Carreau, Numero.As) :: PlayingCard.Group(Couleurs.Carreau, Numero.Huit) :: PlayingCard.Group(Couleurs.Carreau, Numero.Six) :: PlayingCard.Group(Couleurs.Carreau, Numero.Quatre) :: Nil
    val res = getProbaMain(main, MainsPossible.PlusHaute)

    res shouldEqual 1
  }

  "testMains" should "be defined" in {
    val main1 = PlayingCard.Group(Couleurs.Coeur, Numero.Dix) :: PlayingCard.Group(Couleurs.Coeur, Numero.Valet) :: PlayingCard.Group(Couleurs.Coeur, Numero.Dame) :: PlayingCard.Group(Couleurs.Coeur, Numero.Roi) :: Nil
    val proba1 = testMains(main1)

    val main2 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Carreau, Numero.Deux) :: Nil
    val proba2 = testMains(main2)

    proba1(MainsPossible.PlusHaute) shouldEqual 1.0
    proba1(MainsPossible.Quinte) should be > 0.0
    proba1(MainsPossible.QuintFlush) should be > 0.0
    proba1(MainsPossible.QuintFlushRoyale) should be > 0.0
    proba2(MainsPossible.Paire) shouldEqual 1.0
    proba2(MainsPossible.Brelan) should be > 0.0
  }

  "probaMain" should "be defined" in {
    val main1 = PlayingCard.Group(Couleurs.Coeur, Numero.Dix) :: PlayingCard.Group(Couleurs.Coeur, Numero.Valet) :: PlayingCard.Group(Couleurs.Coeur, Numero.Dame) :: Nil
    info(probaMain(main1, 2).toString())

    val main2 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Trefle, Numero.Dix) :: Nil
    val proba2 = probaMain(main2, 2)
    info(probaMainToList(proba2).toString())
//    info(probaMainToList(proba2).toString())

    val main3 = PlayingCard.Group(Couleurs.Coeur, Numero.Dix) :: PlayingCard.Group(Couleurs.Trefle, Numero.Dix) :: Nil
//    info(probaMainToList(probaMain(main3, 2)).toString())
  }
}

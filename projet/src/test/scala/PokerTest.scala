import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{an, shouldBe, be, should, shouldEqual}
import Poker.*

class PokerTest extends AnyFlatSpec {
  /*
  "CarteClassement" should "be defined" in {
    CarteClassement.compare(PlayingCard.Group(Couleurs.Coeur, Numero.As), PlayingCard.Group(Couleurs.Coeur, Numero.Deux)) should be > 0
    CarteClassement.compare(PlayingCard.Group(Couleurs.Coeur, Numero.Deux), PlayingCard.Group(Couleurs.Coeur, Numero.As)) should be < 0
    CarteClassement.compare(PlayingCard.Group(Couleurs.Coeur, Numero.As), PlayingCard.Group(Couleurs.Coeur, Numero.As)) shouldEqual 0
  }

  "HandClassement" should "be defined" in {
    HandClassement.compare(PokerHand.Group(MainsPossible.PlusHaute), PokerHand.Group(MainsPossible.Full)) should be < 0
    HandClassement.compare(PokerHand.Group(MainsPossible.Full), PokerHand.Group(MainsPossible.PlusHaute)) should be > 0
    HandClassement.compare(PokerHand.Group(MainsPossible.PlusHaute), PokerHand.Group(MainsPossible.PlusHaute)) shouldEqual  0
  }
  */

  "getOccurence" should "be defined" in {
    val maMain1 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.Trois) :: Nil
    getOccurence(PlayingCard.Group(Couleurs.Coeur, Numero.As), maMain1) shouldEqual 1

    val maMain2 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.Trois) :: Nil
    getOccurence(PlayingCard.Group(Couleurs.Coeur, Numero.As), maMain2) shouldEqual 0

    val maMain3 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.Trois) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.Dix) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: Nil
    getOccurence(PlayingCard.Group(Couleurs.Coeur, Numero.As), maMain3) shouldEqual 3
  }

  "getCouleurOccurence" should "be defined" in {
    val maMain1 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.Trois) :: Nil
    getCouleurOccurence(PlayingCard.Group(Couleurs.Coeur, Numero.As), maMain1) shouldEqual 3

    val maMain2 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Carreau, Numero.Trois) :: Nil
    getCouleurOccurence(PlayingCard.Group(Couleurs.Coeur, Numero.As), maMain2) shouldEqual 1

    val maMain3 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Trefle, Numero.As) :: PlayingCard.Group(Couleurs.Pique, Numero.Trois) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Carreau, Numero.Dix) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: Nil
    getCouleurOccurence(PlayingCard.Group(Couleurs.Coeur, Numero.As), maMain3) shouldEqual 3
  }

  "getMultipleOccurence" should "be defined" in {
    val maMain1 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: Nil
    val maMain12 = (PlayingCard.Group(Couleurs.Coeur, Numero.Deux), 2) :: (PlayingCard.Group(Couleurs.Coeur, Numero.As), 3) :: Nil
    getMultipleOccurence(maMain1) shouldBe maMain12
  }

  "getMainsNumero" should "be defined" in {
    val maMain1 = (PlayingCard.Group(Couleurs.Coeur, Numero.Deux), 3) :: (PlayingCard.Group(Couleurs.Coeur, Numero.As), 2) :: Nil
    val maMain12 = MainsPossible.DeuxPaire :: MainsPossible.Paire :: MainsPossible.Paire :: Nil
    getMainsNumero(maMain1) shouldBe maMain12
  }

  "getMains" should "be defined" in {
    val maMain1 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: Nil
    val mains = MainsPossible.PlusHaute :: MainsPossible.Full :: MainsPossible.DeuxPaire :: MainsPossible.Paire :: MainsPossible.Paire :: Nil
    getMains(maMain1) shouldBe mains
  }

  "getBestMains" should "be defined" in {
    val maMain1 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: Nil
    println(getMains(maMain1).sorted(HandClassement))
    getBestMain(maMain1) shouldBe MainsPossible.Full


    val maMain2 = PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.Roi) :: PlayingCard.Group(Couleurs.Coeur, Numero.Dame) :: PlayingCard.Group(Couleurs.Coeur, Numero.Valet) :: PlayingCard.Group(Couleurs.Coeur, Numero.Dix) :: Nil
    println(getMains(maMain2).sorted(HandClassement))
    getBestMain(maMain2) shouldBe MainsPossible.QuintFlushRoyale

  }

  /*
  "getQuinte" should "be defined" in {
    val main1 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.Trois) :: PlayingCard.Group(Couleurs.Coeur, Numero.Quatre) :: PlayingCard.Group(Couleurs.Coeur, Numero.Cinq) :: PlayingCard.Group(Couleurs.Trefle, Numero.Six) :: Nil
    getQuinte(main1) shouldEqual true
  }
  */

}
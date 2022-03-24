import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{an, shouldBe, be, should, shouldEqual}
import Poker.*

class PokerTest extends AnyFlatSpec {

  "CarteClassementNumero" should "be defined" in {
    CarteClassementNumero.compare(PlayingCard.Group(Couleurs.Coeur, Numero.As), PlayingCard.Group(Couleurs.Coeur, Numero.Deux)) should be > 0
    CarteClassementNumero.compare(PlayingCard.Group(Couleurs.Coeur, Numero.Deux), PlayingCard.Group(Couleurs.Coeur, Numero.As)) should be < 0
    CarteClassementNumero.compare(PlayingCard.Group(Couleurs.Coeur, Numero.As), PlayingCard.Group(Couleurs.Coeur, Numero.As)) shouldEqual 0
  }


  "HandClassement" should "be defined" in {
    HandClassement.compare(MainsPossible.PlusHaute, MainsPossible.Full) should be > 0
    HandClassement.compare(MainsPossible.Full, MainsPossible.PlusHaute) should be < 0
    HandClassement.compare(MainsPossible.PlusHaute, MainsPossible.PlusHaute) shouldEqual  0
  }


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

    val maMain1 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Trefle, Numero.Deux) :: Nil
    val mains1 = MainsPossible.PlusHaute :: MainsPossible.Paire :: Nil
    getMains(maMain1) shouldBe mains1

    val maMain2 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Trefle, Numero.Deux) :: PlayingCard.Group(Couleurs.Trefle, Numero.As) :: PlayingCard.Group(Couleurs.Pique, Numero.As) :: Nil
    val mains2 = MainsPossible.PlusHaute  :: MainsPossible.DeuxPaire :: MainsPossible.Paire :: MainsPossible.Brelan :: MainsPossible.Paire :: Nil
    getMains(maMain2) shouldBe mains2

    val maMain3 = PlayingCard.Group(Couleurs.Trefle, Numero.Deux) :: PlayingCard.Group(Couleurs.Carreau, Numero.Deux) :: PlayingCard.Group(Couleurs.Pique, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: Nil
    val mains3 = MainsPossible.PlusHaute :: MainsPossible.Carre ::  MainsPossible.Brelan :: MainsPossible.Paire :: Nil
    getMains(maMain3) shouldBe mains3


    val maMain4 = PlayingCard.Group(Couleurs.Trefle, Numero.As) :: PlayingCard.Group(Couleurs.Trefle, Numero.Dame) :: PlayingCard.Group(Couleurs.Trefle, Numero.Dix) :: PlayingCard.Group(Couleurs.Trefle, Numero.Valet) ::  PlayingCard.Group(Couleurs.Trefle, Numero.Roi) :: Nil
    val mains4 = MainsPossible.PlusHaute :: MainsPossible.QuintFlushRoyale :: MainsPossible.QuintFlush :: MainsPossible.Couleur :: Nil
    getMains(maMain4) shouldBe mains4

    val maMain5 = PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Trefle, Numero.Dame) :: PlayingCard.Group(Couleurs.Trefle, Numero.Dix) :: PlayingCard.Group(Couleurs.Trefle, Numero.Valet) ::  PlayingCard.Group(Couleurs.Trefle, Numero.Roi) :: Nil
    val mains5 = MainsPossible.PlusHaute :: MainsPossible.Quinte :: Nil
    getMains(maMain5) shouldBe mains5

    val maMain6 = PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Trefle, Numero.Deux) :: PlayingCard.Group(Couleurs.Trefle, Numero.Trois) :: PlayingCard.Group(Couleurs.Coeur, Numero.Valet) ::  PlayingCard.Group(Couleurs.Trefle, Numero.Roi) :: Nil
    val mains6 = MainsPossible.PlusHaute :: Nil
    getMains(maMain6) shouldBe mains6

    val maMain7 = PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Trefle, Numero.As) :: PlayingCard.Group(Couleurs.Trefle, Numero.Trois) :: PlayingCard.Group(Couleurs.Coeur, Numero.Trois) ::  PlayingCard.Group(Couleurs.Trefle, Numero.Roi) :: Nil
    val mains7 = MainsPossible.PlusHaute :: MainsPossible.DeuxPaire :: MainsPossible.Paire :: MainsPossible.Paire :: Nil
    getMains(maMain7) shouldBe mains7


  }

  "getBestMains" should "be defined" in {
    val maMain1 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.As) :: Nil
    info(getMains(maMain1).sorted(HandClassement).toString())
    getBestMain(maMain1) shouldBe MainsPossible.Full


    val maMain2 = PlayingCard.Group(Couleurs.Coeur, Numero.As) :: PlayingCard.Group(Couleurs.Coeur, Numero.Roi) :: PlayingCard.Group(Couleurs.Coeur, Numero.Dame) :: PlayingCard.Group(Couleurs.Coeur, Numero.Valet) :: PlayingCard.Group(Couleurs.Coeur, Numero.Dix) :: Nil
    info(maMain2.sorted(CarteClassementNumero).toString())
    info(getMains(maMain2).sorted(HandClassement).toString())
    getBestMain(maMain2) shouldBe MainsPossible.QuintFlushRoyale

  }


  "getQuinte" should "be defined" in {
    val main1 = PlayingCard.Group(Couleurs.Coeur, Numero.Deux) :: PlayingCard.Group(Couleurs.Coeur, Numero.Trois) :: PlayingCard.Group(Couleurs.Coeur, Numero.Quatre) :: PlayingCard.Group(Couleurs.Coeur, Numero.Cinq) :: PlayingCard.Group(Couleurs.Trefle, Numero.Six) :: Nil
    getQuinte(main1) shouldEqual true
  }

}
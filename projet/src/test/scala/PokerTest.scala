import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{an, shouldBe, be, should, shouldEqual}
import Poker.*

class PokerTest extends AnyFlatSpec {
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
}
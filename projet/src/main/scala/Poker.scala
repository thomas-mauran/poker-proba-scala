object Poker:
  enum Couleurs:
    case Trefle
    case Pique
    case Carreau
    case Coeur

  enum Numero:
    case Deux
    case Trois
    case Quatre
    case Cinq
    case Six
    case Sept
    case Huit
    case Neuf
    case Dix
    case Valet
    case Dame
    case Roi
    case As

  enum PlayingCard:
    case Group(col: Couleurs, num: Numero)

  object CarteClassement extends scala.math.Ordering[PlayingCard] {
    override def compare(card: PlayingCard, to: PlayingCard): Int = (card, to) match {
      case (PlayingCard.Group(_, x), PlayingCard.Group(_, y)) => x.ordinal - y.ordinal;
    }
  }

  enum MainsPossible:
    case PlusHaute
    case Paire
    case DeuxPaire
    case Brelan
    case Quinte
    case Couleur
    case Full
    case Carre
    case QuintFlush
    case QuintFlushRoyale

  enum PokerHand:
    case Group(main: MainsPossible)

  object HandClassement extends scala.math.Ordering[PokerHand] {
    override def compare(hand: PokerHand, to: PokerHand): Int = (hand, to) match {
      case (PokerHand.Group(x), PokerHand.Group(y)) => x.ordinal - y.ordinal;
    }
  }

  def hello(): Boolean = true
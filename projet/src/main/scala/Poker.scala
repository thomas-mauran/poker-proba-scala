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

  object CarteClassementNumero extends scala.math.Ordering[PlayingCard] {
    override def compare(card: PlayingCard, to: PlayingCard): Int = (card, to) match {
      case (PlayingCard.Group(_, x), PlayingCard.Group(_, y)) => x.ordinal - y.ordinal;
    }
  }

  object CarteClassementCouleur extends scala.math.Ordering[PlayingCard] {
    override def compare(card: PlayingCard, to: PlayingCard): Int = (card, to) match {
      case (PlayingCard.Group(x, _), PlayingCard.Group(y, _)) => x.ordinal - y.ordinal;
    }
  }

  def nextCard(card: PlayingCard): PlayingCard = card match {
    case PlayingCard.Group(a, x) => PlayingCard.Group(a, Numero.fromOrdinal(x.ordinal + 1))
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

  def getOccurence(card: PlayingCard, main: List[PlayingCard]): Int = (main, card) match {
    case (Nil, _) => 0
    case (PlayingCard.Group(_, x) :: z, PlayingCard.Group(_, y)) => if (x == y) 1 + getOccurence(card, z) else getOccurence(card, z)
  }

  def getMultipleOccurence(main: List[PlayingCard]): List[(PlayingCard, Int)] = main match {
    case Nil => Nil
    case x :: z => {
      val occur = getOccurence(x, z)
      val next = z.filter(p => (p, x) match {
        case (PlayingCard.Group(_, a), PlayingCard.Group(_, b)) => a != b
      })
      (x, occur + 1) :: getMultipleOccurence(next)
    }
  }

  def getCouleurOccurence(card: PlayingCard, main: List[PlayingCard]): Int = (main, card) match {
    case (Nil, _) => 0
    case (PlayingCard.Group(x, _) :: z, PlayingCard.Group(y, _)) => if (x == y) 1 + getCouleurOccurence(card, z) else getOccurence(card, z)
  }

  def getMainsNumero(occur: List[(PlayingCard, Int)]): List[MainsPossible] = occur match {
    case Nil => Nil
    case (c, i) :: z => {
      val next = getMainsNumero(z)
      val anotherPaire = next.filter(p => p == MainsPossible.Paire).length == 1
      val triple = z.filter((p, i) => i > 2).length == 1
      val carre = z.filter((p, i) => i > 3).length == 1
      if (carre) MainsPossible.Carre :: MainsPossible.Brelan :: MainsPossible.Paire :: next else
        if (anotherPaire && triple) MainsPossible.Full :: MainsPossible.DeuxPaire :: MainsPossible.Paire :: next else
          if (anotherPaire) MainsPossible.DeuxPaire :: MainsPossible.Paire :: next else
            if (triple) MainsPossible.Brelan :: MainsPossible.Paire :: next else
              if (i > 1) MainsPossible.Paire :: next else next
    }
  }

  def getQuinte(main: List[PlayingCard]): List[MainsPossible] = {
    val quinte = isQuinte(main)
    val couleur = isCouleur(main)

    if (couleur && quinte) MainsPossible.Couleur :: MainsPossible.QuintFlush :: Nil else
      if (quinte) MainsPossible.Quinte :: Nil else
        if (couleur) MainsPossible.Couleur :: Nil else Nil
  }

  def isQuinte(main: List[PlayingCard]): Boolean = main match {
    case Nil => false
    case x :: y :: z => if (nextCard(x) == y) isQuinte(main) else false
  }

  def isCouleur(main: List[PlayingCard]): Boolean = main match {
    case Nil => false
    case PlayingCard.Group(x, _) :: PlayingCard.Group(y, _) :: z => if (x == z) isCouleur(z) else false
  }

  def getMains(main: List[PlayingCard]): List[MainsPossible] = MainsPossible.PlusHaute :: getMainsNumero(getMultipleOccurence(main)).concat(getQuinte(main))

  def getBestMain(main: List[PlayingCard]): MainsPossible = getMains(main).sorted(HandClassement).head

  object HandClassement extends scala.math.Ordering[MainsPossible] {
    override def compare(hand: MainsPossible, to: MainsPossible): Int = (hand, to) match {
      case (x, y) => y.ordinal - x.ordinal;
    }
  }

  def hello(): Boolean = true
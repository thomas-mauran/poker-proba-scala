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

  def isNextCard(card1: PlayingCard, card2: PlayingCard): Boolean = (card1, card2) match {
    case (PlayingCard.Group(a, x), PlayingCard.Group(_, y)) => if (x.ordinal + 1 > Numero.values.length - 1) false else Numero.fromOrdinal(x.ordinal + 1) == y
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
    case x :: z =>
      (x, getOccurence(x, z) + 1) :: getMultipleOccurence(z.filter(p => (p, x) match {
        case (PlayingCard.Group(_, a), PlayingCard.Group(_, b)) => a != b
      }))
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
      if (i > 3) MainsPossible.Carre :: MainsPossible.Brelan :: MainsPossible.Paire :: next
      else if (anotherPaire && i > 2) MainsPossible.Full :: MainsPossible.DeuxPaire :: MainsPossible.Paire :: next
        else if (i > 1 && anotherPaire) MainsPossible.DeuxPaire :: MainsPossible.Paire :: next
          else if (i > 2) MainsPossible.Brelan :: MainsPossible.Paire :: next
            else if (i > 1) MainsPossible.Paire :: next
              else next
    }
  }

  def getQuinte(main: List[PlayingCard]): List[MainsPossible] = {
    val mainSortedNumero = main.sorted(CarteClassementNumero)
    val quinte = isQuinte(mainSortedNumero)
    val couleur = isCouleur(mainSortedNumero)

    if (couleur && quinte && isQuinteFlushRoyale(mainSortedNumero)) MainsPossible.QuintFlushRoyale :: MainsPossible.QuintFlush :: MainsPossible.Couleur :: Nil
    else if (couleur && quinte) MainsPossible.Couleur :: MainsPossible.QuintFlush :: Nil
      else if (quinte) MainsPossible.Quinte :: Nil
        else if (couleur) MainsPossible.Couleur :: Nil
          else Nil
  }

  def isQuinteFlushRoyale(main: List[PlayingCard]): Boolean = main match {
    case Nil => false
    case PlayingCard.Group(_, x) :: z => x == Numero.Dix
  }

  def isQuinte(main: List[PlayingCard]): Boolean = main match {
    case Nil => false
    case x :: Nil => true
    case x :: y :: z => if (isNextCard(x, y)) isQuinte(y :: z) else false
  }

  def isCouleur(main: List[PlayingCard]): Boolean = main match {
    case Nil => false
    case x :: Nil => true
    case PlayingCard.Group(x, a) :: PlayingCard.Group(y, b) :: z => if (x == y) isCouleur(PlayingCard.Group(y, b) :: z) else false
  }

  def getMains(main: List[PlayingCard]): List[MainsPossible] = MainsPossible.PlusHaute :: getMainsNumero(getMultipleOccurence(main)).concat(getQuinte(main))

  def getBestMain(main: List[PlayingCard]): MainsPossible = getMains(main).sorted(HandClassement).head

  object HandClassement extends scala.math.Ordering[MainsPossible] {
    override def compare(hand: MainsPossible, to: MainsPossible): Int = (hand, to) match {
      case (x, y) => y.ordinal - x.ordinal;
    }
  }


  // Type qui assosie un double (probabilité) à une main
  type MainProba = MainsPossible => Double

  // renvoie ) 1 si la main n'est pas possible avec les cartes données et restantes
  def emptyProba: MainProba = (x: MainsPossible) => -1.0

  // pour definir les probas de chaque mains
  def setProba(proba: MainProba, mp: MainsPossible, value: Double): MainProba = (x: MainsPossible) => if (x == mp) value else proba(x)

  // f'(x) = k * f(x) => pour chacun
  def adjustProba(proba: MainProba, k: Double): MainProba = (x: MainsPossible) => if (proba(x) != -1.0) k * proba(x) else -1.0

  // Unie la probabilité de proba 1 et proba 2, si les deux ont le même index de défini: on additionne les deux
  def unionProba(proba1: MainProba, proba2: MainProba): MainProba = (x: MainsPossible) => if (proba1(x) != -1) if (proba2(x) != -1) Math.min(proba1(x) + proba2(x), 1) else proba1(x) else proba2(x)

  // converti un MainProba en une liste
  def probaMainToListAux(proba: MainProba, toCheck: List[MainsPossible]): List[(MainsPossible, Double)] = toCheck match {
    case Nil => Nil
    case x :: z => (x, proba(x)) :: probaMainToListAux(proba, z)
  }
  def probaMainToList(proba: MainProba): List[(MainsPossible, Double)] = probaMainToListAux(proba, MainsPossible.values.toList)
  def listToMainProba(list: List[(MainsPossible, Double)]): MainProba = list match {
    case Nil => emptyProba
    case (main, proba) :: z => setProba(listToMainProba(z), main, proba)
  }

  def getAllCardsAux(i: Int, j: Int, pile: List[PlayingCard]): List[PlayingCard] = {
    if (j + 1 > 12 && i + 1 > 4)
      pile
    else if (i + 1 > 4)
      getAllCardsAux(1, j + 1, PlayingCard.Group(Couleurs.fromOrdinal(0), Numero.fromOrdinal(j + 1)) :: pile)
    else
      getAllCardsAux(i + 1, j, PlayingCard.Group(Couleurs.fromOrdinal(i), Numero.fromOrdinal(j)) :: pile)
  }

  /*
    Fonction pour avoir toutes les cartes possibles, ici 52 cartes au total
  */
  def getAllCards(): List[PlayingCard] = getAllCardsAux(0, 0, Nil)

  def cartesPossible(exclude: List[PlayingCard]): List[PlayingCard] = getAllCards() diff exclude

  def cartePossibleMainAux(curMain: List[PlayingCard], main: MainsPossible, possible: List[PlayingCard], impossible: List[PlayingCard]): List[PlayingCard] = cartesPossible(curMain.concat(impossible).concat(possible)) match {
    case Nil => possible
    case x :: z => if (getMains(x :: curMain).filter(p => p == main).length > 0) cartePossibleMainAux(curMain, main, x :: possible, impossible) else cartePossibleMainAux(curMain, main, possible, x :: impossible)
  }

  /*
    Fonction pour savoir quelles cartes sont possible pour avoir la main, a condition que curMain ai une taille de 4 cartes, on cherche à trouver la 5eme
  */
  def cartePossibleMain(curMain: List[PlayingCard], main: MainsPossible): List[PlayingCard] = cartePossibleMainAux(curMain, main, Nil, Nil)

  def getProbaMain(curMain: List[PlayingCard], testMain: MainsPossible): Double = cartePossibleMain(curMain, testMain).length / (52.0 - curMain.length)

  def testMainsAux(curMain: List[PlayingCard], proba: MainProba, toTest: List[MainsPossible]): MainProba = toTest match {
    case Nil => emptyProba
    case x :: z => unionProba(setProba(proba, x, getProbaMain(curMain, x)), testMainsAux(curMain, proba, z))
  }
  /*
    Renvoi les proba sur chaqune des mains pour la dernière carte à tester
  */
  def testMains(curMain: List[PlayingCard]): MainProba = testMainsAux(curMain, emptyProba, MainsPossible.values.toList)

  def probaMainAux(curMain: List[PlayingCard], m: Int, toTest: List[PlayingCard]): MainProba = (m, toTest) match {
    case (0, _) => setProba(emptyProba, getBestMain(curMain), 1)
    case (1, _) => testMains(curMain)
    case (_, Nil) => emptyProba
    case (_, x :: z) => unionProba( listToMainProba(probaMainToList(probaMainAux(x :: curMain, m-1, cartesPossible(x :: curMain)))), listToMainProba(probaMainToList(probaMainAux(curMain, m, z)))) // on reconvertit en list puis de nouveau en MainProba à cause d'un obscure bug qui faisais une boucle infinie
  }

  /*
    Renvoi toutes les proba sur chaqune des mains
  */
  def probaMain(curMain: List[PlayingCard], m: Int): MainProba = probaMainAux(curMain, m, cartesPossible(curMain))

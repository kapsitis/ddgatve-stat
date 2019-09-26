object HuffmanCode {

	abstract class BinaryTree
	case class Inner(left: BinaryTree, 
	    right: BinaryTree, 
	    chars: List[Char], 
	    weight: Int) extends BinaryTree
		case class Leaf(char: Char, 
		    weight: Int) extends BinaryTree


  def getWeight(tree: BinaryTree): Int = tree match {
	  case Inner(_,_,_,w) => w
	  case Leaf(_,w) => w
	}

	def getChars(tree: BinaryTree): List[Char] = tree match {
	  case Inner(_,_,cs,_) => cs
	  case Leaf(c,_) => List(c)
	}

	def makeCodeTree(left: BinaryTree, right: BinaryTree) =
			Inner(left, right, getChars(left) ::: getChars(right), 
			    getWeight(left) + getWeight(right))

  def getSampleTree(): BinaryTree = {
			    Inner(Inner(Leaf('a',2), Leaf('b',3), List('a','b'), 5), 
      Leaf('d',4), List('a','b','d'), 9)
	}
	
	  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.sortWith((f1,f2) => f1._2 < f2._2).map((f) => Leaf (f._1, f._2))
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  //def singleton(trees: List[BinaryTree]): Boolean = trees.size == 1

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Inner` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[BinaryTree]): List[BinaryTree] = trees match {
    case left :: right :: cs => (makeCodeTree(left, right) :: cs)
                                  .sortWith((t1, t2) => getWeight(t1) < getWeight(t2))
    case _ => trees
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[BinaryTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def untilSingle(f: List[BinaryTree]=>List[BinaryTree])(
      trees: List[BinaryTree]): List[BinaryTree] = {
    if (trees.size == 1) trees
    else untilSingle(f)( f(trees) )
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): BinaryTree = {
    untilSingle(combine)( makeOrderedLeafList(StringUtils.getFrequencies(chars)) ).head
  }



  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: BinaryTree, bits: List[Bit]): List[Char] = {
    def traverse(remaining: BinaryTree, bits: List[Bit]): List[Char] = remaining match {
      case Leaf(c, _) if bits.isEmpty => List(c)
      case Leaf(c, _) => c :: traverse(tree, bits)
      case Inner(left, right, _, _) if bits.head == 0 => traverse(left, bits.tail)
      case Inner(left, right, _, _) => traverse(right, bits.tail)
    }

    traverse(tree, bits)
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: BinaryTree = Inner(Inner(Inner(Leaf('s',121895),Inner(Leaf('d',56269),Inner(Inner(Inner(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Inner(Inner(Inner(Inner(Leaf('z',2093),Inner(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Inner(Inner(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Inner(Inner(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Inner(Inner(Inner(Leaf('r',100500),Inner(Leaf('c',50003),Inner(Leaf('v',24975),Inner(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Inner(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Inner(Leaf('e',225947),Inner(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)



  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: BinaryTree)(text: List[Char]): List[Bit] = {
    def lookup(tree:  BinaryTree)(c: Char): List[Bit] = tree match {
      case Leaf(_, _) => List()
      case Inner(left, right, _, _) if getChars(left).contains(c) => 0 :: lookup(left)(c)
      case Inner(left, right, _, _) => 1 :: lookup(right)(c)
    }

    text flatMap lookup(tree)
  }


  // Part 4b: Encoding using code table

  type Code = (Char, List[Bit])
  type CodeTable = List[Code]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table.filter( (code) => code._1 == char ).head._2
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
   def convert(tree: BinaryTree): CodeTable = tree match {
     case Leaf(c, w) => List( (c, List()) )
     case Inner(left, right, cs, w) => mergeCodeTables(convert(left), convert(right))
   }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    def prepend(b: Bit)(code: Code): Code = (code._1, b :: code._2)

    a.map(prepend(0)) ::: b.map(prepend(1))
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: BinaryTree)(text: List[Char]): List[Bit] = text flatMap codeBits(convert(tree))


}
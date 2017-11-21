package bank

/* The Scala Trait to implement */ 
trait TransValidator {
	def process(e: message)
	def getValidTrans: List[((Nat.nat, (Nat.nat, Nat.nat)),Nat.nat)]
	def authors: String
}
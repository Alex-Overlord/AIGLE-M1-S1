package il_tp9_calculette;

public abstract class EtatCalculette {
	static protected enum operations {plus, moins, mult, div};
	
	abstract int enter(String s) throws CalculetteException, CalculetteNumberException, CalculetteUnknownOperator;
	Calculette calc;
	
	EtatCalculette(Calculette c) { calc = c; }
}

package il_tp9_calculette;

public class ENombre1 extends EtatCalculette {
	ENombre1(Calculette c) { super(c); }
	
	public int enter(String s) throws CalculetteException, CalculetteNumberException {
		try {
			calc.setAccumulateur(Float.parseFloat(s));
		} catch (NumberFormatException e) {
			throw new CalculetteNumberException(s); 
		}
		return (3);
	}
}

package il_tp9_calculette;

public class ENombre2 extends EtatCalculette {
	
	ENombre2(Calculette c) { 
		super(c); 
	}
	
	int enter(String s) throws CalculetteException, CalculetteNumberException, CalculetteUnknownOperator {
		float temp = 0;
		
		try {
			temp = Float.parseFloat(s);
		} catch (NumberFormatException e) {
			throw new CalculetteNumberException(s);
		}
		
		switch(operations.valueOf(calc.getOperateur())) {
		case plus: 
			calc.setAccumulateur(calc.getAccumulateur() + temp);
			break;
		case mult: 
			calc.setAccumulateur(calc.getAccumulateur() * temp);
			break;
		default:
			throw new CalculetteUnknownOperator(calc.getOperateur());
		}
		return (2);
	}
}

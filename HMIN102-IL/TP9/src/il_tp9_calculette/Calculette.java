package il_tp9_calculette;

public class Calculette {
	protected EtatCalculette etatCourant;
	protected EtatCalculette[] etats = new EtatCalculette[3];
	double accumulateur;
	String operateur;
	
	public Calculette() {
		etats[0] = new ENombre1(this);
		etats[1] = new EOperateur(this);
		etats[2] = new ENombre2(this);
		etatCourant = etats[0];
		accumulateur = 0;
	}

	public double getAccumulateur() { return accumulateur; }
	public void setAccumulateur(double accumulateur) { this.accumulateur = accumulateur; }

	public String getOperateur() { return operateur; }
	public void setOperateur(String operateur) { this.operateur = operateur; }
	
	public double getResult() { return accumulateur; }
	
	public void enter(String s) throws CalculetteException, CalculetteNumberException, CalculetteUnknownOperator{
        etatCourant = etats[etatCourant.enter(s) - 1];
	}
	
	public static void main(String[] args) throws CalculetteException, CalculetteNumberException, CalculetteUnknownOperator {
		Calculette c = new Calculette();
		System.out.println("calcul infixé");
		c.enter("123"); // etat 1 : stocke le nombre 123 dans accumulateur
		c.enter("plus"); // etat 2 : stocke l'operation à effectuer dans un registre
		c.enter("234"); // etat 3 : stocke le résultat de l'opération dans accumulateur
		c.enter("plus");
		c.enter("1");
		System.out.println(c.getResult());
	}
}

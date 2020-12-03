
import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TreeMap;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.DoubleWritable;
//import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
//import org.apache.hadoop.io.WritableComparable;
//import org.apache.hadoop.io.WritableComparator;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;


/*
 * Jusqu'� pr�sent nous avons d�fini nos mappers et reducers comme des classes internes � notre classe principale.
 * Dans des applications r�elles de map-reduce cela ne sera g�n�ralement pas le cas, les classes seront probablement localis�es dans d'autres fichiers.
 * Dans cet exemple, nous avons d�fini Map et Reduce en dehors de notre classe principale.
 * Il se pose alors le probl�me du passage du param�tre 'k' dans notre reducer, car il n'est en effet plus possible de d�clarer un param�tre k dans notre 
 * classe principale qui serait partag� avec ses classes internes ; c'est l� que la Configuration du Job entre en jeu.
 */
/*
 - k valeur
 - si k = 3 : r�cup�rer les 3 premi�re valeur
 - l'ordre importe peu
 - 
*/

//Comparateur
//@SuppressWarnings("rawtypes")
//class Comparator1<T extends WritableComparable> extends WritableComparator {
//	private static final Logger LOG = Logger.getLogger(TopkWordCount_1.class.getName());
//	
//	
//	public Comparator1(Class<T> parameterClass) {
//		super(parameterClass, true);
//	}
//	@SuppressWarnings("unchecked")
//	@Override
//	public int compare(WritableComparable a, WritableComparable b) {
////		LOG.info("test");
//		return a.compareTo(b);
//	}
//}
//class TextComparator1 extends Comparator1<Text> {
//	public TextComparator1() {
//		super(Text.class);
//	}
//}

// =========================================================================
// Job A
// =========================================================================
class Map2A extends Mapper<LongWritable, Text, Text, DoubleWritable> {
	private final static String emptyWords[] = { "" };
	private static int compt = 0;

	@Override
	public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
		compt++;
		String line = value.toString();

		String[] words = line.split(",");

		if (Arrays.equals(words, emptyWords))
			return;

		if (compt != 1) {
			context.write(new Text(words[5] + " " + words[6] + ","), new DoubleWritable(Double.parseDouble(words[20])));
		}
	}
}

class Reduce2A extends Reducer<Text, DoubleWritable, Text, DoubleWritable> {
	
	@Override
	public void reduce(Text key, Iterable<DoubleWritable> values, Context context)
			throws IOException, InterruptedException {
		double sum = 0.0;

		for (DoubleWritable val : values)
			sum += val.get();

		context.write(key, new DoubleWritable(sum));
	}
}


//=========================================================================
// Job B
//=========================================================================
class Map2B extends Mapper<LongWritable, Text, Text, DoubleWritable> {
	private final static String emptyWords[] = { "" };
	private static int compt = 0;

	@Override
	public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
		compt++;
		String line = value.toString();

		String[] words = line.split(",");

		if (Arrays.equals(words, emptyWords))
			return;
		
		if (compt != 1) {
			context.write(new Text(words[0]), new DoubleWritable(Double.parseDouble(words[1])));
		}
	}
}

class Reduce2B extends Reducer<Text, DoubleWritable, Text, DoubleWritable> {
	/**
	 * Map avec tri suivant l'ordre naturel de la cl� (la cl� repr�sentant la fr�quence d'un ou plusieurs mots).
	 * Utilis� pour conserver les k mots les plus fr�quents.
	 * 
	 * Il associe une fr�quence � une liste de mots.
	 */
	private TreeMap<Double, List<Text>> sortedWords = new TreeMap<>();
	private int nbsortedWords = 0;
	private int k;

	/**
	 * M�thode appel�e avant le d�but de la phase reduce.
	 */
	@Override
	public void setup(Context context) {
		// On charge k
		k = context.getConfiguration().getInt("k", 1);
	}

	@Override
	public void reduce(Text key, Iterable<DoubleWritable> values, Context context)
			throws IOException, InterruptedException {
		double sum = 0.0;

		for (DoubleWritable val : values)
			sum += val.get();

		// On copie car l'objet key reste le m�me entre chaque appel du reducer
		Text keyCopy = new Text(key);

		// Fr�quence d�j� pr�sente
		if (sortedWords.containsKey(sum)) // si la fr�quence est d�j� dans treeMap on ajoute les mots
			sortedWords.get(sum).add(keyCopy);
		else { // sinon on ajoute la fr�quence et les mots
			List<Text> words = new ArrayList<>();

			words.add(keyCopy);
			sortedWords.put(sum, words); 
		}

		// Nombre de mots enregistr�s atteint : on supprime le mot le moins fr�quent (le premier dans sortedWords)
		if (nbsortedWords == k) {
			Double firstKey = sortedWords.firstKey();
			List<Text> words = sortedWords.get(firstKey);
			words.remove(words.size() - 1);

			if (words.isEmpty())
				sortedWords.remove(firstKey);
		} else
			nbsortedWords++;
	}

	/**
	 * M�thode appel�e � la fin de l'�tape de reduce.
	 * 
	 * Ici on envoie les mots dans la sortie, tri�s par ordre descendant.
	 */
	@Override
	public void cleanup(Context context) throws IOException, InterruptedException {
		Double[] nbofs = sortedWords.keySet().toArray(new Double[0]); // les fr�quences

		// Parcours en sens inverse pour obtenir un ordre descendant
		int i = nbofs.length;

		while (i-- != 0) {
			Double nbof = nbofs[i];

			for (Text words : sortedWords.get(nbof)) {
				context.write(words, new DoubleWritable(nbof));
			}
		}
	}
}

public class TopkWordCount_2 {
	private static final String INPUT_PATH = "input-groupBy/";
	private static final String OUTPUT_PATH = "output/TopkWordCount2A-";
	private static final String OUTPUT_PATH_B = "output/TopkWordCount2B-";
	private static final Logger LOG = Logger.getLogger(TopkWordCount_2.class.getName());

	static {
		System.setProperty("java.util.logging.SimpleFormatter.format", "%5$s%n%6$s");

		try {
			FileHandler fh = new FileHandler("out.log");
			fh.setFormatter(new SimpleFormatter());
			LOG.addHandler(fh);
		} catch (SecurityException | IOException e) {
			System.exit(1);
		}
	}

	/**
	 * Ce programme permet le passage d'une valeur k en argument de la ligne de commande.
	 */
	public static void main(String[] args) throws Exception {
		// Borne 'k' du topk
		int k = 10;

		try {
			// Passage du k en argument ?
			if (args.length > 0) {
				k = Integer.parseInt(args[0]);

				// On contraint k � valoir au moins 1
				if (k <= 0) {
					LOG.warning("k must be at least 1, " + k + " given");
					k = 1;
				}
			}
		} catch (NumberFormatException e) {
			LOG.severe("Error for the k argument: " + e.getMessage());
			System.exit(1);
		}

		Configuration conf = new Configuration();
		conf.set("fs.file.impl", "com.conga.services.hadoop.patch.HADOOP_7682.WinLocalFileSystem");
		
		conf.setInt("k", k);

		Job jobA = new Job(conf, "A");

		jobA.setOutputKeyClass(Text.class);
		jobA.setOutputValueClass(DoubleWritable.class);

		jobA.setMapperClass(Map2A.class);
		jobA.setReducerClass(Reduce2A.class);

		jobA.setInputFormatClass(TextInputFormat.class);
		jobA.setOutputFormatClass(TextOutputFormat.class);

		long instant = Instant.now().getEpochSecond();
		FileInputFormat.addInputPath(jobA, new Path(INPUT_PATH));
		FileOutputFormat.setOutputPath(jobA, new Path(OUTPUT_PATH + instant));

		jobA.waitForCompletion(true);
		
		Job jobB = new Job(conf, "B");

		jobB.setOutputKeyClass(Text.class);
		jobB.setOutputValueClass(DoubleWritable.class);

		jobB.setMapperClass(Map2B.class);
		jobB.setReducerClass(Reduce2B.class);

		jobB.setInputFormatClass(TextInputFormat.class);
		jobB.setOutputFormatClass(TextOutputFormat.class);

		FileInputFormat.addInputPath(jobB,  new Path(OUTPUT_PATH + instant));
		FileOutputFormat.setOutputPath(jobB, new Path(OUTPUT_PATH_B + instant));

		jobB.waitForCompletion(true);
	}
}
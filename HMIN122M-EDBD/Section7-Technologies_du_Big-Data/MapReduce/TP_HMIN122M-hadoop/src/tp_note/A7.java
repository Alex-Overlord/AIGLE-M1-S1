package tp_note;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
//import java.util.ArrayList;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
//import org.apache.hadoop.io.IntWritable;
//import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.WritableComparable;
import org.apache.hadoop.io.WritableComparator;
//import org.apache.hadoop.io.WritableComparable;
//import org.apache.hadoop.io.WritableComparator;
//import org.apache.hadoop.io.WritableComparable;
//import org.apache.hadoop.io.WritableComparator;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;

/* 7. Quel est le role le plus occupé par les femmes, ainsi que celui par les hommes, et la moyenne respectif de leur cachet ? (count, gender, role, moyenne(salaire))}
SELECT COUNT(*), Acteur.gender, Role.nom, AVG(Cachets.montant) FROM Role, Acteur, Cachets GROUP BY Acteur.gender;\\

Jointure par hachage
Mapper A : 
Acteur : emit(ida, gender)
Role : emit (idr, nom)
Cachets : emit(ida+idr, montant)

Reducer A : emit(ida+idr, gender+montant+nom)

Group By
Mapper B : emit(nom+gender, montant) 
Reducer B : emit(nom+gender, moyenne(montant)+nb_acteur)

Max(gender)
Mapper C : emit(gender, nom+moyenne(montant)+nb_acteur)
Reducer C : emit(gender, nom+moyenne(montant)+nb_acteur)
 */

// Main class
public class A7 {
	private static final String INPUT_PATH = "input-disneyland/";
	private static final String OUTPUT_PATH = "output/A7-";
	private static final String OUTPUT_PATH_B = "output/B7-";
	private static final String OUTPUT_PATH_C = "output/C7-";
	private static final Logger LOG = Logger.getLogger(A7.class.getName());
	private static int compt = 0;

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

	//Comparateur (tri décroissant)
	@SuppressWarnings("rawtypes")
	class Comparator<T extends WritableComparable> extends WritableComparator {

		public Comparator(Class<T> parameterClass) {
			super(parameterClass, true);
		}
		@Override
		public int compare(WritableComparable a, WritableComparable b) {
			return (int) (Double.parseDouble(b.toString()) - Double.parseDouble(a.toString()));
		}
	}
	class TextComparator extends Comparator<Text> {
		public TextComparator() {
			super(Text.class);
		}
	}
	
	public static ArrayList<String> partitionnement(String key, int taille){
		ArrayList<String> tab = new ArrayList<String>();
		for (int i = 1; i <= taille; i++) {
			tab.add(key + String.valueOf(i));
		}
		return tab;
	}
	
	// Mapper A
	/*
		Acteur : emit(ida, gender)
		Role : emit (idr, nom)
		Cachets : emit(ida+idr, montant)
	*/
	public static class Map extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };

		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); //pour chaque ligne appel a map
			String[] words = line.split(",");  // tableau de mots
			compt++;
			
			if (Arrays.equals(words, emptyWords))
				return;

			if (compt != 1) {
				if (words[0].charAt(0) == 'e') { // Acteur
					String Act_ida = words[0];
					String Act_gender = words[3];
					for (String rol : partitionnement("m", 10)) {
//						LOG.info(Act_ida+" "+rol+" "+Act_gender);
						context.write(new Text(Act_ida+" "+rol), new Text("Act"+Act_gender));
					}
				} else if (words[0].charAt(0) == 'm') { // Role
//					LOG.info("test: "+words[0]+","+words[1]);
					String Rol_idr = words[0];
					String Rol_nom = words[1];
					for (String act : partitionnement("e", 500)) {
//						LOG.info(act+" "+Rol_idr+" "+Rol_nom);
						context.write(new Text(act+" "+Rol_idr), new Text("Rol"+Rol_nom));
					}
				} else if (words[0].charAt(0) == 's') { // Cachets
					String Cac_ida = words[1];
					String Cac_idr = words[2];
					String Cac_montant = words[4];
//					LOG.info(Cac_ida+" "+Cac_idr+" "+Cac_montant);
					context.write(new Text('e'+Cac_ida+" "+'m'+Cac_idr), new Text("Cac"+Cac_montant));
				}
			}
		}
	}

	// Reducer A
	// emit(ida+idr, gender+montant+nom)
	public static class Reduce extends Reducer<Text, Text, Text, Text> {
		public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
			
			ArrayList<String> values_copy = new ArrayList<String>();
			for (Text val : values) {	
				values_copy.add(val.toString());
			}
			
			for (String act : values_copy) {
				if (act.startsWith("Act")) {
					act = act.substring(3); // gender
					for (String rol : values_copy) {
						if (rol.startsWith("Rol")) {
							rol = rol.substring(3); // nom
							for (String cac : values_copy) {
								if (cac.startsWith("Cac")) {
									cac = cac.substring(3); // montant
									context.write(key, new Text(";"+act+";"+cac+";"+rol));
								}
							}
						}
					}
				}
			}			
		}
	}
	
	// Mapper B
	// emit(nom+gender, montant) 
	public static class MapB extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };

		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); //pour chaque ligne appel a map
			String[] words = line.split(";");  // tableau de mots
			
			if (Arrays.equals(words, emptyWords))
				return;

			String Rol_nom = words[3];
			String Act_gender = words[1];
			String Cac_montant = words[2];
			context.write(new Text(Rol_nom+" "+Act_gender), new Text(Cac_montant));
		}
	}
	
	// Reducer B
	// emit(nom+gender, moyenne(montant)+nb_acteur)
	public static class ReduceB extends Reducer<Text, Text, Text, Text> {
		
		public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
			Double sum = 0.0;
			int count = 0;
			ArrayList<String> values_copy = new ArrayList<String>();
			
			for (Text val : values) {	
				values_copy.add(val.toString());
			}
			
			for (String val : values_copy) {
				count++;
				sum += Double.valueOf(val);
			}
//			LOG.info(key+";"+sum/count+";"+count);
			context.write(key, new Text(";"+sum/count+";"+count));
		}
	}
	
	// Mapper C
	// emit(gender, nom+moyenne(montant)+nb_acteur)
	public static class MapC extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };

		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); //pour chaque ligne appel a map
			String[] words = line.split(";");  // tableau de mots
			String nom = words[0].split("\\s")[0];
			String gender = words[0].split("\\s")[1];
			String avg = words[1];
			String nb_acteur = words[2];
			
			if (Arrays.equals(words, emptyWords))
				return;

//			LOG.info(gender+nom+" "+value);
			context.write(new Text(gender), new Text(nom+";"+avg+";"+nb_acteur));
		}
	}
	
	// Reducer C
	// emit(gender, nom+moyenne(montant)+nb_acteur)
	// Le nombre maximum d'acteurs par rapport au genre
	public static class ReduceC extends Reducer<Text, Text, Text, Text> {
		
		public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
			int max = 0;
			String nom = null;
			double avg = 0;
			int nb_acteur = 0;
			
			for (Text val : values) {
				String[] words = val.toString().split(";");
				nb_acteur = Integer.valueOf(words[2]);
				
				if (nb_acteur > max) {
					max = nb_acteur;
					nom = words[0];
					avg = Double.valueOf(words[1]);
				}
			}
			context.write(key, new Text(nom+" "+avg+" "+max));
		}
	}
	
	
	// Main
	public static void main(String[] args) throws Exception {
		Configuration conf = new Configuration();
		conf.set("fs.file.impl", "com.conga.services.hadoop.patch.HADOOP_7682.WinLocalFileSystem");

		// Job A
		Job job = new Job(conf, "A");
		
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(Text.class);

		job.setMapperClass(Map.class);
		job.setReducerClass(Reduce.class);

		job.setOutputValueClass(Text.class); 
		
		job.setInputFormatClass(TextInputFormat.class);
		job.setOutputFormatClass(TextOutputFormat.class);

		long instant = Instant.now().getEpochSecond();
		FileInputFormat.addInputPath(job, new Path(INPUT_PATH));
		FileOutputFormat.setOutputPath(job, new Path(OUTPUT_PATH + instant));

		job.waitForCompletion(true);
		
		// Job B
		Job jobB = new Job(conf, "B");
//		jobB.setSortComparatorClass(TextComparator.class);
		
		jobB.setOutputKeyClass(Text.class);
		jobB.setOutputValueClass(Text.class);

		jobB.setMapperClass(MapB.class);
		jobB.setReducerClass(ReduceB.class);

		jobB.setOutputValueClass(Text.class); 
		
		jobB.setInputFormatClass(TextInputFormat.class);
		jobB.setOutputFormatClass(TextOutputFormat.class);

		FileInputFormat.addInputPath(jobB, new Path(OUTPUT_PATH + instant));
		FileOutputFormat.setOutputPath(jobB, new Path(OUTPUT_PATH_B + instant));

		jobB.waitForCompletion(true);
		
		// Job C
		Job jobC = new Job(conf, "C");
		
		jobC.setOutputKeyClass(Text.class);
		jobC.setOutputValueClass(Text.class);

		jobC.setMapperClass(MapC.class);
		jobC.setReducerClass(ReduceC.class);

		jobC.setOutputValueClass(Text.class); 
		
		jobC.setInputFormatClass(TextInputFormat.class);
		jobC.setOutputFormatClass(TextOutputFormat.class);

		FileInputFormat.addInputPath(jobC, new Path(OUTPUT_PATH_B + instant));
		FileOutputFormat.setOutputPath(jobC, new Path(OUTPUT_PATH_C + instant));

		jobC.waitForCompletion(true);
	}
}
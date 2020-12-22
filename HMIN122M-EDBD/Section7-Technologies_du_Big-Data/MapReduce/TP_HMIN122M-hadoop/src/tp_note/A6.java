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
//import org.apache.hadoop.mapreduce.Reducer.Context;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;

//import tp_note.A7.MapC;
//import tp_note.A7.ReduceC;

/* 6. Quels sont les comédiens qui ont eu le plus gros cachet en février ? (montant, name, first_name, last_name, age, nationality)}

Mapper A : 
- Actor : emit(ida+idm, first_name+last_name+age+nationality)
- Stamps : emit((ida+idm, montant) 
- Month : emit(ida+idm, name) filtrer par rapport à février
Reducer A : Jointue par hachage (triple boucle for)
- emit(ida+idm, montant+name+first_name+last_name+age+nationality)

Mapper B : 
- emit(name+first_name+last_name+age+nationality, montant)
Reducer B : Parcours et fait la somme de montant pour chaque groupe
- emit(name+first_name+last_name+age+nationality, SUM(montant))
 */

//Comparateur (tri décroissant)
@SuppressWarnings("rawtypes")
class ComparatorA6<T extends WritableComparable> extends WritableComparator {

	public ComparatorA6(Class<T> parameterClass) {
		super(parameterClass, true);
	}
	@Override
	public int compare(WritableComparable a, WritableComparable b) {
		return (int) (Double.parseDouble(b.toString()) - Double.parseDouble(a.toString()));
	}
}
class TextComparatorA6 extends ComparatorA6<Text> {
	public TextComparatorA6() {
		super(Text.class);
	}
}

// Main class
public class A6 {
	private static final String INPUT_PATH = "input-disneyland/";
	private static final String OUTPUT_PATH = "output/A6-";
	private static final String OUTPUT_PATH_B = "output/B6-";
	private static final String OUTPUT_PATH_C = "output/C7-";
	private static final Logger LOG = Logger.getLogger(A6.class.getName());
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

	public static ArrayList<String> partitionnement(String key, int taille){
		ArrayList<String> tab = new ArrayList<String>();
		for (int i = 1; i <= taille; i++) {
			tab.add(key + String.valueOf(i));
		}
		return tab;
	}
	
	// Mapper A
	/*
		- Actor : emit(idact+idmon, first_name+last_name+age+nationality)
		- Month : emit(idact+idmon, name) filtrer par rapport à février
		- Stamps : emit(idact+idmon, montant) 
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
				if (words[0].charAt(0) == 'e') { // Actor
					String Act_id = words[0];
					String Act_first_name = words[1];
					String Act_last_name = words[2];
					String Act_age = words[4];
					String Act_nationality = words[5];
					for (String mon : partitionnement("n", 12)) { // Month
						context.write(new Text(Act_id+" "+mon), new Text("Act"+Act_first_name+" "+Act_last_name+" "+Act_age+" "+Act_nationality));
					}
				} else if (words[0].charAt(0) == 'n') { // Month
					String Mon_id = words[0];
					String Mon_name = words[1];
					if (Mon_name.equals("fevrier")) {
						for (String act : partitionnement("e", 500)) { // Actor
							context.write(new Text(act+" "+Mon_id), new Text("Mon"));
						}
					}
				} else if (words[0].charAt(0) == 's') { // Stamps
					String Sta_id_act = words[1]; 
					String Sta_id_mon = words[3]; 
					String Sta_montant = words[4];
					context.write(new Text('e'+Sta_id_act+" "+'n'+Sta_id_mon), new Text("Sta"+Sta_montant));
				}
			}
		}
	}

	// Reducer A : Jointue par hachage (triple boucle for)
	// - emit(ida+idm, montant+name+first_name+last_name+age+nationality)
	public static class Reduce extends Reducer<Text, Text, Text, Text> {
		public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
			
			ArrayList<String> values_copy = new ArrayList<String>();
			for (Text val : values) {	
				values_copy.add(val.toString());
			}
			
			for (String act : values_copy) {
				if (act.startsWith("Act")) {
					act = act.substring(3); // first_name+last_name+age+nationality
					for (String mon : values_copy) {
						if (mon.startsWith("Mon")) {
							mon = mon.substring(3); // name
							for (String sta : values_copy) {
								if (sta.startsWith("Sta")) {
									sta = sta.substring(3); // montant
									context.write(key, new Text(";"+sta+";"+mon+";"+act));
								}
							}
						}
					}
				}
			}			
		}
	}
	
	// Mapper B : (ce que je reçois : ida+idm, montant+name+first_name last_name age nationality)
	// - emit(first_name+lasy_name+age+nationality, montant)
	public static class MapB extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };

		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); //pour chaque ligne appel a map
			String[] words = line.split(";");  // tableau de mots
			
			if (Arrays.equals(words, emptyWords))
				return;

			String Sta_montant = words[1];
			String Mon_name = words[2];
			String Act_all = words[3];
			context.write(new Text(Mon_name+" "+Act_all), new Text(Sta_montant));
		}
	}
	
	// Reducer B : Parcours et fait la somme de montant pour chaque groupe
	// - emit(name+first_name+last_name+age+nationality, SUM(montant))
	public static class ReduceB extends Reducer<Text, Text, Text, Text> {
		
		public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
			Double sum = 0.0;
			ArrayList<String> values_copy = new ArrayList<String>();
			
			for (Text val : values) {	
				values_copy.add(val.toString());
			}
			
			for (String val : values_copy) {
				sum += Double.valueOf(val);
			}
			context.write(key, new Text(";"+sum));
		}
	}
	
	// Mapper C : ce que je reçois : name+first_name+last_name+age+nationality, SUM(montant)
	// - emit(SUM(montant), name+first_name+last_name+age+nationality)
	public static class MapC extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };

		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); //pour chaque ligne appel a map
			String[] words = line.split(";");  // tableau de mots
			String Act_all = words[0];
			String sum = words[1];
				
			if (Arrays.equals(words, emptyWords))
				return;

			context.write(new Text(sum), new Text(Act_all));
		}
	}
		
	// Reducer C
	// - emit(SUM(montant), name+first_name+last_name+age+nationality)
	public static class ReduceC extends Reducer<Text, Text, Text, Text> {			
		public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {

			for (Text val : values) {
				context.write(key, val);
			}
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
		jobC.setSortComparatorClass(TextComparatorA6.class);
		
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
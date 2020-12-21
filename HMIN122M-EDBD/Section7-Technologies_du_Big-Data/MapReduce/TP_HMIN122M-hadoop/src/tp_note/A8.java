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

/* Quelles sont les femmes qui on gagner le plus dans l'année ? (montant, prénom, nom, age, pays)
 * 
 * SELECT montant, first_name, last_name, age, pays WHERE Salaire.id_employe = Employe.id_employe AND EMploye.gender = "Female";
 */

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

// Main class
public class A8 {
	private static final String INPUT_PATH = "input-disneyland/";
	private static final String OUTPUT_PATH = "output/A8-";
	private static final String OUTPUT_PATH_B = "output/B8-";
	private static final Logger LOG = Logger.getLogger(A8.class.getName());
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

	// Mapper A
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
				if (words[0].charAt(0) == 's') {
					String Sal_id_employe = words[0].substring(1);
					String Sal_montant = words[4];
					LOG.info(Sal_id_employe+"Sal"+Sal_montant);
					context.write(new Text(Sal_id_employe), new Text("Sal"+Sal_montant));
				} else if (words[0].charAt(0) == 'e' && words[3].equals("Female")) {
					String Emp_id_employe = words[0].substring(1);
					String Emp_first_name = words[1];
					String Emp_last_name = words[2];
					String Emp_age = words[4];
					String Emp_pays = words[5];
					LOG.info(Emp_id_employe+"Emp"+Emp_first_name+";"+Emp_last_name+";"+Emp_age+";"+Emp_pays);
					context.write(new Text(Emp_id_employe), new Text("Emp"+Emp_first_name+";"+Emp_last_name+";"+Emp_age+";"+Emp_pays));
				}
			}
		}
	}			
	// SELECT montant, first_name, last_name, age, pays WHERE Salaire.id_employe = Employe.id_employe AND Employe.gender = "Female" ORDER BY montant;

	// Reducer A
	public static class Reduce extends Reducer<Text, Text, Text, Text> {
		public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
			
			ArrayList<String> values_copy = new ArrayList<String>();
			for (Text val : values) {
				values_copy.add(val.toString());
			}
			
			for (String emp : values_copy) {
				double sum = 0;	
				if (emp.startsWith("Emp")) {
					emp = emp.substring(3);
					for (String sal : values_copy) {
						if (sal.startsWith("Sal")) {
							sal = sal.substring(3);
							sum += Double.parseDouble(sal);
						}
					}
					context.write(new Text(emp), new Text(";" + String.valueOf(sum)));
				}
			}			
		}
	}
	
	// Mapper B
	public static class MapB extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };

		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); //pour chaque ligne appel a map
			String[] words = line.split(";");  // tableau de mots
			
			if (Arrays.equals(words, emptyWords))
				return;

			String Emp_first_name = words[0];
			String Emp_last_name = words[1];
			String Emp_age = words[2];
			String Emp_pays = words[3];
			String gain = words[4];
			context.write(new Text(gain), new Text(Emp_first_name+" "+Emp_last_name+" "+Emp_age+" "+Emp_pays));
		}
	}
	
	// Reducer B
	public static class ReduceB extends Reducer<Text, Text, Text, Text> {
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
		jobB.setSortComparatorClass(TextComparator.class);
		
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
	}
}
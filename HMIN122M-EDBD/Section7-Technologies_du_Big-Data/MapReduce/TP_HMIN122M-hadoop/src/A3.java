import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
//import org.apache.hadoop.io.DoubleWritable;
//import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
//import org.apache.hadoop.mapreduce.Reducer.Context;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;


public class A3 {
	private static final String INPUT_PATH = "input-Disney/";
	private static final String OUTPUT_PATH = "output/A3-";
	private static final String OUTPUT_PATH_B = "output/B3-";
	private static final Logger LOG = Logger.getLogger(A3.class.getName());
	

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
	
	// fonctions
	public int age(String date_naissance, String date_billet) throws ParseException {

		String[] naissance = date_naissance.split("/");
		String[] billet = date_billet.split("/");
	    
	    LocalDate start = LocalDate.of(Integer.parseInt(naissance[2]), Integer.parseInt(naissance[1]), Integer.parseInt(naissance[0])); 
	    LocalDate end = LocalDate.of(Integer.parseInt(billet[2]), Integer.parseInt(billet[1]), Integer.parseInt(billet[0])); 
	    // use for age-calculation: LocalDate.now() 
	    long years = ChronoUnit.YEARS.between(start, end); 
		return (int) years;
	} 
	
	public boolean estMineur(String date_naissance, String date_billet) throws ParseException {
		return this.age(date_naissance, date_billet)<18;
	}
	@SuppressWarnings("deprecation")
	public String mois(String date) throws ParseException {
		Date date1 = new SimpleDateFormat("dd/MM/yyyy").parse(date);
		return String.valueOf(date1.getMonth());
	}
	@SuppressWarnings("deprecation")
	public int mois1(String date) throws ParseException {
		Date date1 = new SimpleDateFormat("dd/MM/yyyy").parse(date);
		return date1.getMonth();
	}
	
	
	public static class Map extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };
		
		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); // pour chaque ligne appel a map
			String[] words = line.split(",");
			
			if (Arrays.equals(words, emptyWords))
				return;
			
			if (words.length == 7) {
				// VISITEUR
				String custkey = words[0];
				String naissance = words[4];
				
				context.write(new Text(custkey.substring(1)), new Text("N" + naissance));
			} else {
				// VENTES
				if(words.length == 4) {
					String custkey = words[0];
					String date = words[3];
					context.write(new Text(custkey), new Text("V" + date));
					LOG.info("V"+date);
				}
			}
		}
	}

	public static class Reduce extends Reducer<Text, Text, LongWritable, Text> {

		@Override
		public void reduce(Text key, Iterable<Text> values, Context context)
				throws IOException, InterruptedException {

			ArrayList<String> values_copy = new ArrayList<String>();
			
			A3 a3 = new A3();
			for (Text val : values) {
				//LOG.info(msg);
				values_copy.add(val.toString());
			}
			for(String a : values_copy) {
				if(a.charAt(0) == 'N') { // [ N_dateNaissance ; V_dateBillet1 ; V_dateBillet2 ; V_dateBillet3 ]
					for(String b : values_copy) {
						if(b.charAt(0) == 'V') {
							try {
								if(a3.estMineur(a.substring(1), b.substring(1)+"20")) {
									context.write(new LongWritable(a3.mois1(b.substring(1)+"20")), new Text("; MINEUR"));
								}else {
									context.write(new LongWritable(a3.mois1(b.substring(1)+"20")), new Text("; MAJEUR"));
								}
							} catch (ParseException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
						}
						
					}
				}

			}
		}
	}
	
	// mois ; MINEUR/MAJEUR -> mois(janv...) min = 10; maj = 10
	
	public static class MapB3 extends Mapper<LongWritable, Text, LongWritable, Text> {
		private final static String emptyWords[] = { "" };

		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); 
			String[] words = line.split("\\;");  
			//compt++;
			
			if (Arrays.equals(words, emptyWords))
				return;

			String mois = words[0];
			String age = words[1].substring(1);
//			String[] p = mois.split(" ");
			
			mois = mois.replaceAll("\\s", "");
			if(age.equals("MAJEUR")) {
				context.write(new LongWritable(Integer.parseInt(mois)), new Text(0 + ";" + 1));
			}else {
				context.write(new LongWritable(Integer.parseInt(mois)), new Text(1 + ";" + 0));
			}
		}
	}
	
	// Reducer B
	public static class ReduceB3 extends Reducer<LongWritable, Text, Text, Text> {
//		private static String max = "";
		private static final String[] mois = {"janvier","fevrier","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","decembre"};
		
		public void reduce(LongWritable key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
			int Sum_majeur = 0;
			int Sum_mineur = 0;
			int majeur;
			int mineur;
			int key_int = Integer.parseInt(key.toString());
			for (Text val : values) {
				
				String[] val_string = val.toString().split(";");
				mineur = Integer.parseInt(val_string[0]);
				majeur = Integer.parseInt(val_string[1]);
				Sum_majeur += majeur;
				Sum_mineur += mineur;
			}
			context.write(new Text(mois[key_int]), new Text("maj = " + Sum_majeur + " min = " + Sum_mineur));	
		}
	}

	public static void main(String[] args) throws Exception {
		
		Configuration conf = new Configuration();
		conf.set("fs.file.impl", "com.conga.services.hadoop.patch.HADOOP_7682.WinLocalFileSystem");
		
		Job job = new Job(conf, "A3");

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

		jobB.setOutputKeyClass(LongWritable.class);
		jobB.setOutputValueClass(Text.class);

		jobB.setMapperClass(MapB3.class);
		jobB.setReducerClass(ReduceB3.class);

		jobB.setOutputValueClass(Text.class); 
						
		jobB.setInputFormatClass(TextInputFormat.class);
		jobB.setOutputFormatClass(TextOutputFormat.class);

		FileInputFormat.addInputPath(jobB, new Path(OUTPUT_PATH + instant));
		FileOutputFormat.setOutputPath(jobB, new Path(OUTPUT_PATH_B + instant));

		jobB.waitForCompletion(true);
	}
}
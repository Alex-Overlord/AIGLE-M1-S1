package tp_note;

import java.io.IOException;
//import java.text.DateFormat;
import java.text.ParseException;
//import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
//import java.util.Date;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
//import org.apache.hadoop.io.DoubleWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
//import org.apache.hadoop.mapreduce.Mapper.Context;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;

/*
 * 2. Quelles est la moyenne d'âge des visiteur restant plus d'un jours ?
 * jointure par hachage ld la classe visiteur et
 * */
public class A2 {
	//private static final String[] billets = {"b1", "b2", "b3", "b4", "b5", "b6","b7","b8","b9","b10","b11","b12","b13","b14"};
	//private static final String[] visiteur = {"v", "1000"};
	private static final String INPUT_PATH = "input-Disney/";
	private static final String OUTPUT_PATH = "output/A2-";
	private static final String OUTPUT_PATH_B = "output/B2-";
	private static final Logger LOG = Logger.getLogger(HashJoin.class.getName());
	
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
	
	public static int age(String date_naissance, String date_billet) throws ParseException {
//		Date date1 = new SimpleDateFormat("dd/MM/yyyy").parse(date_naissance);
//		Date date2 = new SimpleDateFormat("dd/MM/yyyy").parse(date_billet);
		String[] naissance = date_naissance.split("/");
		String[] billet = date_billet.split("/");
	    
	    LocalDate start = LocalDate.of(Integer.parseInt(naissance[2]), Integer.parseInt(naissance[1]), Integer.parseInt(naissance[0])); 
	    LocalDate end = LocalDate.of(Integer.parseInt(billet[2]), Integer.parseInt(billet[1]), Integer.parseInt(billet[0])); 
	    // use for age-calculation: LocalDate.now() 
	    long years = ChronoUnit.YEARS.between(start, end); 
		return (int) years;
	} 
	
	public boolean estMineur(String date_naissance, String date_billet) throws ParseException {
		return A2.age(date_naissance, date_billet)<18;
	}
	
	
	/*--------------------------------------------------------------------*/
	public static class Map extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };
		
		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); // pour chaque ligne appel a map
			String[] words = line.split(","); // tableau de mots
			
			if (Arrays.equals(words, emptyWords))
				return;
			//de la forme (id_v id_b) 
			if (words[0].charAt(0) == 'b') {
				// billet
				String custkey = words[0];
				//String nom = words[1];
				String jour = words[2];
				for (String a : partitionnement("v", 1000)) {
					context.write(new Text(a + " " + custkey), new Text("b" + jour ));
				}
				
			} else if(words[0].charAt(0) == 'v'){
				// visiteur
				String custkey = words[0];
				//String name = words[1];
				String naissance = words[4];
				for(String x : partitionnement("b", 14)) {
					context.write(new Text(custkey + " " + x), new Text("v" + naissance));
				}
				}else {
					//ventes
					if(words.length==4 && words[3].contains("/")) {
						String custkey_b = words[1];
						String custkey_v = words[0];
						String date = words[3];
						context.write(new Text("v" + custkey_v + " " + "b" + custkey_b), new Text("f"+date));
					}
				}
		}
	}
	public static class Reduce extends Reducer<Text, Text, Text, Text> {
		public static int n = 0; 
		public static int sum_age = 0;
		@Override
		public void reduce(Text key, Iterable<Text> values, Context context)
				throws IOException, InterruptedException {
			
			ArrayList<String> values_copy = new ArrayList<String>();
			for (Text val : values) {
				values_copy.add(val.toString());
			}		
				for (String a : values_copy) {
					for (String b : values_copy) {
						for(String c : values_copy) {
							if (a.charAt(0) == 'b' && b.charAt(0) == 'v' && c.charAt(0) == 'f') {	
								if(Double.parseDouble(a.substring(1)) > 1 ) {
									try {
										sum_age += age(b.substring(1), c.substring(1)+"20");
										n ++;
										context.write(key, new Text(a.substring(1)+" "+ age(b.substring(1), c.substring(1)+"20")));
									} catch (IOException e) {
										// TODO Auto-generated catch block
										e.printStackTrace();
									} catch (InterruptedException e) {
										// TODO Auto-generated catch block
										e.printStackTrace();
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
	}
	
	public static class Map2 extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };
		
		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); // pour chaque ligne appel a map
			String[] words = line.split("\\s"); // tableau de mots
			
			if (Arrays.equals(words, emptyWords))
				return;
			context.write(new Text("key"), new Text(words[3]) );
		}
	}
	public static class Reduce2 extends Reducer<Text, Text, Text, Text> {
		public static int n = 0; 
		public static int sum_age = 0;
		@Override
		public void reduce(Text key, Iterable<Text> values, Context context)
				throws IOException, InterruptedException {
			
			ArrayList<String> values_copy = new ArrayList<String>();
			for (Text val : values) {
				values_copy.add(val.toString());
			}		
			for (String val : values_copy) {
				n++;
				sum_age += Double.valueOf(val);
			}
			context.write(new Text("Moyenne âge : "), new Text(String.valueOf(sum_age/n)));
			
		}
	}
	
	public static void main(String[] args) throws ParseException, IOException, ClassNotFoundException, InterruptedException {
//		TODO Auto-generated method stub
//		A2 a2 = new A2();
//		//System.out.print(a2.age("23/09/1998", "18/12/2020"));
//		LocalDate start = LocalDate.of(1996, 2, 29);
//		LocalDate end = LocalDate.of(2014, 2, 28); 
//
//		long years = ChronoUnit.YEARS.between(start, end); 
//		
//		String b = "12    ";
//		b = b.replaceAll("\\s", "");
//		System.out.println("|"+b+"|");
//		System.out.println("10.2".contains("."));
//		String[] st = "TARIF_ENFANT r90	b8 107.00".split("\\s");
//		System.out.println("----------");
//		DoubleWritable dw = new DoubleWritable(100);
//		System.out.println(dw.compareTo(new DoubleWritable(Double.parseDouble("-1000"))));
//		for (String s : st) {
//			System.out.println("|"+s+"|");
//		}
		Configuration conf = new Configuration();
		conf.set("fs.file.impl", "com.conga.services.hadoop.patch.HADOOP_7682.WinLocalFileSystem");
		
		Job job = new Job(conf, "HashJoin");

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

		jobB.setMapperClass(Map2.class);
		jobB.setReducerClass(Reduce2.class);

		jobB.setOutputValueClass(Text.class); 
						
		jobB.setInputFormatClass(TextInputFormat.class);
		jobB.setOutputFormatClass(TextOutputFormat.class);

		FileInputFormat.addInputPath(jobB, new Path(OUTPUT_PATH + instant));
		FileOutputFormat.setOutputPath(jobB, new Path(OUTPUT_PATH_B + instant));

		jobB.waitForCompletion(true);
	}
	
	

}

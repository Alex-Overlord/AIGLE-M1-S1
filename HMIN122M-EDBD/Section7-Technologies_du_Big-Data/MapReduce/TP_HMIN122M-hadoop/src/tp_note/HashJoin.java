package tp_note;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
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


public class HashJoin {
	private static final String INPUT_PATH = "input-HashJoin/";
	private static final String OUTPUT_PATH = "output/HashJoin-";
	private static final Logger LOG = Logger.getLogger(HashJoin.class.getName());
	private static final String[] client = {"c1", "c2", "c3"};
	private static final String[] produit = {"p1", "p2" , "p3" , "p4"};
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
	
	public static class Map extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };
		
		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); // pour chaque ligne appel a map
			String[] words = line.split(","); // tableau de mots
			
			if (Arrays.equals(words, emptyWords))
				return;
			// idc idp
			if (words[0].charAt(0) == 'c' && words[1].charAt(0) == 'p') {
				// achat
				String custkey = words[0];//id_cliet
				String nom = words[1];//id_produit
				context.write(new Text(custkey +" " + nom), new Text("f"));
			} else if(words[0].charAt(0) == 'p'){
				// produit
				String custkey = words[0];
				String name = words[1];
				for (String a : client) {
					context.write(new Text(a + " " + custkey), new Text("p"+name));
				}
				
				}else {
					//client
					if(words[0].charAt(0) == 'c') {
						String custkey = words[0];
						String name = words[1];
						for (String a : produit) {
							context.write(new Text(custkey + " " + a), new Text("c"+name));
						}
					}
					
				}
		}
	}
	
	public static class Reduce extends Reducer<Text, Text, Text, Text> {
		public static int comp_key = 0; 
		@Override
		public void reduce(Text key, Iterable<Text> values, Context context)
				throws IOException, InterruptedException {
			
			ArrayList<String> values_copy = new ArrayList<String>();
			for (Text val : values) {
				values_copy.add(val.toString());
				LOG.info(val.toString());
			}
				for (String a : values_copy) {
					for (String b : values_copy) {
						for(String c : values_copy) {
							if (a.charAt(0) == 'c' && b.charAt(0) == 'p' && c.charAt(0) == 'f') {				
								context.write(key, new Text(a.substring(1)+" "+b.substring(1)));
								
							}
						}
					}	
				}
		}
	}
	
	public static void main(String[] args) throws IOException, ClassNotFoundException, InterruptedException {
		// TODO Auto-generated method stub
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

		FileInputFormat.addInputPath(job, new Path(INPUT_PATH));
		FileOutputFormat.setOutputPath(job, new Path(OUTPUT_PATH + Instant.now().getEpochSecond()));

		job.waitForCompletion(true);
	}

}

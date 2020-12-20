import java.io.IOException;
import java.time.Instant;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.WritableComparable;
import org.apache.hadoop.io.WritableComparator;
import org.apache.hadoop.io.WritableComparable;
import org.apache.hadoop.io.WritableComparator;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;

//@SuppressWarnings("rawtypes")
class ComparatorA<T extends WritableComparable> extends WritableComparator {
	private final Logger LOG = Logger.getLogger(A1.class.getName());

	public ComparatorA(Class<T> parameterClass) {
		super(parameterClass, true);
	}


	public int compare(WritableComparable a, WritableComparable b) {		
		String[] ac = a.toString().split(",");
		String[] bc = b.toString().split(",");
		LOG.info(b+" - "+a);
		return (int) (Double.parseDouble(b.toString()) - Double.parseDouble(a.toString()));
	}
}


class ComparatorA1 extends ComparatorA<Text> {
	public ComparatorA1() {
		super(Text.class);
	}
}

public class A1 {
	private static final String INPUT_PATH = "input-Disney/";
	private static final String OUTPUT_PATH = "output/A1-";
	private static final String OUTPUT_PATH_B = "output/B1-";
	private static final Logger LOG = Logger.getLogger(Join.class.getName());
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
	
//  -- Indices du Moodle --
	
	public static class Map extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };
		
		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); 
			String[] words = line.split(","); 
			
			if (Arrays.equals(words, emptyWords))
				return;
			compt ++;
			if (compt != 1) {
				if (words.length == 4) {
				// VENTES
					String custkey = words[1];
					//String comment = words[8];
					LOG.info("V = " + custkey + " 1" );
					context.write(new Text(custkey), new Text("1"));
				} else {
					// BILLET
					if(words[0].charAt(0) == 'b') {
						String custkey = words[0].substring(1);
						String name = words[1];
						LOG.info("B = " + custkey + " " + name);
						context.write(new Text(custkey), new Text("T"+name));
					}
				}
		
				}
			}
	}

	public static class Reduce extends Reducer<Text, Text, Text, Text> {

		@Override
		public void reduce(Text key, Iterable<Text> values, Context context)
				throws IOException, InterruptedException {
			//ArayList <String> lst=new ArrayList <String>();  
			ArrayList<String> values_copy = new ArrayList<String>();
			LOG.info("--------");
			for (Text val : values) {
				values_copy.add(val.toString());
				LOG.info("val = " + val);
			}

			
			for (String a : values_copy) {
				//LOG.info("a = " + a);
				double sum = 0.0;
				for (String b : values_copy) {
					//LOG.info("b = " + b.charAt(0));
					if (a.charAt(0)=='T' && b.charAt(0) == '1' ) {					
						//String result = a.toString() + " " + b.toString();
						a.substring(1);
						sum += 1.0;
						//LOG.info("boucle if");
						//context.write(key, new Text(a + " " + sum));
					}

				}
				if(a.charAt(0)=='T') {
					context.write(key , new Text("," + a.substring(1)+","+Double.toString(sum)));
				}
				
	            
	        }
		}
	}

	
	public static class MapB extends Mapper<LongWritable, Text, Text, Text> {
		private final static String emptyWords[] = { "" };

		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			String line = value.toString(); 
			String[] words = line.split(",");  
			compt++;
			
			if (Arrays.equals(words, emptyWords))
				return;

			if (compt != 1) {
				String id = words[0];
				String name = words[1];
				String sum = words[2];
				context.write(new Text(sum), new Text(id + " " + name));
			}
		}
	}
	
	// Reducer B
	public static class ReduceB extends Reducer<Text, Text, Text, Text> {
		private static String max = "";
		public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
			
			for (Text val : values) {
				if (max.equals("")) {
					max = val.toString();
				}
				
				if(max.equals(val.toString())) {
					context.write(val, key);	
					
				}
			}
		}
	}
	
	public static void main(String[] args) throws Exception {		
		Configuration conf = new Configuration();
		conf.set("fs.file.impl", "com.conga.services.hadoop.patch.HADOOP_7682.WinLocalFileSystem");
		
		Job job = new Job(conf, "A1");

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
		jobB.setSortComparatorClass(ComparatorA1.class);
				
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
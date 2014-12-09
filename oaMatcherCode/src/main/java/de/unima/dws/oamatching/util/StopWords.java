package de.unima.dws.oamatching.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.TreeSet;

public class StopWords 
{
	private static StopWords	small	=	null;
	private static StopWords	medium	=	null;
	private static StopWords	full	=	null;
	
	public	TreeSet<String> stopwords;
	
	private StopWords() 
	{
		super();
		stopwords	=	new TreeSet<String>();
	}
	
		
	public TreeSet<String> getStopwords() {
		return stopwords;
	}

	public static StopWords getSmallSet()
	{
		if(small == null)
		{
			small	=	new StopWords();
			
			
			String[] words	=	{"a", "about", "above" ,"an", "are", "as", "at",
					 			 "be", "by", "both",
					 			 "for","from",
					 			 "has","have","her","his",
					 			 "in","into","is",
					 			 "like",
					 			 "near",
					 			 "of","off","on","onto","or","over",
					 			 "per",
					 			 "since","so",
					 			 "than","that","to","the","then","this","through",
					 			 "un","until","up",
					 			 "versus","via",
					 			 "with","within","without"};

			for(String item : words)
			{
				small.stopwords.add(item);
			}
			
		}
		
		return small;
	}
	
	public static StopWords getMediumSet()
	{
		if(medium == null)
		{
			medium	=	new StopWords();
			
			String	stops	=	"a,able,about,across,after,all,almost,also,am,among,an," +
								"and,any,are,as,at,be,because,been,but,by,can,cannot," +
								"could,dear,did,do,does,either,else,ever,every,for,from," +
								"get,got,had,has,have,he,her,hers,him,his,how,however," +
								"i,if,in,into,is,it,its,just,least,let,like,likely," +
								"may,me,might,most,must,my,neither,no,nor,not,of,off," +
								"often,on,only,or,other,our,own,rather,said,say,says," +
								"she,should,since,so,some,than,that,the,their,them,then," +
								"there,these,they,this,tis,to,too,twas,us,wants,was,we,were," +
								"what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";
			
			String[] words	=	{"a", "about", "above" ,"an", "are", "as", "at",
					 			 "be", "by", "both",
					 			 "for","from",
					 			 "has","have","her","his",
					 			 "in","into","is",
					 			 "like",
					 			 "near",
					 			 "of","off","on","onto","or","over",
					 			 "per",
					 			 "since","so",
					 			 "than","that","to","the","then","this","through",
					 			 "un","until","up",
					 			 "versus","via",
					 			 "with","within","without"};

			for(String item : words)
			{
				medium.stopwords.add(item);
			}
			
			for(String item : stops.split("[,]"))
			{
				medium.stopwords.add(item);
			}
			
		}
		
		return medium;
	}
	
	public static StopWords getFullSet()
	{
		if(full == null)
		{
			full	=	new StopWords();
			
			try 
			{
				BufferedReader	bfr	=	new BufferedReader(new FileReader("configs" + File.separatorChar + "stopwords.dat"));
				
				String	line	=	null;
				
				while ((line = bfr.readLine()) != null) 
				{
					if(line.trim().length() == 0 || line.trim().startsWith("#"))
					{
						continue;
					}
					
					String[] tokens	=	line.split(",");
					
					for(String token : tokens)
					{
						full.stopwords.add(token.toLowerCase());
					}
				}
			} 
			catch (Exception e) 
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		return full;
	}
	
	public boolean contains(String word)
	{
		return stopwords.contains(word.toLowerCase());
	}
	
}

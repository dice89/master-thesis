/**
 * 
 */
package de.unima.dws.oamatching.util.wordnet;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.sun.javafx.print.PrintHelper;

import de.unima.dws.oamatching.config.Config;
import de.unima.dws.oamatching.util.SimpleSpliter;
import de.unima.dws.oamatching.util.StopWords;


/**
 * Original Class from  YAM++ 2012 OAEI Version
 * @author ngoduyhoa (modified by Alexander C. Mueller)
 * 
 */
public class Supports 
{
	static Logger	logger	=	Logger.getLogger(Supports.class);

	

	// create a property file for initialize wordnet by directory and version
	public static void createWNPropertyFile(String wordnetdir, String wordnetversion) throws Exception
	{
		
		
		BufferedReader	bufRead		=	new BufferedReader(new FileReader(Config.WNTMP()));
		BufferedWriter	bufWrite	=	new BufferedWriter(new FileWriter(Config.WNPROP()));
		
		String	line	=	null;
		
		while((line = bufRead.readLine()) != null)
		{
			if(line.startsWith("INSERT"))
			{
				int	ind	=	line.indexOf("YOUR_WN_Version");
				
				if(ind > 0)
				{
					String	part1	=	line.substring(6, ind);
					String	part2	=	line.substring(ind + 15);
					
					line	=	part1 + wordnetversion + part2;
				}
				else 
				{
					ind	=	line.indexOf("YOUR_WN_Path");
					
					if(ind > 0)
					{
						String	part1	=	line.substring(6, ind);
						String	part2	=	line.substring(ind + 12);
						
						line	=	part1 + wordnetdir + part2;
					}
				}
			}
			
			bufWrite.write(line + "\n");
			
		}
		
		bufRead.close();
		bufWrite.flush();
		bufWrite.close();
	}
	
	public static String createWNPropertyString(String wordnetdir, String wordnetversion) throws Exception
	{
		BufferedReader	bufRead		=	new BufferedReader(new FileReader(Config.WNTMP()));
		
		StringWriter	swriter		=	new StringWriter();
		BufferedWriter	bufWrite	=	new BufferedWriter(swriter);
		
		String	line	=	null;
		
		while((line = bufRead.readLine()) != null)
		{
			if(line.startsWith("INSERT"))
			{
				int	ind	=	line.indexOf("YOUR_WN_Version");
				
				if(ind > 0)
				{
					String	part1	=	line.substring(6, ind);
					String	part2	=	line.substring(ind + 15);
					
					line	=	part1 + wordnetversion + part2;
				}
				else 
				{
					ind	=	line.indexOf("YOUR_WN_Path");
					
					if(ind > 0)
					{
						String	part1	=	line.substring(6, ind);
						String	part2	=	line.substring(ind + 12);
						
						line	=	part1 + wordnetdir + part2;
					}
				}
			}
			
			bufWrite.write(line + "\n");
			
		}
		
		bufRead.close();
		bufWrite.flush();
		bufWrite.close();
		
		return swriter.toString();
	}
	
	/*public static String getMappingType(int type)
	{
		if(type == Configs.FALSE_POSITIVE)
			return "FP";
		else if(type == Configs.TRUE_POSITIVE)
			return "TP";
		else if(type == Configs.FALSE_NEGATIVE)
			return "FN";
		else if(type == Configs.UNKNOWN)
			return "UN";
		else 
		{
			if(type % Configs.MARKED == 0)
			{
				int	k	=	type / Configs.MARKED;
				return ""+ k + "xMARK";
			}
			else
				return "UN";
		}	
	}*/
	
	/*public static String getEntityType(int type)
	{
		if(type == Configs.E_CLASS)
			return "CLASS";
		else if(type == Configs.E_OBJPROP)
			return "OBJ_PROP";
		else if(type == Configs.E_DATAPROP)
			return "DATA_PROP";
		
		return "UNKNOWN";
	}*/
	
	public static String getBaseName(String fullname)
	{
		int	ind	=	fullname.lastIndexOf(File.separatorChar);
		
		return fullname.substring(ind +1);
	}
	
	public static String getLocalName(String uri)
	{
		int	ind	=	uri.lastIndexOf('#');
		
		if(ind == -1)
		{
			ind	=	uri.lastIndexOf('/');
			if(ind == -1)
				return uri;
		}
		
		return uri.substring(ind+1);
	}
	
	public static String getPrefix(String uri)
	{
		int	ind	=	uri.lastIndexOf('#');
		
		if(ind == -1)
		{
			ind	=	uri.lastIndexOf('/');
			if(ind == -1)
				return "";
		}
		
		return uri.substring(0,ind+1);
	}
	
	public static String getNS(String uri)
	{
		int	ind	=	uri.lastIndexOf('#');
		
		if(ind == -1)
		{
			ind	=	uri.lastIndexOf('/');
			if(ind == -1)
				return "";
		}
		
		return uri.substring(0,ind);
	}
	
	// check if uri has not a Name space
	public static boolean isNoNS(String uri)
	{
		int	ind	=	uri.indexOf('#');
		
		if(ind == -1)
			ind	=	uri.indexOf('/');
		
		if(ind == -1)
			return true;
		
		return false;
	}
	
	// remove whitespace and special symbol in string
	// special chars are not letter or digit
	public static String removeSpecialChars(String str)
	{
		StringBuffer	buf	=	new StringBuffer();
		
		for(int i = 0; i < str.length(); i++)
		{
			if(Character.isLetterOrDigit(str.charAt(i)))
				buf.append(str.charAt(i));
		}
		
		return buf.toString();
	}
	
	// remove whitespace, digit and special symbol in string
	// special chars are not letter 
	public static String removeNonLetterChars(String str)
	{
		StringBuffer	buf	=	new StringBuffer();
		
		for(int i = 0; i < str.length(); i++)
		{
			if(Character.isLetter(str.charAt(i)))
				buf.append(str.charAt(i));
		}
		
		return buf.toString();
	}
	
	public static String replaceNonLetterByBlank(String str)
	{
		StringBuffer	buf	=	new StringBuffer();
		
		for(int i = 0; i < str.length(); i++)
		{
			if(Character.isLetter(str.charAt(i)))
				buf.append(str.charAt(i));
			else
			{
				if(buf.charAt(buf.length()-1) != ' ')
					buf.append(" ");
				else
					continue;
			}
		}
		
		return buf.toString().trim();
	}
	
	// insert delimiter (whitespace) between tokens of input string
	public static String insertDelimiter(String str)
	{
		StringBuffer	buf	=	new StringBuffer();
		
		// split input string to tokens (without number)
		String[]	tokens	=	SimpleSpliter.split(str, false);
		
		// put tokens in buffer with delimiter
		for(String token : tokens)
		{
			if(token.charAt(0) == '*')
				token	=	token.substring(1);
			
			if(token.length() > 0)
			{
				buf.append(token);
				buf.append(' ');
			}				
		}
		
		return buf.toString().trim();
	}
	

	public static String getEntityLabelFromURI(String uriStr)
	{
		if (uriStr.indexOf("#")>=0)
			return uriStr.split("#")[1];
		return uriStr;
	}
	
	
	public static String[] splitStringByCapitalLetter(String str)
	{		
		String pattern =

	        "(?<=[^\\p{Upper}])(?=\\p{Upper})"
	        // either there is anything that is not an uppercase character
	        // followed by an uppercase character

	        + "|(?<=[\\p{Lower}])(?=\\d)"
	        // or there is a lowercase character followed by a digit

	        //+ "|(?=\\d)(?<=[\\p{Lower}])"
	        ;

		return str.split(pattern);
		//return str.split("(?=\\p{Upper})");
		
	}
	
	
	public static List<String> cleanLabel(String label_value)
	{		
		List<String> cleanWords=new ArrayList<String>();
		
		String[] words;
				
		label_value=label_value.replace(",", "");
		
		if (label_value.indexOf("_")>0){ //NCI and SNOMED
			words=label_value.split("_");
		}
		else if (label_value.indexOf(" ")>0){ //FMA
			words=label_value.split(" ");
		}
		//Split capitals...
		else{
			words=splitStringByCapitalLetter(label_value);
		}
		//else {
		//	words=new String[1];
		//	words[0]=label_value;
		//}
		
		//To lowercase
		
		
		for (int i=0; i<words.length; i++){
			
			words[i]=words[i].toLowerCase(); //to lower case
			
			if (words[i].length()>0){
			
				if (!StopWords.getSmallSet().contains(words[i])){
				//if (!LexicalUtilities.getStopwordsSetExtended().contains(words[i])){
					//words[i].length()>2 &&  Not for exact IF: it may contain important numbers					
					cleanWords.add(words[i]);
				}				
			}			
		}
		
		
		
		return cleanWords;
		
	}
	
	
	// replace all special characters and remove all token has only 1 char
	public static String replaceSpecialChars(String str)
	{
		String	res	=	str.replaceAll("[^a-zA-Z0-9\\-]", " ");
		
		//System.out.println(res);
		
		//res	=	res.replaceAll(" [a-zA-Z0-9] ", " ");
		
		return res;
	}
	
	// select only main tokens (without number, length >= 3)
	// the remains are concatenate in one token with symbol'*' at the begining, 
	// which means it is not a word 
	public static String[] getWords(String str)
	{
		// get small set of stop words
		StopWords	stopwords	=	StopWords.getSmallSet();
		
		// temporary storage saves all tokens belong to string
		List<String> words	=	new ArrayList<String>();
		
		// split input string to tokens (without number)
		String[]	tokens	=	SimpleSpliter.split(str, false);
		
		// buffer is used for save not-word tokens
		StringBuffer	buf	=	new StringBuffer();
		
		// append symbol '*' at begining
		buf.append('*');
		
		for(String token : tokens)
		{
			// if token is a word in stoplist --> remove it
			if(stopwords.contains(token))
				continue;
			
			if(token.charAt(0) == '*')
			{
				token	=	token.substring(1);
				
				if(stopwords.contains(token))
					continue;
				
				buf.append(token);
			}			
			else
			{
				// if token has all symbol are vowel or consonant --> add in buffer  
				if(isConsonant(token) || isVowel(token))
					buf.append(token);				
				else
					words.add(token.toLowerCase());
			}
		}
		
		// if buffer is not empty (exclude symbol '*')
		if(buf.length() > 1)
			words.add(buf.toString());
		
		// convert array list into array
		return words.toArray(new String[words.size()]);
	}
	
	private	static boolean isVowel(char c)
	{
		if(c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' || c == 'y' ||
		   c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' || c == 'Y')
			return true;
		
		return false;
	}
	
	private	static boolean isVowel(String str)
	{
		for(int i = 0; i < str.length(); i++)
		{
			if(!isVowel(str.charAt(i)))
				return false;
		}
		return true;
	}
	
	private	static boolean isConsonant(char c)
	{
		if(Character.isLetter(c) && !isVowel(c))
			return true;
		
		return false;
	}
	
	private static boolean isConsonant(String str)
	{
		for(int i = 0; i < str.length(); i++)
		{
			if(!isConsonant(str.charAt(i)))
				return false;
		}
		return true;
	}
	
	public static boolean isMarked(String token)
	{
		if(token == null || token.length() == 0)
			return false;
		
		return token.charAt(0) == '*';
	}
	
	public static String unMarked(String token)
	{
		if(token != null && token.length() > 0 &&token.charAt(0) == '*')
			return token.substring(1);
		
		return token;
	}
	
	public static boolean isRandomString(String label)
	{
		String	pattern1	=	"MA_[0-9]{3,}+|NCI_[0-9]{3,}+|[a-zA-Z]+-[0-9]{3,}+-[0-9]{3,}+";
		
		boolean	match	=	false;
		
		match	=	label.matches(pattern1);
		
		return match;
	}
	

	//////////////////////////////////////////////////////////////////////////////
	
	// given string in dictionary file: word1 : synonym1 : synonym2 :
	// extarct it into hashmap<string, arraylist<string>>
	private static HashMap<String, ArrayList<String>> getDictionary(String dictionaryFile)
	{
		HashMap<String, ArrayList<String>> dictionary = new HashMap<String, ArrayList<String>>();
		try {
			BufferedReader br = new BufferedReader(new FileReader(dictionaryFile));
			String line;
			while((line = br.readLine()) != null)
			{
				// replace all character after symbol ':' by enpty ""
				String word = line.replaceAll(":.*","");
				
				// replace all characters from beginning of the line to first ':'  
				// split the remain by character ':'
				String synonyms[] = line.replaceAll("^[^:]+:","").split(":");
				
				dictionary.put(word, new ArrayList<String>(Arrays.asList(synonyms)));
					
				logger.debug("full line : " + line);
				logger.debug("main word : " + word);
				for(String item : synonyms)
				{
					logger.debug("\t synonyms : " + item);
				}
	        } 
		}
		catch (java.io.IOException e) { System.out.println("(E) ThesaurusMatcher.getDictionary - " + e); }
		return dictionary;
	}



	
	// get core name of metric
	// e.x StringMetric[Jaro]  --> return Jaro
	public static String getInternalMetricName(String matcherName)
	{
		int	ind1	=	matcherName.indexOf('[');
		int	ind2	=	matcherName.indexOf(']');
		
		if(ind1 > 0 && ind2 > 0)
			return	matcherName.substring(ind1+1, ind2);
		
		return matcherName;
	}
	
	///////////////////////////////////////////////////////////////////////////////
	
	public static void testGetDictionary()
	{		
		String	dicpath	=	"repos" + File.separatorChar + "test_dic.txt";
		HashMap<String, ArrayList<String>> thesaurus = getDictionary(dicpath);
	}
	
	public static void testReplaceSpecialChars()
	{
		String	str	=	"Ph.D_physic U.S.A.B.C 12-10 {California}";
		
		System.out.println("original : " + str);
		
		System.out.println("replace special chars : " + replaceSpecialChars(str));
		
		System.out.println("replace special chars : " + replaceNonLetterByBlank(str));
	}
	
	
	
	public static void testInsertDelimiter()
	{
		String	str	=	"GoodDD1979ngoDuy>Hoa2810KKKL_-@-1984Vu.ThiMM-nn_THANHMai1210Dd1-*";
		//String	str	=	"ADDRESS";
		System.out.println(insertDelimiter(str));
	}
	
	public static void testGetWords()
	{
		//String	str	=	"GoodDD1979ngoDuy>Hoa2810KKKL_-@-1984Vu.ThiMM-nn_THANHMai1210Dd1-*";
		//String	str	=	"Ph.DStudent";
		//String	str	=	"i.s.b.n";
		//String	str	=	"u.s.a_army";
		String	str	=	"Ph.D_physic.U.S.A1210California";
		//String	str	=	"Ph.D_physic.U.S.A.1210California";
		//String	str	=	"Ph.DThesis_physic.at1210California_U.S.A";
		//String	str	=	"PH.D_THESIS_PHYSIC.U.S.A1210California";
		
		
		for(String token : getWords(str))
		{
			System.out.print(token + " : ");
		}
		
		System.out.println();		
	}
	
	public static void testGetPrefix()
	{
		String	cls_uri1	=	"article";
		String	cls_uri2	=	"http://www.w3.org/2002/12/cal/ical"; 
		String	cls_uri3	=	"http://www.w3.org/1999/02/22-rdf-syntax-ns#List";
				
		if(getPrefix(cls_uri1).equals(""))
			System.out.println("cls_uri1 does not have prefix!!!");
		
		System.out.println("Prefix : " + getPrefix(cls_uri1));
						
		System.out.println("Prefix : " + getPrefix(cls_uri2));
		System.out.println("Prefix : " + getPrefix(cls_uri3));
		System.out.println("Prefix : " + getNS(cls_uri3));
	}

	
	
	public static void testPattern()
	{
		String	label1	=	"c-1283-405";//"NCI_0111";
		
		System.out.println(label1 + " is special : " + isRandomString(label1));
	}
	
	////////////////////////////////////////////////////////////////////////////////
	
	// test static methods above
	public static void main(String[] args) 
	{
		logger.setLevel(Level.DEBUG);
		
		//testInsertDelimiter();
		//testGetWords();
		//testGetLocalName();
		
		//testGetPrefix();
		
		//testPattern();
		
		//testGetScenario();
		
		//testGetDictionary();
		
		testReplaceSpecialChars();
	}
}

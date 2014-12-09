/**
 * 
 */
package de.unima.dws.oamatching.util;

import java.util.ArrayList;
import java.util.List;

import de.unima.dws.oamatching.config.Config;

/**
 * @author ngoduyhoa
 * This tool split a string in tokens.
 * The rules are:
 *  1. split by white space characters
 *  2. split by turning from character to number and vice versa
 *  2. split by turning from lower to upper case and vice versa. The conditions of string are
 *     2.1 if string.length >= 3 (e.g. PhD,  is not split)
 *     2.2    
 */
public class SimpleSpliter 
{
	// split string in tokens
	// e.g. "Ph.D_physic.U.S.A.1210California" --> {"*PhD","physic","*USA","1210","California"}
	// symbol '*' at the start means this token is return from buffer (usually is not a meaning word)  
	public static List<String> splitL(String str, boolean withNumber)
	{
		// temporary storage saves all tokens belong to string
		List<String> tokens	=	new ArrayList<String>();
		
		if(isCapitalString(str))
		{
			tokens.add(str);
			
			return tokens;
		}	
		
		
		// split string in sub sequences
		String[]	seqs	=	addBlank(str).split("\\s+");
		
		// temporary buffer
		StringBuffer	buf	=	new StringBuffer();
		buf.append('*');
		
		// concatenate small substrings (which length < min_len)
		for(String seq : seqs)
		{
			if(isNumber(seq))
			{
				if(withNumber)
				{
					if(buf.length() > 1)
					{
						// add concatenated string in buffer
						tokens.add(buf.toString());
						
						// release buffer
						buf.delete(1, buf.length());						
					}
					
					tokens.add(seq);
				}	
				
				// next sequence
				continue;
			}
						
			if(seq.length() < Config.MIN_LEN())
			{
				buf.append(seq);
			}
			else
			{
				if(buf.length() > 1)
				{
					// add concatenated string in buffer
					tokens.add(buf.toString());
					
					// release buffer
					buf.delete(1, buf.length());					
				}
				
				// add current sequence
				tokens.add(seq);
			}
		}
		
		// if buufer is not empty --> add concatenated string in buffer
		if(buf.length() > 1)
			tokens.add(buf.toString());
		
		return tokens;
	}
	
	public static String[] split(String str, boolean withNumber)
	{
		
		if(isCapitalString(str))
		{
			String[]	arr	=	new String[1];
			arr[0]	=	str;
			
			return arr;
		}
			
		
		// temporary storage saves all tokens belong to string
		List<String> tokens	=	new ArrayList<String>();
		
		// split string in sub sequences
		String[]	seqs	=	addBlank(str).split("\\s+");
		
		// temporary buffer
		StringBuffer	buf	=	new StringBuffer();
		buf.append('*');
		
		// concatenate small substrings (which length < min_len)
		for(String seq : seqs)
		{
			if(isNumber(seq))
			{
				if(withNumber)
				{
					if(buf.length() > 1)
					{
						// add concatenated string in buffer
						tokens.add(buf.toString());
						
						// release buffer
						buf.delete(1, buf.length());						
					}
					
					tokens.add(seq);
				}	
				
				// next sequence
				continue;
			}
						
			if(seq.length() < Config.MIN_LEN())
			{
				buf.append(seq);
			}
			else
			{
				if(buf.length() > 1)
				{
					// add concatenated string in buffer
					tokens.add(buf.toString());
					
					// release buffer
					buf.delete(1, buf.length());					
				}
				
				// add current sequence
				tokens.add(seq);
			}
		}
		
		// if buufer is not empty --> add concatenated string in buffer
		if(buf.length() > 1)
			tokens.add(buf.toString());
		
		// convert array list into array
		return tokens.toArray(new String[tokens.size()]);
	}
	
	// replace special symbol by blank space
	// add blank between letter --> digit, digit --> letter, lower --> upper, upper --> upper.lower
	public static String addBlank(String str)
	{
		// temporary buffer
		StringBuffer	buf	=	new StringBuffer();
		
		for(int ind = 0; ind < str.length(); ind++)
		{			
			if(!Character.isLetterOrDigit(str.charAt(ind)))
			{
				buf.append(' ');
				continue;
			}				
			
			// add current char in buffer
			buf.append(str.charAt(ind));
			
			// check whether it is needed to insert a blank space
			if(isNeededBlank(ind, str))
				buf.append(' ');			
		}
		
		return buf.toString().trim();
	}
	
	private static boolean isNeededBlank(int ind, String str)
	{
		if(letter2digit(ind, str))
			return true;
		else if(digit2letter(ind, str))
			return true;
		else if(lower2upper(ind, str))
			return true;
		else if(upper2lower(ind, str))
			return true;
		
		return false;
	}
	
	private static boolean letter2digit(int ind, String str)
	{
		if(ind < str.length() -1)
		{
			if(Character.isLetter(str.charAt(ind)) && Character.isDigit(str.charAt(ind+1)))
				return true;
		}
		return false;
	}
	
	private static boolean digit2letter(int ind, String str)
	{
		if(ind < str.length() -1)
		{
			if(Character.isDigit(str.charAt(ind)) && Character.isLetter(str.charAt(ind+1)))
				return true;
		}
		return false;
	}
	
	private static boolean lower2upper(int ind, String str)
	{
		if(ind < str.length() -1)
		{
			if(Character.isLowerCase(str.charAt(ind)) && Character.isUpperCase(str.charAt(ind+1)))
				return true;
		}
		return false;
	}
	
	private static boolean upper2lower(int ind, String str)
	{
		if(ind < str.length() -2)
		{
			if(Character.isUpperCase(str.charAt(ind)) &&
			   Character.isUpperCase(str.charAt(ind+1)) && 
			   Character.isLowerCase(str.charAt(ind+2)))
				return true;
		}
		if(ind == str.length() -2)
		{
			if(Character.isUpperCase(str.charAt(ind)) &&
			   Character.isUpperCase(str.charAt(ind+1)))
				return true;
		}
		return false;
	}
	
	// checks whether string is a number
	private static boolean isNumber(String str)
	{
		if(str.length() > 0 && Character.isDigit(str.charAt(0)))
			return true;
		
		return false;
	}	
	
	private static boolean isCapitalString(String str)
	{	
		str	=	str.trim();
		
		if(str.length() == 0)
			return false;
		
		boolean	flag	=	true;
		
		for(int i = 0; i < str.length(); i++)
		{			
			if(Character.isLetter(str.charAt(i)) && Character.isUpperCase(str.charAt(i)))
			{
				continue;
			}
			else
				return false;
		}
		
		return flag;
	}
	
	/////////////////////////////////////////////////////////////////////
	public static List<String> sentenceSplitter(String str)
	{
		List<String> items	=	new ArrayList<String>();
		
		StopWords	stoplist	=	StopWords.getFullSet();
		
		List<String> tokens	=	splitL(str, true);
		
		for(String token : tokens)
		{
			if(token.startsWith("*"))
				token	=	token.substring(1);
			
			if(!stoplist.contains(token))
				items.add(token);
		}
		
		return items;
	}
	
	////////////////////////////////////////////////////////////////////////
	
	public static void main(String[] args) 
	{
		String	str	=	"Proc. Of the 13th Int. Conference on Knowledge Engineering and Management (EKAW-2002)  . Proc. Of the 13th Int. Conference on Knowledge Engineering and Management (EKAW-2002) . Springer-Verlag  . Springer-Verlag .  . DE . Heidelberg .  .  . 13th Int. Conference on Knowledge Engineering and Management (EKAW-2002)  . " +
				"EKAW . 13 . Int. Conference on Knowledge Engineering and Management .  . --10 . 2002 .  .  .  . 2002 .  .  . ";
		
		
		String	str2	=	"Measuring Similarity between Ontologies .";
		
		List<String> items	=	SimpleSpliter.sentenceSplitter(str2);
		
		for(String item : items)
		{
			System.out.println(item);
		}
		
		System.out.println("--------------------------------------------------------");
		System.out.println();
		
		List<String> items2	=	SimpleSpliter.splitL(str, true);
		
		for(String item : items2)
		{
			System.out.println(item);
		}
		
	}
}

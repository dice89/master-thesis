package  de.unima.dws.oamatching.util;
/**
 * 
 */

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import de.unima.dws.oamatching.util.wordnet.Supports;

/**
 * @author ngoduyhoa
 *
 */
public class LabelTokenizer 
{
	int nbToken = 0;

	public LabelTokenizer() {
		
	}
	
	public ArrayList<String> tokenize(String name)
	{		
		ArrayList<String> result = new ArrayList<String>();
		
		if(Supports.isRandomString(name))
		{
			result.add(name);
			
			return result;
		}
		
		//name	=	Supports.replaceSpecialChars(name);
		
		
		ArrayList<Integer> delim = new ArrayList<Integer>();
		int dsz = 0;
		nbToken = 0;
		String regex = "[A-Z]+[0-9]*[A-Z]+|([A-Z]+[a-z]*([-]+[A-Z]*[a-z]*)+)|([A-Z]\\.)+|" 
						+ "[A-Z]+|[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?|\\s|" 
						+ "[,]|[{]|[}]|[:]|[_]";
		Pattern p = Pattern.compile(regex);
		Matcher m = p.matcher(name);
		while(m.find()){
            delim.add(m.start());dsz++;
        }
		delim.add(new Integer(name.length()));dsz++;
		int start = 0;
		for(int i = 0 ; i < dsz ; i++){
			int end = delim.get(i).intValue();
			String tok = name.substring(start,end);
			start = end;
			if (saveToken(tok)){
				tok = tok.replace("_","");
				tok = tok.replace(".","");
				
				//result.add(tok);
				//nbToken++;
				
				ArrayList<String> items	=	tokenProcessing(tok);
				result.addAll(items);
				
				nbToken +=	items.size();
			}	
		}		
		return result;
	}
	
	public boolean saveToken(String tok)
	{
		boolean result = true;
		result = result && (!tok.equalsIgnoreCase(""));
		result = result && (!tok.equalsIgnoreCase(" "));
		result = result && (!tok.equalsIgnoreCase("}"));
		result = result && (!tok.equalsIgnoreCase("{"));
		result = result && (!tok.equalsIgnoreCase(","));
		result = result && (!tok.equalsIgnoreCase(":"));
		result = result && (!tok.equalsIgnoreCase("_"));
		return result;
	}
	
	public ArrayList<String> tokenProcessing(String token)
	{
		ArrayList<String> result = new ArrayList<String>();		
				
		String[] items	=	token.split("[-+]");
		
		StringBuffer	buf	=	new StringBuffer();
		
		for(String item : items)
		{
			/*
			if(item.equalsIgnoreCase("1st"))
				item	=	"first";
			
			if(item.equalsIgnoreCase("2nd"))
				item	=	"second";
			
			
			if(item.equalsIgnoreCase("3rd"))
				item	=	"third";
			*/
			/*			
			if(Character.isDigit(item.charAt(0)))
				continue;
			*/
			if(buf.length() > 3)
			{
				result.add(buf.toString());
				buf.delete(0, buf.length());
			}
			
			buf.append(item);
				
		}
		
		if(buf.length() > 0)
			result.add(buf.toString());
		
		return result;
	}
	
	public ArrayList<String> tokenizePath(String path){
		ArrayList<String> result = new ArrayList<String>();
		nbToken = 0;
		int end = 0;
		int start = 0;
		int parCount = 0;
		int pLen = path.length();
		for(int i = 0 ; i < pLen ; i++){
			char c = path.charAt(i);
			if (c == '('){parCount++;}
			if (c == ')'){parCount--;}
			if (parCount == 0){
				end = i+1;
				String tok = path.substring(start,end);
				tok = tok.replace("(","");
				tok = tok.replace(")","");
				result.add(tok);
				nbToken++;
				start = end;
			}
		}
		return result;
	}
	
	public int getNumberOfTokens(){
		return nbToken;
	}

	//////////////////////////////////////////////////////////////////////////
	
	public static void main(String[] args) 
	{
		// TODO Auto-generated method stub
		
		LabelTokenizer	tokenizer	=	new LabelTokenizer();
		/*
		for(String item : tokenizer.tokenProcessing("co-workshop-participant"))
		{
			System.out.println(item);
		}
		*/
		/*
		String[] labels	=	{"Phd.Student", "co-author","in_collections","unpublished_paper","contribution_1th-author", "hasName",
							"ProgramCommiteeMember","Member_PC", "Contribution_co-author", "invites_co-reviewers",
							"assignedByReviewer", "Name_of_conference", "has_a_track-workshop-tutorial_topic",
							"Early-Registered Participant", "Conference_www", "Track-workshop_chair", "PublishingHouse",
							"PersonList","Roger2009Dictionary","wordnet2.0","U.S.A Student","hasU.S.A---Passprot","becomeEUCitizen","ASIAPeople","10e-2Peson",
							"has_an_email","subject123Area"};
		
		*/
		
		String[] labels	=	{"1st-author"};
		for(String label : labels)
		{
			System.out.println(label);
			for(String item : tokenizer.tokenize(label))
			{
				System.out.println("\t" + item);
			}
			System.out.println("--------------------------------------");
		}
		
	}
}

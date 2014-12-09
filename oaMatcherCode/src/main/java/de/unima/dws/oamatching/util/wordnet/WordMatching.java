/**
 * 
 */
package de.unima.dws.oamatching.util.wordnet;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import net.didion.jwnl.data.IndexWord;
import net.didion.jwnl.data.POS;
import net.didion.jwnl.data.Synset;
import de.unima.dws.oamatching.config.Config;
import de.unima.dws.oamatching.datatypes.wn.LCS;
import fr.inrialpes.exmo.ontosim.string.StringDistances;

/**
 * @author ngoduyhoa
 * Compute similarity score between 2 single words by using Wordnet and Lin algorithm
 * NOTE: we must
 */
public class WordMatching 
{	

	public double getScore(String str1, String str2) {
		// TODO Auto-generated method stub
		return getSimScore(str1, str2);
	}
	
	public static double  getSimScore(String word1, String word2) 
	{		
		// TODO Auto-generated method stub
		word1	=	word1.toLowerCase();
		word2	=	word2.toLowerCase();
				
		if(word1.equalsIgnoreCase(word2))
			return 1.0f;		
		/*
		String	stem1	=	Porter2Stemmer.stem(word1);
		String	stem2	=	Porter2Stemmer.stem(word2);
		
		if(word1.equals(stem2) || word2.equals(stem1) || stem1.equals(stem2))
			return 0.95;
		*/
		// instantiate a WordNethelper
		WordNetHelper	helper	=	WordNetHelper.getInstance();
				
		double 	score	=	0;
		
		try
		{	
			String	vstem1	=	helper.wnstemmer.verbStem(word1);
			if(vstem1 != null)
			{
				String	vstem2	=	helper.wnstemmer.verbStem(word2);
				
				if(vstem2 != null && vstem1.equals(vstem2))
					return 0.95;
			}
			
			
			List<Synset>	list1	=	helper.getLimitSynsetsByPOS(POS.NOUN, word1, Config.SENSE_DEPTH());

			List<Synset>	list2	=	helper.getLimitSynsetsByPOS(POS.NOUN, word2, Config.SENSE_DEPTH());
	
			if(!Collections.disjoint(list1, list2))
				return 1.0;
			
			IndexWord	adjword1	=	helper.getFullIndexWord(POS.ADJECTIVE, word1);
			IndexWord	adjword2	=	helper.getFullIndexWord(POS.ADJECTIVE, word2);
			
			if(helper.getSynonymScore(adjword1, adjword2) == 1)
				return 1.0;			
			
			
			Set<Synset>	set1	=	helper.getRelatedNounSynset(word1);
			Set<Synset> set2	=	helper.getRelatedNounSynset(word2);
			
			if(!Collections.disjoint(list1, set2) || !Collections.disjoint(list2, set1) || !Collections.disjoint(set1, set2))
				return 0.95;
			
			list1.addAll(set1);
			list2.addAll(set2);
			
			if(!list1.isEmpty() && !list2.isEmpty())
			{								
				LCS	lcs	=	helper.getLCS(list1, list2);
				
				if(lcs != null)
		    	{
					// get depth of each synsets
					double 	ic	=	helper.getIC(helper.getSynset(POS.NOUN, lcs.getOffset()));
					double 	ic1	=	helper.getIC(helper.getSynset(POS.NOUN, lcs.getOffset1()));
					double 	ic2	=	helper.getIC(helper.getSynset(POS.NOUN, lcs.getOffset2()));
			    	
			    	// compute by lin
					double 	sim	=	2f * ic / (ic1 + ic2);	
			    	
			    	if(sim > score)
			    		score	=	sim;
		    	}		
			}	
			
		}
		catch (Exception e) {
			// TODO: handle exception
		}	
		
		//fallback
		if(score == 0)
			score	=	(1.0 + StringDistances.smoaDistance(word1, word2))/2;
		
		return score;		
	}
	
	///////////////////////////////////////////////////////////////////////
	
	public static void testLin()
	{
		WordMatching	matcher	=	new WordMatching();
			
		String[]	word1s	=	{"cardiac","finger","finger","toe","toe","fat"};
		
		/*
		String[]	word1s	=	{"teaching","thoracic","location","broken","lost","teaching","anus","sulcus","paper","size","person","teacher","booklet","production","adult_male",
								"male","teacher","man","men","subject area","participant",
								"attendee","participant","member","conference","chairman","trip",
								"location","building","event","contribution","lecturer","lecturer","teacher",
								"id","coursework","performance","topic","trip","title"};
    	*/
		
		String[]	word2s	=	{"heart","hand","digit","foot","digit","adipose"};
		
		/*
		String[]	word2s	=	{"teaches","mammary","address","break","lose","teach","anal","fissure","article","dimension","someone","man","product","folder","instructor",
    							"person","instructor","woman","man","topic","member",
    							"participant","attendee","attendee","congress","chair","excursion",
    							"place","hotel","activity","paper","teacher","professor","professor",
    							"identity","course","presentation","presentation","excursion","event"};
    	 */ 	
    	
    	for(int i = 0; i < word1s.length; i++)
    	{
    		String	word1	=	word1s[i];
    		String	word2	=	word2s[i];
    		    		
    		double 	score1	=	getSimScore(word1, word2);
    		System.out.println("sim.score(" + word1 + "," + word2 + ") = " + score1);
    	}		
	}
	
	public static void main(String[] args) throws Exception
	{
		WordNetHelper.getInstance().initializeWN(Config.WNDIR(), Config.WNVER());
		WordNetHelper.getInstance().initializeIC(Config.WNIC());
		
		long	startTime	=	System.currentTimeMillis();
		System.out.println("START...");
		
		testLin();
		
		long	endTime	=	System.currentTimeMillis();
		System.out.println("Running time = " + (endTime - startTime));
		
		System.out.println("FINISH.");
	}
}

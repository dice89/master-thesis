/**
 * 
 */
package de.unima.dws.oamatching.util.wordnet;

import java.util.List;

import net.didion.jwnl.JWNLException;
import net.didion.jwnl.data.POS;
import net.didion.jwnl.data.Synset;
import de.unima.dws.oamatching.config.Config;
import de.unima.dws.oamatching.datatypes.wn.LCS;
import fr.inrialpes.exmo.ontosim.string.StringDistances;

/**
 * @author ngoduyhoa Compute similarity score between 2 single words by using
 *         Wordnet and Wu-Palmer algorithm NOTE: we must
 */
public class JiangConrath extends LinWordMatching {
	boolean withMorphing;

	public JiangConrath() {
		super();
		withMorphing = true;
	}

	public JiangConrath(boolean withMorphing) {
		super();
		this.withMorphing = withMorphing;
	}
	
	@Override
	protected double getSimScore(String word1, String word2) throws Exception {

		if (word1.equalsIgnoreCase(word2))
			return 1.0f;

		word1 = word1.toLowerCase();
		word2 = word2.toLowerCase();

		// instantiate a WordNethelper
		WordNetHelper helper = null;
		try {
			helper = WordNetHelper.getInstance();
		} catch (Exception e1) {
			e1.printStackTrace();
		}

		float score = 0f;

		for (Object item : POS.getAllPOS()) {
			POS pos = (POS) item;
			try {
				List<Synset> list1 = null;
				List<Synset> list2 = null;

				if (withMorphing) {
					list1 = helper.getLimitSynsetsByPOS(pos, word1,
							Config.SENSE_DEPTH());
					list2 = helper.getLimitSynsetsByPOS(pos, word2,
							Config.SENSE_DEPTH());
				} else {
					list1 = helper.getLimitSynsetsByPOSWithoutMorphing(pos,
							word1, Config.SENSE_DEPTH());
					list2 = helper.getLimitSynsetsByPOSWithoutMorphing(pos,
							word2, Config.SENSE_DEPTH());
				}

				if (pos.equals(POS.NOUN)  || pos.equals(POS.VERB) ) {

					LCS lcs = helper.getLCS(list1, list2);

					if (lcs == null) {
						continue;
					}

					// get depth of each synsets
					float ic = helper.getIC(helper.getSynset(pos,
							lcs.getOffset()));
					float ic1 = helper.getIC(helper.getSynset(pos,
							lcs.getOffset1()));
					float ic2 = helper.getIC(helper.getSynset(pos,
							lcs.getOffset2()));

			
					// compute by jiang conrath
				   	float	sim	=	1/ Math.abs(ic1 + ic2 - 2* ic);	
	
					if (sim > score)
						score = sim;
				} else {
				

					float sim = helper.getSynonymScore(list1, list2);

					if (sim > score)
						score = sim;
				}
			} catch (JWNLException e) {
				e.printStackTrace();
			}
		}
		
		
		//implemented fallback Levensthein
		if(score == 0.0){
			score = 1 -((float) StringDistances.levenshteinDistance(word1, word2));	
		}
		if (score == Float.MAX_VALUE){
			score =1.0f;
		}
	
		double score_d = (double) score;
		
		if(score_d > Double.MAX_VALUE){
			score_d =1.0;
		}
		
		
		return score_d;
	}

	public static void testJiangConrath() {
		JiangConrath matcher = new JiangConrath();

		String[] word1s = { "hand" };// {"cardiac","finger","finger","toe","toe","fat"};

		/*
		 * String[] word1s =
		 * {"teaching","thoracic","location","broken","lost","teaching"
		 * ,"anus","sulcus"
		 * ,"paper","size","person","teacher","booklet","production"
		 * ,"adult_male",
		 * "male","teacher","man","men","subject area","participant",
		 * "attendee","participant","member","conference","chairman","trip",
		 * "location"
		 * ,"building","event","contribution","lecturer","lecturer","teacher",
		 * "id","coursework","performance","topic","trip","title"};
		 */

		String[] word2s = { "foot" };// {"heart","hand","digit","foot","digit","adipose"};

		/*
		 * String[] word2s =
		 * {"teaches","mammary","address","break","lose","teach"
		 * ,"anal","fissure"
		 * ,"article","dimension","someone","man","product","folder"
		 * ,"instructor", "person","instructor","woman","man","topic","member",
		 * "participant","attendee","attendee","congress","chair","excursion",
		 * "place","hotel","activity","paper","teacher","professor","professor",
		 * "identity"
		 * ,"course","presentation","presentation","excursion","event"};
		 */

		for (int i = 0; i < word1s.length; i++) {
			String word1 = word1s[i];
			String word2 = word2s[i];

			double score1 = 0.0;
			try {
				score1 = matcher.getSimScore(word1, word2);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			System.out.println("sim.score(" + word1 + "," + word2 + ") = "
					+ score1);
		}
		
		
		
	}

	public static void main(String args[]) throws Exception {

		WordNetHelper.getInstance()
				.initializeWN(Config.WNDIR(), Config.WNVER());
		WordNetHelper.getInstance().initializeIC(Config.WNIC());

		testJiangConrath();
	}

}

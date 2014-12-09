package de.unima.dws.oamatching.util.wordnet;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import de.unima.dws.oamatching.config.Config;
import net.didion.jwnl.JWNL;
import net.didion.jwnl.JWNLException;
import net.didion.jwnl.data.IndexWord;
import net.didion.jwnl.data.POS;
import net.didion.jwnl.dictionary.Dictionary;
import net.didion.jwnl.dictionary.MorphologicalProcessor;


/**
 * @author ngoduyhoa http://tipsandtricks.runicsoft.com/Other/JavaStemmer.html
 */
public class WordNetStemmer {
	private int MaxWordLength = 50;
	private Dictionary dic;
	private MorphologicalProcessor morph;
	private boolean IsInitialized = false;
	public HashMap<String, String> AllWords = null;

	/**
	 * establishes connection to the WordNet database
	 */
	public WordNetStemmer(Dictionary dic) {
		AllWords = new HashMap<String, String>();

		this.dic = dic;
		this.morph = dic.getMorphologicalProcessor();
		// ((AbstractCachingDictionary)dic).
		// setCacheCapacity (10000);
		IsInitialized = true;
	}

	public void Unload() {
		dic.close();
		Dictionary.uninstall();
		JWNL.shutdown();
	}

	/*
	 * stems a word with wordnet
	 * 
	 * @param word word to stem
	 * 
	 * @return the stemmed word or null if it was not found in WordNet
	 */
	public String StemWordWithWordNet(String word) {
		if (!IsInitialized)
			return word;
		if (word == null)
			return null;
		if (morph == null)
			morph = dic.getMorphologicalProcessor();

		IndexWord w;
		try {
			w = morph.lookupBaseForm(POS.VERB, word);
			if (w != null)
				return w.getLemma().toString();
			w = morph.lookupBaseForm(POS.NOUN, word);
			if (w != null)
				return w.getLemma().toString();
			w = morph.lookupBaseForm(POS.ADJECTIVE, word);
			if (w != null)
				return w.getLemma().toString();
			w = morph.lookupBaseForm(POS.ADVERB, word);
			if (w != null)
				return w.getLemma().toString();
		} catch (JWNLException e) {
		}
		return null;
	}

	public String[] AllStemsWordWithWordNet(String word) {
		List<String> stems = new ArrayList<String>();

		if (!IsInitialized)
			stems.add(word);
		else {
			if (word == null)
				return null;
			if (morph == null)
				morph = dic.getMorphologicalProcessor();

			List<IndexWord> ws;
			try {
				ws = morph.lookupAllBaseForms(POS.NOUN, word);
			} catch (JWNLException e) {
			}
		}

		return stems.toArray(new String[stems.size()]);
	}
	
	public String verbStem(String word)
	{
		if (!IsInitialized)
			return word;
		if (word == null)
			return null;
		if (morph == null)
			morph = dic.getMorphologicalProcessor();
		
		IndexWord w = null;
		try {
			w = morph.lookupBaseForm(POS.VERB, word);
		} catch (JWNLException e) {
			// TODO Auto-generated catch block
			return null;
		}
		if (w != null)
			return w.getLemma().toString();
		
		return null;
	}


	/**
	 * Stem a single word tries to look up the word in the AllWords HashMap If
	 * the word is not found it is stemmed with WordNet and put into AllWords
	 * 
	 * @param word
	 *            word to be stemmed
	 * @return stemmed word
	 */
	public String Stem(String word) {
		// check if we already know the word
		String stemmedword = AllWords.get(word);
		if (stemmedword != null)
			return stemmedword; // return it if we already know it

		// don't check words with digits in them
		if (containsNumbers(word) == true)
			stemmedword = null;
		else
			// unknown word: try to stem it
			stemmedword = StemWordWithWordNet(word);

		if (stemmedword != null) {
			// word was recognized and stemmed with wordnet:
			// add it to hashmap and return the stemmed word
			AllWords.put(word, stemmedword);
			return stemmedword;
		}
		// word could not be stemmed by wordnet,
		// thus it is no correct english word
		// just add it to the list of known words so
		// we won't have to look it up again
		AllWords.put(word, word);
		return word;
	}

	/**
	 * performs Stem on each element in the given Vector
	 * 
	 */
	public List<String> Stem(List<String> words) {
		if (!IsInitialized)
			return words;

		for (int i = 0; i < words.size(); i++) {
			words.set(i, Stem((String) words.get(i)));
		}
		return words;
	}

	public boolean containsNumbers(String word) {
		for (int i = 0; i < word.length(); i++)
			if (Character.isDigit(word.charAt(i)))
				return true;

		return false;
	}

	// TEST /////////////////////////////////////////////////////////

	public static void main(String[] args) throws Exception {

		WordNetHelper.getInstance().initializeWN(Config.WNDIR(), Config.WNVER());

		Dictionary dictionary = Dictionary.getInstance();

		WordNetStemmer stemmer = new WordNetStemmer(dictionary);

		String[] words	=	{"broken", "organizations","organizings","teachered","teaching","Persons"};
		
		for(String word : words)
		{
			System.out.println("WNStem of " + word + " is :" + stemmer.Stem(word));
		}	
	}

}

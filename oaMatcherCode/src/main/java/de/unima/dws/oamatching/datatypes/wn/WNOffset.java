package de.unima.dws.oamatching.datatypes.wn;


public class WNOffset 
{
	private	long	offset;
	private	int		depth;
	
	public WNOffset(long synset, int depth) {
		super();
		this.offset = synset;
		this.depth = depth;
	}

	public long getOffset() {
		return offset;
	}

	public int getDepth() {
		return depth;
	}	
}

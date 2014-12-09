package de.unima.dws.oamatching.datatypes.wn;


public class LCS extends WNOffset 
{	
	// first input synset
	private	long	offset1;
	private	int		depth1;
	
	// seocnd iput synset
	private	long	offset2;
	private	int		depth2;
	
	public LCS(long offset, int depth, long offset1, int depth1, long offset2, int depth2)
	{
		super(offset, depth);
		// TODO Auto-generated constructor stub
		
		this.offset1	=	offset1;
		this.depth1		=	depth1;
		
		this.offset2	=	offset2;
		this.depth2		=	depth2;
	}

	public long getOffset1() {
		return offset1;
	}

	public int getDepth1() {
		return depth1;
	}

	public long getOffset2() {
		return offset2;
	}

	public int getDepth2() {
		return depth2;
	}	
}

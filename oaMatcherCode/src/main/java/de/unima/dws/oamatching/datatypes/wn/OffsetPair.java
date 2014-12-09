
package de.unima.dws.oamatching.datatypes.wn;

/**
 * @author ngoduyhoa
 *
 */
public class OffsetPair
{
	// two indexWords
	private	long		offset1;
	private	long		offset2;
	
	
	public OffsetPair(long offset1, long offset2) {
		super();
		this.offset1 = offset1;
		this.offset2 = offset2;			
	}

	public long getOffset1() {
		return offset1;
	}

	public long getOffset2() {
		return offset2;
	}

	

	@Override
	public int hashCode() {
		final int prime = 31;
		
		int result = 1;
		
		long	maxOff	=	Math.max(offset1, offset2);
		long	minOff	=	Math.min(offset1, offset2);
		
		result = prime * result + (int) (maxOff ^ (maxOff >>> 32));
		result = prime * result + (int) (minOff ^ (minOff >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		
		OffsetPair other = (OffsetPair) obj;
		if (offset1 == other.getOffset1() && offset2 == other.getOffset2())
			return true;
		
		if (offset1 == other.getOffset2() && offset2 == other.getOffset1())
			return true;
		
		return false;
	}

	@Override
	public String toString() {
		return "OffsetPair [offset1=" + offset1 + ", offset2=" + offset2 + "]";
	}	
}

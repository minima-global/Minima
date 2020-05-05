package org.minima.database.coindb;

import org.minima.objects.Coin;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public interface CoinDBRow {
	
	public Coin getCoin();
	
	public void setIsSpent(boolean zSpent);
	public boolean isSpent();
	
	public void setInBlockNumber(MiniNumber zInBlockNumber);
	public MiniNumber getInBlockNumber();
	
	public void setIsInBlock(boolean zIn);
	public boolean isInBlock();
	
	public void setMMREntry(MiniInteger zEntry);
	public MiniInteger getMMREntry();
	
	public void setRelevant(boolean zRelevant);
	public boolean isRelevant();
	
	public void setKeeper(boolean zRelevant);
	public boolean isKeeper();
	
	public JSONObject toJSON();
}

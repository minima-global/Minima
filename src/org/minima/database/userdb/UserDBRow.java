package org.minima.database.userdb;

import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.utils.json.JSONObject;

public interface UserDBRow {

	public int getID();
	
	public Witness getWitness();
	public Transaction getTransaction();
		
	public void setTransaction(Transaction zTrans);
	public void setWitness(Witness zWitness);
	
	public JSONObject toJSON();
	
}

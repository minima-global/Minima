package org.minima.database.maxima;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;

public class MaximaDB extends SqlDB {

	/**
	 * PreparedStatements
	 */
	PreparedStatement SQL_INSERT_MAXIMA_HOST 		= null;
	PreparedStatement SQL_INSERT_MAXIMA_CONTACT 	= null;
	
	public MaximaDB() {
		super();
	}
	
	@Override
	protected void createSQL() {
		try {
			
			//Create the various tables..
			Statement stmt = mSQLConnection.createStatement();

//			//Create main table
//			String create = "CREATE TABLE IF NOT EXISTS `syncblock` ("
//							+ "  `id` IDENTITY PRIMARY KEY,"
//							+ "  `txpowid` varchar(80) NOT NULL UNIQUE,"
//							+ "  `block` bigint NOT NULL UNIQUE,"
//							+ "  `timemilli` bigint NOT NULL,"
//							+ "  `syncdata` blob NOT NULL"
//							+ ")";
//			
//			//Run it..
//			stmt.execute(create);
//			
//			//Create some fast indexes..
//			String index = "CREATE INDEX IF NOT EXISTS fastsearch ON syncblock ( txpowid, block )";
//					
//			//Run it..
//			stmt.execute(index);
//			
			
			//All done..
			stmt.close();
//			
//			//Create some prepared statements..
//			String insert 			= "INSERT IGNORE INTO syncblock ( txpowid, block, timemilli, syncdata ) VALUES ( ?, ? ,? ,? )";
//			SQL_INSERT_SYNCBLOCK 	= mSQLConnection.prepareStatement(insert);
//			
//			//Select 
//			SQL_FIND_SYNCBLOCK 		= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE txpowid=?");
//			SQL_EXISTS_SYNCBLOCK	= mSQLConnection.prepareStatement("SELECT block FROM syncblock WHERE txpowid=?");
//			SQL_TOTAL_COUNT			= mSQLConnection.prepareStatement("SELECT COUNT(*) as tot FROM syncblock");
//			SQL_SELECT_RANGE		= mSQLConnection.prepareStatement("SELECT syncdata FROM syncblock WHERE block>? AND block<? ORDER BY block DESC");
//			SQL_DELETE_TXBLOCKS		= mSQLConnection.prepareStatement("DELETE FROM syncblock WHERE timemilli < ?");
			
		} catch (SQLException e) {
			MinimaLogger.log(e);
		}
	}
	
}

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
	PreparedStatement SQL_UPDATE_MAXIMA_HOST 		= null;
	PreparedStatement SQL_SELECT_MAXIMA_HOST 		= null;
	PreparedStatement SQL_SELECT_ALL_HOSTS 			= null;
	
	
	PreparedStatement SQL_INSERT_MAXIMA_CONTACT 	= null;
	
	public MaximaDB() {
		super();
	}
	
	@Override
	protected void createSQL() {
		try {
			
			//Create the various tables..
			Statement stmt = mSQLConnection.createStatement();

			//Create hosts table
			String hosts = "CREATE TABLE IF NOT EXISTS `hosts` ("
							+ "  `id` IDENTITY PRIMARY KEY,"
							+ "  `host` varchar(255) NOT NULL UNIQUE,"
							+ "  `publickey` blob NOT NULL,"
							+ "  `privatekey` blob NOT NULL,"
							+ "  `lastseen` bigint NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(hosts);

			//Create contacts table
			String contacts = "CREATE TABLE IF NOT EXISTS `contacts` ("
							+ "  `id` IDENTITY PRIMARY KEY,"
							+ "  `name` varchar(255) NOT NULL UNIQUE,"
							+ "  `publickey` blob NOT NULL,"
							+ "  `currenthost` varchar(255) NOT NULL,"
							+ "  `currentpublickey` blob NOT NULL"
							+ ")";
			
			//Run it..
			stmt.execute(contacts);
			
			//All done..
			stmt.close();
			
			//Create some prepared statements..
			SQL_INSERT_MAXIMA_HOST	= mSQLConnection.prepareStatement("INSERT IGNORE INTO hosts ( host, publickey, privatekey, lastseen ) VALUES ( ?, ? ,? ,? )");
			
//			String insert 			= "";
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

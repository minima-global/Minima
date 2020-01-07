package org.minima.system.network.webproxy;

/*public class RPCWebSocketServer extends WebSocketServer {

	int mPort;
	
	ConcurrentHashMap<String, WebSocket> mWebID = new ConcurrentHashMap<String, WebSocket>();
	
	//A link from the hashcode of the connection to tghe string id
	ConcurrentHashMap<Integer, String> mHashLink = new ConcurrentHashMap<Integer, String>();
	
	public RPCWebSocketServer( int port ) throws UnknownHostException {
		super( new InetSocketAddress( port ) );
		
		mPort = port;
		
		System.out.println("RPCWebSocketServer started on "+port);
	}
	
	public int getPort() {
		return mPort;
	}
	
	public WebSocket getConnection(String zWebID) {
		return mWebID.get(zWebID);
	}
	
	public void sendMessage(String zWebID, String zMessage) {
		WebSocket conn = getConnection(zWebID);
		conn.send(zMessage);
	}
	
	@Override
	public void onOpen(WebSocket conn, ClientHandshake handshake) {
		System.out.println("WS Open : "+conn);
	}

	@Override
	public void onClose(WebSocket conn, int code, String reason, boolean remote) {
		System.out.println("WS Close : "+conn);
		
		//Get the id..
		String uid = mHashLink.get(new Integer(conn.hashCode()));
		
		//Remove from the list
		if(uid!=null) {
			mWebID.remove(uid);
		}
	}

	@Override
	public void onMessage(WebSocket conn, String message) {
		//Received a message
		System.out.println("WS Message : "+message);
		
		//get the hashcode
		mHashLink.put(new Integer(conn.hashCode()), message);
		
		//This will be the website identifying itself..
		mWebID.put(message, conn);
	}

	@Override
	public void onError(WebSocket conn, Exception ex) {
		System.out.println("WS Error : "+conn);
	}
	
	@Override
	public void onStart() {
//		System.out.println("WS On Start..");
//		setConnectionLostTimeout(0);
		setConnectionLostTimeout(100);
	}
	
}
*/
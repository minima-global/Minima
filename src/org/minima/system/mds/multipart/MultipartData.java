package org.minima.system.mds.multipart;

import org.minima.utils.json.JSONObject;

public class MultipartData {

	public static final int TYPE_TEXT = 0;
	public static final int TYPE_FILE = 1;
	
	public int mType;
	
	public String mName="";
	
	public String mTextData="";
	
	public String mFileName 	= "";
	public String mContentType 	= "text/plain";
	public byte[] mFileData		= null;
	
	public MultipartData() {}
	
	public int getType() {
		return mType;
	}
	
	public String getName() {
		return mName;
	}
	
	public String getFileName() {
		return mFileName;
	}
	
	public String getContentType() {
		return mContentType;
	}
	
	public String getTextData() {
		return mTextData;
	}
	
	public byte[] getFileData() {
		return mFileData;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("type", mType);
		ret.put("name", mName);
		ret.put("value", mTextData);
		
		ret.put("filename", mFileName);
		ret.put("content-type", mContentType);
		
		return ret;
	}
}

package com.test;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import com.facebook.infer.annotation.ThreadSafe;


public class ThreadA {

	public static void main(String args[]) {
		TestThread T1 = new TestThread();
		T1.update();
		T1.start();
		TestThread T2 = new TestThread();
		T2.start();
	}

}
@ThreadSafe
class TestThread extends Thread {
	byte[] messageByte = new byte[100];
	int port = 8080;
	boolean read = true;
	
	Thread mythread;
	
	public void update() {
		this.read = false;
	}
	
	public void run() {
		
		  ServerSocket server; 
		  Socket clientSocket = null; 
		  int currentBytesRead; try {
		  server = new ServerSocket(port); 
		  Socket socket = server.accept();
		
		  DataInputStream in = new DataInputStream(new BufferedInputStream(socket.getInputStream())); 
		  DataOutputStream out = new DataOutputStream(clientSocket.getOutputStream());
		  
		  if(read) 
			  currentBytesRead = in.read(messageByte); 
		  else
			  out.write(messageByte);
		  
		  
		  } catch (IOException e) { // TODO Auto-generated catch block
		  e.printStackTrace(); }
		
	}	 
	
}
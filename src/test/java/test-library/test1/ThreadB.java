package com.test;
import com.facebook.infer.annotation.ThreadSafe;

@ThreadSafe

public class ThreadB {
	byte[] messageByte = new byte[100];
	int port = 8080;

	Thread mythread;

	public void run(boolean rand) {
	    byte[] a = new byte[100],b = new byte[100];

		if (rand)
			read(a,messageByte);
		else
			write(b,messageByte);
	}
	
	public void read(byte[] a, byte[] msg) {
		msg[0] = a[0];
	}
	
	public void write(byte[] a, byte[] msg) {
		a[0] = msg[0] ;
	}
	
}
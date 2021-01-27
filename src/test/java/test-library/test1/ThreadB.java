package com.test;
import com.facebook.infer.annotation.ThreadSafe;

@ThreadSafe

public class ThreadB {

	public static void main(String[] args) {
		TestThreadA T1 = new TestThreadA();
		T1.update();
		T1.start();
		TestThreadA T2 = new TestThreadA();
		T2.start();

	}

}

@ThreadSafe
class TestThreadA extends Thread {
	byte[] messageByte = new byte[100];
	int port = 8080;
	boolean read = true;
	byte rand = 1;
	
	Thread mythread;
	
	public void update() {
		this.read = false;
	}
	
	public void run() {
	
		if (read)
			read(messageByte);
		else
			write(messageByte);
	}
	
	public void read(byte[] messageByte) {
		rand = messageByte[0];
	}
	
	public void write(byte[] messageByte) {
		messageByte[0] = rand;
	}
	
}
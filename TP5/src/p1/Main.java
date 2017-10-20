package p1;

import test0.Sequence;
import test0.Tester;
import TP5.SequenceImp;

public class Main {
	public static void main(String[] args) {
		Sequence s= new SequenceImp();
		Tester t = new Tester(s);
		t.go();
	}
}
 
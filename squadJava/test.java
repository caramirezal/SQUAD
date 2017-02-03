package squad;

import net.sf.javabdd.*;

public class test {
 public static void main(String args[]){
	 int indexes[] = {1,2,3};
	 int fun[] = {0,1,0,0, 1,0,0,0};
	 
	 BDD b;
	 
	 b = boolFunToBDD.conj(fun,indexes);
	 b.printDot();
 }
}

/*
 * Written by Carlos Ramirez
 * February 2, 2017
 * Mexico City
 * */

package squad;

public class dec2Bin {
	public static boolean[] decimalToBinary(int d, int n){
		//System.out.println("decimal = " + d );
		int r = d;
		int b;
	    boolean B[] = new boolean[n];
		for (int i=0;i<=(n-2);i++){
		        b = r / (int)Math.pow(2, n-(i+1));
		        r = r % (int)Math.pow(2, n-(i+1));
				//System.out.println("n-(i+1) = "+ (n-(i+1)) );
				if (b == 0) {
					//System.out.println("b = 0 ");
					B[n-(i+1)] = false ;
					//
				}
				else if (b==1) {
					//System.out.println("b = 1 ");
					B[n-(i+1)] = true;
				}
				else {
					//System.out.println("An error occurs");
				}
		}
		if ( (d%2) == 0 ) {
			B[0] = false;
		}
		else if ( (d%2)==1) {
			B[0] = true;
		}
		else {
			System.out.println("An error occur");
		}
		//System.out.print("Binary: ");
		/*
		for (int i=0;i<n;i++ ){
			if (B[i]==false){
				System.out.print("0 " );				
			}
			if (B[i]==true){
				System.out.print("1 " );				
			}
		}
		System.out.println();
		*/
		return B;
	}	
}

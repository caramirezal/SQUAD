
public class getAttTest {

	public static void main(String args[]){
		byte res[][];
		res = SSSearcher.getAttractors();
		for (int i=0;i<res.length;i++){
			for (int j=0;j<res[i].length;j++){
				System.out.print(res[i][j]);
			}
			System.out.println();
		}
	}
}


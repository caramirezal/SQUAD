package gargAlgorithm;

import net.sf.javabdd.*;
import java.util.*;
 /*
  * Transform an arbitrary Boolean function to a Binary decision
  * diagram representation.
 */

public class boolFuncToBDD {
	public static void main(String args[]){
		//BDD b;
		conjunction co = new conjunction();
		co.conj();	
		//b.printDot();
	}
}


class conjunction {
  void conj (){
	  
	  // conjBool has true or false according to a disjunctive
	  // canonical form.
	  boolean conjBool[] = {true, true, false};
	  int disjBool[] = {1,0,0,0, 1,0,1,1};
	  int n = conjBool.length;
	  int N = (int)Math.pow(2, n);
	  BDDFactory F;
	  F = BDDFactory.init(1000, 1000);
      F.setVarNum(n);
      // conjBDD variable contains a conjunctive form for the variables
      BDD conjBDD;
      // normal disjunctive form
      BDD disjBDD;
      // disjBDD stories the nodes of the conjunctive form
      BDD[] nodes;
      nodes = new BDD[n];

      List<Integer> indexes = new ArrayList<Integer>();
      
      for (int i=0;i<N;i++){
    	  if ( disjBool[i] == 1 ){
    		  indexes.add(i);
    	  }
      }
      
      // disjunctives stores BDD representation of conjunctive forms
      BDD disjunctive[] = new BDD[indexes.size()];      
      //System.out.println(indexes.size());
      
      for (int i=0;i<indexes.size();i++) {
    	  // System.out.println("Index: "+indexes.get(i));
    	  conjBool = decToBin.decimalToBinary(indexes.get(i), n);
          /* 
           * Construction of conjBool.
           * RBDD representation of normal conjunctive forms. Nodes are set 
           * to a simple (or negative) binary decision node according to the 
           * values of the conjunctive canonical form.
          */
    	      
    	  for (int j=0;j<n;j++){
    		  
    	      if (conjBool[j]==true){
    	    	  nodes[j] = F.ithVar(j);
    	      }
    	      else if (conjBool[j]==false) {
    	    	  BDD val = F.ithVar(j);
    	    	  nodes[j] = val.not();
    	      }
    	      else { System.out.println("Error. Non boolean value. " + i ); }		  
    	  }
    	  conjBDD = nodes[0];
          for (int j=1;j<n;j++){
        	  conjBDD = conjBDD.and(nodes[j]);
          }
          
          disjunctive[i] = conjBDD;
      }
      
      disjBDD = disjunctive[0];
      for (int i=1;i<disjunctive.length;i++){
    	  //disjunctive[i].printDot();
    	  disjBDD = disjBDD.or(disjunctive[i]);
      }
      disjBDD.printDot();
      
  //return conjBDD;    
  }
}


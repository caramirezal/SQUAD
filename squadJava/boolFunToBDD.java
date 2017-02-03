/*
 * Written by Carlos Ramirez
 * February 2, 2017
 * Mexico City
 * */

package squad;

import java.util.ArrayList;
import java.util.List;
import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDFactory;

public class boolFunToBDD {

	  static BDD conj (int boolFunc[],int netIndexes[]){


		  BDDFactory F;
		  F = BDDFactory.init(1000, 1000);
	      F.setVarNum(10);
	      
	      // networkBDD is the BDD representation of the Boolean network
	      BDD networkBDD[] = new BDD[boolFunc.length];
	      BDD netBDD;

	      
	      int disjBool[];
	      int indexesBDD[];

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		  


	          
	          disjBool = boolFunc;
	    	  indexesBDD = netIndexes;
	    	  
	    	  int n = indexesBDD.length;
	    	  int N = (int)Math.pow(2, n);
	    	  // conjBool is a 0/1 valued array which has true or false values according to a disjunctive
	    	  // canonical form.
	          boolean conjBool[] = new boolean[n];
	          // conjBDD BDD contains a conjunctive form for the variables
	          BDD conjBDD;
	          // disjBDD is the normal disjunctive form in BDD representation
	          BDD disjBDD;
	          // nodes stores the conjunctive BDD forms
	          BDD nodes[];
	          nodes = new BDD[n];

	          /*
	           * This block of code transforms a boolean function (BF) given as a array of 0 and 1 values which are the values of the 
	           * true table representation of the BF.
	           * 
	           * */
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
	        	  conjBool = dec2Bin.decimalToBinary(indexes.get(i), n);
	              /* 
	               * Construction of conjBool.
	               * RBDD representation of normal conjunctive forms. Nodes are set 
	               * to a simple (or negative) binary decision node according to the 
	               * values of the conjunctive canonical form.
	              */
	        	      
	        	  for (int j=0;j<n;j++){
	        		  
	        	      if (conjBool[j]==true){
	        	    	  nodes[j] = F.ithVar(indexesBDD[j]);
	        	      }
	        	      else if (conjBool[j]==false) {
	        	    	  BDD val = F.ithVar(indexesBDD[j]);
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
	          

	          //networkBDD[k] = disjBDD;
	          
	          
	  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	      

	      
	  return disjBDD;    
	  }
}

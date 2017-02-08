/*
 * Written by Carlos Ramirez
 * January 2, 2017
 * Mexico City
 * */

package squad;

import java.util.ArrayList;
import java.util.List;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDFactory;

public class NetTransitionBDDTest {
	  public static void netTransitionBDD(){
		  /*
		   * Definition of the net rules.
		   * BoolFunc contains true table representation of boolean functions
		   * associated with each  nodes.
		   * netIndexes contains indexes of the regulators of the node i
		   * */
		  int boolFunc[][] = {{0,1,0,0},{0,1},{1,0}};
		  int netIndexes[][] = {{0,2},{0},{1}};
		  BDDFactory F;
		  F = BDDFactory.init(1000, 1000);
	      F.setVarNum(10);
	      // network BDD stores Ti function as defined in Garg algorithm (2006)
	      BDD networkBDD[] = new BDD[boolFunc.length];
	      // netBDD is the disjunction of all Ti's
	      BDD netBDD;	      
	      // disjBool and indexesBDD temporally stores BoolFunc and netIndexes elements
	      int disjBool[];
	      int indexesBDD[];
	      /*
	       * Construction of networkBDD. 
	       * networkBDD[i] = a_0' <-> a_0 and ... a_i' <-> fi and ... a_n' <-> a_n
	       * Where a_j, a_j' are node j values in time t and t + 1, respectively.
	       * fi is the boolean function associated of node i in BDD representation. 
	       * */
	      for (int nodeIndex=0;nodeIndex<boolFunc.length;nodeIndex++){
	          disjBool = boolFunc[nodeIndex];
	    	  indexesBDD = netIndexes[nodeIndex];
	    	  // n is the number of regulators of the node i (=nodeIndex)
	    	  int n = indexesBDD.length;
	    	  int N = (int)Math.pow(2, n);
	          boolean conjBool[] = new boolean[n];
	          BDD conjBDD;
	          BDD disjBDD;
	          BDD nodes[];
	          nodes = new BDD[n];
	          /*
	           * indexes list stores the indexes in which boolFunc[i] element
	           * takes a 1 value for construction of the canonical representation
	           * of the function fi = boolFunc[i]
	           * */
	          List<Integer> indexes = new ArrayList<Integer>();
	          for (int i=0;i<N;i++){
	        	  if ( disjBool[i] == 1 ){
	        		  indexes.add(i);
	        	  }
	          }          
	          /* 
	           * disjunctive stores the conjunctions of the regulators of node i
	           * in the BDD canonical representation of fi.
	          */ 
	          BDD disjunctive[] = new BDD[indexes.size()];
	          for (int i=0;i<indexes.size();i++) {
	        	  conjBool = dec2Bin.decimalToBinary(indexes.get(i), n);
	        	  for (int j=0;j<n;j++){
	        	      if ( conjBool[j] ){
	        	    	  nodes[j] = F.ithVar(indexesBDD[j]);
	        	      }
	        	      else if ( ! conjBool[j] ) {
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
	              //conjBDD.printDot();
	          }
	          
	          /*
	           *  Canonical representation of fi construction by taking the
	           *  a disjunction of all of the conjunctive forms 
	           * */
	          disjBDD = disjunctive[0];
	          for (int i=1;i<disjunctive.length;i++){
	        	  disjBDD = disjBDD.or(disjunctive[i]);
	          }
	          //disjBDD.printDot();                            //checkpoint r
	          /*
	           * The node (i + total of variables) is taken as the node i
	           * in the time (t + 1 ), denoted as i'. Then i' <-> fi  
	           * */
	          BDD disjBDD_next = F.nithVar(nodeIndex+boolFunc.length);
        	  disjBDD_next = disjBDD_next.biimp(disjBDD.not());
	          //disjBDD_next.printDot();                      //checkpoint r
	          // 
        	  
        	  
	          for (int i=0;i<boolFunc.length;i++){
	        	  if (i!=nodeIndex){
		        	  BDD nodeBDD;
		        	  BDD nextTimeNode;
		        	  nodeBDD = F.nithVar(i); // first invocation of node i
		        	  nodeBDD = nodeBDD.not();
		        	  //nodeBDD.printDot();
		        	  nextTimeNode = F.nithVar(boolFunc.length+i);
		        	  nextTimeNode = nextTimeNode.not();
		        	  //nextTimeNode.printDot();
		        	  nextTimeNode = nextTimeNode.biimp(nodeBDD);
	        		  disjBDD_next = disjBDD_next.and(nextTimeNode);
	        	  }
	          }
	    	  networkBDD[nodeIndex] = disjBDD_next;

	      }

	      for (int i=0;i<networkBDD.length;i++){
	    	  networkBDD[i].printDot();
	      }

    	  
	      netBDD = networkBDD[0];
	      for (int i=1;i<boolFunc.length;i++){
	    	  //networkBDD[i].printDot();
	    	  netBDD = netBDD.or(networkBDD[i]);
	      }
	      netBDD.printDot();                   // checkpoint
	      /*
	       * Construction of the initial state BDD representation
	       * */
	      BDD initialStateBDD;

	      initialStateBDD = F.nithVar(0);
	      BDD node;
	      for (int i=1;i<boolFunc.length;i++){
	      	node = F.nithVar(i); // second invocation of node i
	      	initialStateBDD = initialStateBDD.or(node);
	      }
		  

	      //return initialStateBDD.and(netBDD);
	  }
}

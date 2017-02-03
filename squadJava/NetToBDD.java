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

public class NetToBDD {
	  public static BDD netToBDD(){
		  int boolFunc[][] = {{0,1,0,0},{0,1},{1,0}};
		  int netIndexes[][] = {{0,2},{0},{1}};
		  BDDFactory F;
		  F = BDDFactory.init(1000, 1000);
	      F.setVarNum(10);
	      BDD networkBDD[] = new BDD[boolFunc.length];
	      BDD netBDD;	      
	      int disjBool[];
	      int indexesBDD[];
/////////////////////////////////////////////////////////////////////////////////////////
	      // modify this to construct T function
	      for (int nodeIndex=0;nodeIndex<boolFunc.length;nodeIndex++){
	          disjBool = boolFunc[nodeIndex];
	    	  indexesBDD = netIndexes[nodeIndex];
	    	  int n = indexesBDD.length;
	    	  int N = (int)Math.pow(2, n);
	          boolean conjBool[] = new boolean[n];
	          BDD conjBDD;
	          BDD disjBDD;
	          BDD nodes[];
	          nodes = new BDD[n];
	          List<Integer> indexes = new ArrayList<Integer>();
	          for (int i=0;i<N;i++){
	        	  if ( disjBool[i] == 1 ){
	        		  indexes.add(i);
	        	  }
	          }          
	          BDD disjunctive[] = new BDD[indexes.size()]; 
	          for (int i=0;i<indexes.size();i++) {
	        	  conjBool = dec2Bin.decimalToBinary(indexes.get(i), n);
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
	          /*
	           * Here modify disjunctive to disjBDD[i] = disjBDD[i].and(node[j])
	           *  for all node[j]  (i!=j)
	           * */
	          disjBDD = disjunctive[0];
	          for (int i=1;i<disjunctive.length;i++){
	        	  disjBDD = disjBDD.or(disjunctive[i]);
	          }
	          
	          BDD disjBDD_next = F.nithVar(nodeIndex+boolFunc.length);
        	  disjBDD_next = ( ( disjBDD.and(disjBDD_next.not() ).not()) )
     			         .and( ( disjBDD_next.and(disjBDD.not()).not() ));
	          
	          // 
	          for (int i=0;i<boolFunc.length;i++){
	        	  if (i!=nodeIndex){
		        	  BDD nodeBDD;
		        	  BDD nextTimeNode;
		        	  nodeBDD = F.nithVar(i);
		        	  nextTimeNode = F.nithVar(boolFunc.length+i);
		        	  nextTimeNode = ( ( nodeBDD.and(nextTimeNode.not() ).not()) )
		        			     .and( ( nextTimeNode.and(nodeBDD.not()).not() ));
	        		  disjBDD_next = disjBDD_next.and(nextTimeNode);
	        	  }
	          }
	    	  networkBDD[nodeIndex] = disjBDD_next;
	    	  
	      }
    	  /*
    	   * Make BDD a;
    	   *  a = F.nthVar(numberOfNodes + nodeIndex)
    	   *  take an OR of all a's to construct T
    	   * */
	      netBDD = networkBDD[0];
	      for (int i=1;i<boolFunc.length;i++){
	    	  netBDD = netBDD.or(networkBDD[i]);
	      }
//////////////////////////////////////////////////////////////////////////////////////////
	      return netBDD;
	  }
}

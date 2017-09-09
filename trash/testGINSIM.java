//package ginsim;

//import ginsim.org.gimsim.*;
import org.ginsim.service.format.sbml.*;
import org.ginsim.service.tool.stablestates.StableStatesService;
import java.util.Arrays;
import org.ginsim.core.graph.regulatorygraph.RegulatoryGraph;
import org.ginsim.gui.graph.dynamicgraph.StableTableModel;

public class testGINSIM {
	public static void main(String Args[]){
		
		System.out.println("Test GINSIM");
		
		// Importing sbml file to ginsim
		RegulatoryGraph Net;
		SBMLqualService S = new SBMLqualService();
		String pathFile = "/home/carlos/scripts/SQUAD/squadJava/regulatoryNetworkGMPModel.sbml";
		Net = S.importLRG(pathFile);
		System.out.println("Graph name: "+Net.getGraphName());
		System.out.println("Graph: "+Net.toString());
		
		// Getting methods and fields from RegulatoryGraph class
		// Logical model
		org.colomoto.logicalmodel.LogicalModel netModel;
		netModel = Net.getModel();
		System.out.println("LogicalModel: "+netModel.toString());
		// Logical functions
		int logfun[];
		logfun = netModel.getLogicalFunctions();
		System.out.println("Logical functions: "+logfun);
		// Size of the network
		System.out.println("Number of nodes: "+Net.getNodeCount());
		org.colomoto.logicalmodel.tool.stablestate.StableStateSearcher sss;

		// Getting the stable states searcher service 
		StableStatesService sser = new StableStatesService();
		sss = sser.getSearcher(Net);
		
		//setRunning(true);
		//int res;
		//sss.start();
		sss.run();
		System.out.println(sss.getStatus());
		//int result;
		//result = sss.getResult();
		//sss.
		//System.out.println("Result: "+result);
		StableTableModel model;
		model = new StableTableModel(Net);
		
		model.setResult(sss.getMDDManager(), sss.getResult() );
		//if (sss.getStatus() == "FINISHED") {
		System.out.println("Number of rows: "+model.getRowCount());
		System.out.println("State: "+model.getState(0));
		System.out.println("Help: "+model.toString());
		//System.out.println("State: "+model.);
		//}
		for (int i=0;i<model.getRowCount();i++){
			System.out.println("State: "+Arrays.toString(model.getState(i)));
		}
		int t;
		t = 10;
		System.out.println(t);
	}
}

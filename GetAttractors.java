/*
   Performs the calculation of asynchronous network model using GINSIM
*/

import org.ginsim.service.format.sbml.*;
import org.ginsim.service.tool.stablestates.StableStatesService;
import java.util.Arrays;
import org.ginsim.core.graph.regulatorygraph.RegulatoryGraph;
import org.ginsim.gui.graph.dynamicgraph.StableTableModel;

class GetAttractors {
	public static void main(String Args[]){
		// Importing sbml file to ginsim
		RegulatoryGraph Net;
		SBMLqualService S = new SBMLqualService();
		String pathFile = "regulatoryNetworkModel.sbml";
		Net = S.importLRG(pathFile);
		
		// Getting the stable states searcher service 
		StableStatesService sser = new StableStatesService();
		org.colomoto.logicalmodel.tool.stablestate.StableStateSearcher sss;
		sss = sser.getSearcher(Net);
		
		// runnning methods and get attractors
		sss.run();
		StableTableModel model;
		model = new StableTableModel(Net);

		// getting and exporting the results	
		model.setResult(sss.getMDDManager(), sss.getResult() );
		for (int i=0;i<model.getRowCount();i++){
			//System.out.println("State: "+Arrays.toString(model.getState(i)));
			for (int j=0;j<model.getState(i).length;j++){
				System.out.print(model.getState(i)[j]);
				if ( j < ( model.getState(i).length - 1 ) ) {
					System.out.print(",");
					}
				}
			System.out.println();
		}

	}
}





/*
   Performs the calculation of asynchronous network model using GINSIM
*/

import org.ginsim.service.format.sbml.*;
import org.ginsim.service.tool.stablestates.StableStatesService;
import java.util.Arrays;
import org.ginsim.core.graph.regulatorygraph.RegulatoryGraph;
import org.ginsim.gui.graph.dynamicgraph.StableTableModel;

class GetAttractors {
	public static byte[][] get(String pathFile){
		// Importing sbml file to ginsim
		RegulatoryGraph Net;
		SBMLqualService S = new SBMLqualService();
		//String pathFile = "regulatoryNetworkGMPModel.sbml";
		Net = S.importLRG(pathFile);
		
		// Getting the stable states searcher service 
		StableStatesService sser = new StableStatesService();
		org.colomoto.logicalmodel.tool.stablestate.StableStateSearcher sss;
		sss = sser.getSearcher(Net);
		
		// runnning methods and get attractors
		sss.run();
		//System.out.println(sss.getStatus());
		StableTableModel model;
		model = new StableTableModel(Net);

		// getting the results		
		model.setResult(sss.getMDDManager(), sss.getResult() );
		byte res[][] = new byte[model.getRowCount()][]; 
		for (int i=0;i<model.getRowCount();i++){
			res[i] = model.getState(i);
			//System.out.println(Arrays.toString(model.getState(i).getClass().getName()) );
			}
		
		return res;
	}
}


class test {
	public static void main(String Args[]){
		//System.out.println("Hola Mundo");
                byte res[][];
                res = GetAttractors.get("regulatoryNetworkGMPModel.sbml");
                for (int i=0;i<res.length;i++){
			for (int j=0;j<res[i].length;j++){
	                        System.out.print(res[i][j]);
				if (j<( res[i].length - 1 ) ) {
					System.out.print(",");
					}
				}
			System.out.println();
                        }

	}
}

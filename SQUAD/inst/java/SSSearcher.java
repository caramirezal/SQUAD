//package squad;

//import java.util.Arrays;

import org.ginsim.core.graph.regulatorygraph.RegulatoryGraph;
import org.ginsim.gui.graph.dynamicgraph.StableTableModel;
import org.ginsim.service.format.sbml.SBMLqualService;
import org.ginsim.service.tool.stablestates.StableStatesService;

 

public class SSSearcher {
		public static byte[][] getAttractors(){
			
			// Importing sbml file to ginsim
			RegulatoryGraph Net;
			SBMLqualService S = new SBMLqualService();
			String pathFile = "regulatoryNetworkModel.sbml";
			Net = S.importLRG(pathFile);
			
			

			// Getting the stable states searcher service 
			org.colomoto.logicalmodel.tool.stablestate.StableStateSearcher sss;
			StableStatesService sser = new StableStatesService();
			sss = sser.getSearcher(Net);
			
			// Running the method
			sss.run();
			System.out.println(sss.getStatus());
			StableTableModel statStates;
			statStates = new StableTableModel(Net);			
			statStates.setResult(sss.getMDDManager(), sss.getResult() );
			
			// setting the results in the model object
			StableTableModel model;
			model = new StableTableModel(Net);			
			model.setResult(sss.getMDDManager(), sss.getResult() );
			
			int nrow = model.getRowCount();
			System.out.println("nrow "+ nrow);
			// This needs to be modified
			int ncol = model.getState(0).length;
			System.out.println("ncol "+ ncol);
			
			byte res[][] = new byte[nrow][ncol];
			
			for (int i=0;i<nrow;i++){
				for (int j=0;j<ncol;j++){
					res[i][j] = model.getState(i)[j];
				}
			}
			
			return(res);
			
			
		}

}



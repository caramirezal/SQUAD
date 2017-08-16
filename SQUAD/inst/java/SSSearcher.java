//package squad;

//import java.util.Arrays;

import org.ginsim.core.graph.regulatorygraph.RegulatoryGraph;
import org.ginsim.gui.graph.dynamicgraph.StableTableModel;
import org.ginsim.service.format.sbml.SBMLqualService;
import org.ginsim.service.tool.stablestates.StableStatesService;



public class SSSearcher {
		public static int[][] getAttractors(){

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
			//System.out.println(sss.getStatus());

			// setting the results in the model object
			StableTableModel model;
			model = new StableTableModel(Net);
			model.setResult(sss.getMDDManager(), sss.getResult() );

			int nrow = model.getRowCount();
			//System.out.println("nrow "+ nrow);

			int res[][] = new int[nrow][];

			// This needs to be modified
			if ( nrow > 0 ) {

			        int ncol = model.getState(0).length;
			        //System.out.println("ncol "+ ncol);

			        res = new int[nrow][ncol];

			        for (int i=0;i<nrow;i++){
				        for (int j=0;j<ncol;j++){
					        res[i][j] = model.getState(i)[j];
				        }
			        }

			}

			if ( nrow <= 0 ) {

			        res = new int[0][0];
			        //System.out.println("No fixed points found! Maybe all solutions are cycles");

			}

			return res;

		}

}



package squad;

import org.ginsim.service.format.sbml.*;
import org.ginsim.core.graph.regulatorygraph.*;
import org.colomoto.logicalmodel.tool.stablestate.StableStateSearcher;
import org.ginsim.core.graph.GraphManager;
import org.ginsim.core.service.ServiceManager;
import org.ginsim.core.graph.regulatorygraph.RegulatoryGraph;

public class testGINSIM {
	public static void main(String Args[]){
		
		System.out.println("Test GINSIM");
		RegulatoryGraph Net;
		SBMLqualService S = new SBMLqualService();

		// Importing sbml file to ginsim
		Net = S.importLRG("/home/carlos/scripts/RegulatoryNetworkGMPModel/regulatoryNetworkGMPModel.SBML");
		//assertNotNull( "Import graph : Graph node number is not correct", Net);
		System.out.println(Net.getGraphName());
		
		ServiceManager SerMan;
		StableStateSearcher ssSearcher;
		SerMan.getServicesInfo().toString();

		
		
	}
}

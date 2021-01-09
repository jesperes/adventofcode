package common2;

import java.awt.Desktop;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.server.handler.HandlerCollection;
import org.eclipse.jetty.server.handler.ResourceHandler;
import org.eclipse.jetty.servlet.ServletContextHandler;

/* 
 * https://www.hascode.com/2012/10/html5-server-send-events-using-node-js-or-jetty/
 */
public class AocServer {
    public static void main(String[] args) throws Exception {

        HandlerCollection collection = new HandlerCollection();

        Server server = new Server();
        ServerConnector connector = new ServerConnector(server);
        server.addConnector(connector);

        ResourceHandler handler = new ResourceHandler();
        handler.setDirectoriesListed(true);
        handler.setDirAllowed(true);
        handler.setResourceBase("web");

        ServletContextHandler servletContextHandler = new ServletContextHandler(
                server, "/");
        servletContextHandler.addServlet(AocEventSourceServlet.class, "/talk");
        collection.addHandler(handler);
        collection.addHandler(servletContextHandler);
        server.setHandler(collection);
        server.start();

        if (Desktop.isDesktopSupported()
                && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)) {
            Desktop.getDesktop().browse(server.getURI());
        }

        server.join();
    }
}

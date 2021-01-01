package aoc2020;

import java.awt.Desktop;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.server.handler.ResourceHandler;

public class AocServer {
    public static void main(String[] args) throws Exception {

        Server server = new Server();
        ServerConnector connector = new ServerConnector(server);
        server.addConnector(connector);

        ResourceHandler handler = new ResourceHandler();
        handler.setDirectoriesListed(true);
        handler.setDirAllowed(true);
        handler.setResourceBase("web");
        server.setHandler(handler);
        server.start();

        if (Desktop.isDesktopSupported()
                && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)) {
            Desktop.getDesktop().browse(server.getURI());
        }

        server.join();
    }
}

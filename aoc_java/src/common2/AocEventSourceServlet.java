package common2;

import java.io.IOException;
import java.util.Date;

import org.eclipse.jetty.servlets.EventSource;
import org.eclipse.jetty.servlets.EventSourceServlet;

import jakarta.servlet.http.HttpServletRequest;

public class AocEventSourceServlet extends EventSourceServlet {
    private static final long serialVersionUID = 1L;

    @Override
    protected EventSource newEventSource(final HttpServletRequest req) {
        return new EventSource() {
            @Override
            public void onOpen(final Emitter emitter) throws IOException {
                emitter.data("new server event " + new Date().toString());
                while (true) {
                    System.out.println("propagating event..");
                    try {
                        Thread.sleep(2000);
                        emitter.data(
                                "new server event " + new Date().toString());
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }

            @Override
            public void onClose() {
                System.out.println("closed");
            }
        };
    }
}

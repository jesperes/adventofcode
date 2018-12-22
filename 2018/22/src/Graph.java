
import java.util.HashSet;
import java.util.Set;

public class Graph<T> {

    private Set<Node<T>> nodes = new HashSet<>();

    public void addNode(Node<T> node) {
        nodes.add(node);
    }

    public Set<Node<T>> getNodes() {
        return nodes;
    }

    public void setNodes(Set<Node<T>> nodes) {
        this.nodes = nodes;
    }
}

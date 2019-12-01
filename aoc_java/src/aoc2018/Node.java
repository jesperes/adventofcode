
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class Node<T> {

    private LinkedList<Node<T>> shortestPath = new LinkedList<>();

    private Integer distance = Integer.MAX_VALUE;

    private Map<Node<T>, Integer> adjacentNodes = new HashMap<>();

    private T payload;

    public Node(T payload) {
        this.payload = payload;
    }

    public T getPayload() {
        return payload;
    }

    public void addDestination(Node<T> destination, int distance) {
        adjacentNodes.put(destination, distance);
    }

    public Map<Node<T>, Integer> getAdjacentNodes() {
        return adjacentNodes;
    }

    public void setAdjacentNodes(Map<Node<T>, Integer> adjacentNodes) {
        this.adjacentNodes = adjacentNodes;
    }

    public Integer getDistance() {
        return distance;
    }

    public void setDistance(Integer distance) {
        this.distance = distance;
    }

    public List<Node<T>> getShortestPath() {
        return shortestPath;
    }

    public void setShortestPath(LinkedList<Node<T>> shortestPath) {
        this.shortestPath = shortestPath;
    }

}

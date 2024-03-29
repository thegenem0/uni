// GraheER - C++ implementation of a graph editor
#include <climits>
#include <iostream>
#include <random>
#include <vector>

class Graph {
private:
  std::vector<std::vector<int>> adjacencyList;

protected:
  void addEdgeToAdjacencyList(int from, int to) {
    if (from < adjacencyList.size() && to < adjacencyList.size()) {
      adjacencyList[from].push_back(to);
    }
  }

  void removeEdgeFromAdjacencyList(int from, int to) {
    if (from < adjacencyList.size() && to < adjacencyList.size()) {
      for (int i = 0; i < adjacencyList[from].size(); ++i) {
        if (adjacencyList[from][i] == to) {
          adjacencyList[from].erase(adjacencyList[from].begin() + i);
          break;
        }
      }
    }
  }

  void addNodeToAdjacencyList() { adjacencyList.push_back(std::vector<int>()); }

  void removeNodeFromAdjacencyList(int node) {
    if (node < adjacencyList.size()) {
      for (int i = 0; i < adjacencyList.size(); ++i) {
        for (int j = 0; j < adjacencyList[i].size(); ++j) {
          if (adjacencyList[i][j] == node) {
            adjacencyList[i].erase(adjacencyList[i].begin() + j);
            break;
          }
        }
      }
      adjacencyList.erase(adjacencyList.begin() + node);
    }
  }

public:
  Graph(int numNodes) : adjacencyList(numNodes) {
    for (int i = 0; i < numNodes; ++i) {
      adjacencyList[i] = std::vector<int>();
    }
  }

  virtual ~Graph() {}

  virtual void addEdge(int from, int to) = 0;
  virtual void removeEdge(int from, int to) = 0;
  virtual void addNode() = 0;
  virtual void removeNode(int node) = 0;

  void printGraph() {
    for (int i = 0; i < adjacencyList.size(); ++i) {
      std::cout << i << ": ";
      for (int j = 0; j < adjacencyList[i].size(); ++j) {
        std::cout << adjacencyList[i][j] << " ";
      }
      std::cout << std::endl;
    }
  }

  void BFS(int startNode) {
    std::vector<bool> visited(adjacencyList.size(), false);
    std::vector<int> queue;
    queue.push_back(startNode);
    visited[startNode] = true;

    while (!queue.empty()) {
      int node = queue.front();
      queue.erase(queue.begin());
      std::cout << node << " ";

      for (int i = 0; i < adjacencyList[node].size(); ++i) {
        if (!visited[adjacencyList[node][i]]) {
          queue.push_back(adjacencyList[node][i]);
          visited[adjacencyList[node][i]] = true;
        }
      }
    }
  }

  void DFS(int startNode) {
    std::vector<bool> visited(adjacencyList.size(), false);
    DFSUtil(startNode, visited);
  }

  void Dijkstra(int startNode) {
    std::vector<bool> visited(adjacencyList.size(), false);
    std::vector<int> distance(adjacencyList.size(), INT_MAX);
    std::vector<int> parent(adjacencyList.size(), -1);
    distance[startNode] = 0;

    for (int i = 0; i < adjacencyList.size(); ++i) {
      int minDistance = INT_MAX;
      int minNode = -1;

      for (int j = 0; j < adjacencyList.size(); ++j) {
        if (!visited[j] && distance[j] < minDistance) {
          minDistance = distance[j];
          minNode = j;
        }
      }

      if (minNode == -1) {
        break;
      }

      visited[minNode] = true;

      for (int j = 0; j < adjacencyList[minNode].size(); ++j) {
        int node = adjacencyList[minNode][j];
        int weight = 1;

        if (distance[minNode] + weight < distance[node]) {
          distance[node] = distance[minNode] + weight;
          parent[node] = minNode;
        }
      }
    }

    for (int i = 0; i < adjacencyList.size(); ++i) {
      std::cout << "Distance from " << startNode << " to " << i << " is "
                << distance[i] << std::endl;
    }
  }

private:
  void DFSUtil(int node, std::vector<bool> &visited) {
    visited[node] = true;
    std::cout << node << " ";

    for (int i = 0; i < adjacencyList[node].size(); ++i) {
      if (!visited[adjacencyList[node][i]]) {
        DFSUtil(adjacencyList[node][i], visited);
      }
    }
  }
};

class DirectedGraph : public Graph {
public:
  DirectedGraph(int numNodes) : Graph(numNodes) {

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, 1);

    for (int i = 0; i < numNodes; ++i) {
      for (int j = 0; j < numNodes; ++j) {
        if (i != j && dis(gen) == 1) {
          addEdge(i, j);
        }
      }
    }
  }

  void addEdge(int from, int to) override { addEdgeToAdjacencyList(from, to); }
  void removeEdge(int from, int to) override {
    removeEdgeFromAdjacencyList(from, to);
  }
  void addNode() override { addNodeToAdjacencyList(); }
  void removeNode(int node) override { removeNodeFromAdjacencyList(node); }
};

class UndirectedGraph : public Graph {
public:
  UndirectedGraph(int numNodes) : Graph(numNodes) {

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, 1);

    for (int i = 0; i < numNodes; ++i) {
      for (int j = 0; j < numNodes; ++j) {
        if (i != j && dis(gen) == 1) {
          addEdge(i, j);
          addEdge(j, i);
        }
      }
    }
  }

  void addEdge(int from, int to) override { addEdgeToAdjacencyList(from, to); }
  void removeEdge(int from, int to) override {
    removeEdgeFromAdjacencyList(from, to);
  }
  void addNode() override { addNodeToAdjacencyList(); }
  void removeNode(int node) override { removeNodeFromAdjacencyList(node); }
};

int main() {
  std::string graphType;
  int numNodes, studentID = 20055710;

  std::cout << "Enter graph type (directed/undirected): ";
  std::cin >> graphType;

  if (graphType != "directed" && graphType != "undirected") {
    std::cout << "Invalid graph type\n";
    return 1;
  }

  std::cout << "Enter number of nodes: ";
  std::cin >> numNodes;

  bool isDirected = (graphType == "directed");

  Graph *graph;

  if (isDirected) {
    graph = new DirectedGraph(numNodes);
  } else {
    graph = new UndirectedGraph(numNodes);
  }

  int option;
  while (true) {
    std::cout << "1. Print graph\n";
    std::cout << "2. Add node\n";
    std::cout << "3. Remove node\n";
    std::cout << "4. Add edge\n";
    std::cout << "5. Remove edge\n";
    std::cout << "6. BFS\n";
    std::cout << "7. DFS\n";
    std::cout << "8. Dijkstra's Algorithm\n";
    std::cout << "Enter option: \n";
    std::cin >> option;

    switch (option) {
      int startNode;
    case 1:
      graph->printGraph();
      break;
    case 2:
      graph->addNode();
      break;
    case 3:
      std::cout << "Enter node: ";
      std::cin >> startNode;
      graph->removeNode(startNode);
      break;
    case 4:
      std::cout << "Enter from node: ";
      std::cin >> startNode;
      std::cout << "Enter to node: ";
      std::cin >> option;
      graph->addEdge(startNode, option);
      break;
    case 5:
      std::cout << "Enter from node: ";
      std::cin >> startNode;
      std::cout << "Enter to node: ";
      std::cin >> option;
      graph->removeEdge(startNode, option);
      break;
    case 6:
      std::cout << "Enter start node: ";
      std::cin >> startNode;
      graph->BFS(startNode);
      break;
    case 7:
      std::cout << "Enter start node: ";
      std::cin >> startNode;
      graph->DFS(startNode);
      break;
    case 8:
      std::cout << "Enter start node: ";
      std::cin >> startNode;
      graph->Dijkstra(startNode);
      break;
    default:
      std::cout << "Invalid option\n";
    }
  }

  delete graph;
  return 0;
}

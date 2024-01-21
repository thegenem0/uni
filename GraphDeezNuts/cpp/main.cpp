#include <climits>
#include <iostream>
#include <random>
#include <vector>

class Graph {
private:
  std::vector<std::vector<int>> adjacencyList;
  bool isDirected;

public:
  Graph(int numNodes, bool isDirected)
      : adjacencyList(numNodes), isDirected(isDirected) {
    adjacencyList.resize(numNodes);

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, 1);

    for (int i = 0; i < numNodes; ++i) {
      for (int j = 0; j < numNodes; ++j) {
        if (i != j && dis(gen) == 1) {
          addEdge(i, j);
          if (!isDirected) {
            addEdge(j, i);
          }
        }
      }
    }
  }

  void addEdge(int u, int v) {
    if (u < adjacencyList.size() && v < adjacencyList.size()) {
      adjacencyList[u].push_back(v);
      if (!isDirected) {
        adjacencyList[v].push_back(u);
      }
    }
  }

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
  Graph graph(numNodes, isDirected);

  int option;
  while (true) {
    std::cout << "1. Print graph\n";
    std::cout << "2. BFS\n";
    std::cout << "3. DFS\n";
    std::cout << "4. Dijkstra's Algorithm\n";
    std::cout << "Enter option: ";
    std::cin >> option;

    switch (option) {
    case 1:
      graph.printGraph();
      break;
    case 2:
      graph.BFS(0);
      break;
    case 3:
      graph.DFS(0);
      break;
    case 4:
      graph.Dijkstra(0);
      break;
    default:
      std::cout << "Invalid option\n";
    }
  }

  return 0;
}

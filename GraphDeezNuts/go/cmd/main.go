package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"strconv"
)

var (
	// Adjacency list representation of the graph
	graph [][]int
)

type Player struct {
	name string
}

func createPlayer(name string) Player {
	return Player{name}
}

const MaxInt = int(^uint(0) >> 1)

func initGraph(numNodes int, isDirected bool) {
	graph = make([][]int, numNodes)

	for i := 0; i < numNodes; i++ {
		for j := 0; j < numNodes; j++ {
			if i != j && rand.Intn(2) == 1 {
				graph[i] = append(graph[i], j)
				if !isDirected {
					graph[j] = append(graph[j], i)
				}
			}
		}
	}
}

func printNodes() {
	for i := range graph {
		fmt.Printf("Node %d\n ", i)
	}
}

func BFS(startNode int) {
	numNodes := len(graph)
	visited := make([]bool, numNodes)
	var queue []int

	queue = append(queue, startNode)
	visited[startNode] = true

	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]

		fmt.Printf("-> Visited %d\n", node)

		for _, adjacent := range graph[node] {
			if !visited[adjacent] {
				visited[adjacent] = true
				queue = append(queue, adjacent)
			}
		}
	}
}

func DFS(startNode int) {
	visited := make([]bool, len(graph))
	runDFS(startNode, visited)
}

func runDFS(startNode int, visited []bool) {
	visited[startNode] = true
	fmt.Printf("-> Visited %d\n", startNode)

	for _, adjacent := range graph[startNode] {
		if !visited[adjacent] {
			runDFS(adjacent, visited)
		}
	}
}

func Dijkstra(startNode int) {
	numNodes := len(graph)
	dist := make([]int, numNodes)
	visited := make([]bool, numNodes)

	for i := range dist {
		dist[i] = MaxInt
	}

	dist[startNode] = 0

	for count := 0; count < numNodes-1; count++ {
		u := minDistance(dist, visited)

		visited[u] = true

		for _, v := range graph[u] {
			if !visited[v] && dist[u] != MaxInt && dist[u]+1 < dist[v] {
				dist[v] = dist[u] + 1
			}
		}
	}

	fmt.Printf("Distance from node %d to other nodes:\n", startNode)
	for i := 0; i < numNodes; i++ {
		fmt.Printf("To node %d -> %d\n", i, dist[i])
	}
}

func minDistance(dist []int, visited []bool) int {
	min := MaxInt
	minIndex := -1

	for v := 0; v < len(dist); v++ {
		if !visited[v] && dist[v] <= min {
			min = dist[v]
			minIndex = v
		}
	}

	return minIndex
}

func main() {
	var graphType string
	var numNodes int
	var studentID int = 20055710

	fmt.Println("Enter graph type (directed/undirected):")
	fmt.Scan(&graphType)

	if graphType != "directed" && graphType != "undirected" {
		fmt.Println("Invalid graph type")
		return
	}

	fmt.Println("Enter number of nodes:")
	_, err := fmt.Scan(&numNodes)
	if err != nil {
		fmt.Println("Invalid number of nodes")
		return
	}

	rand.New(rand.NewSource(int64(studentID)))

	isDirected := graphType == "directed"
	initGraph(numNodes, isDirected)

	fmt.Println("1. Print graph")
	fmt.Println("2. BFS")
	fmt.Println("3. DFS")
	fmt.Println("4. Dijkstra's Algorithm")
	fmt.Println("Enter option:")

	scanner := bufio.NewScanner(os.Stdin)
	for {
		scanner.Scan()
		input := scanner.Text()
		option, err := strconv.Atoi(input)
		if err != nil {
			fmt.Println("Invalid input, please enter a number.")
			continue
		}

		switch option {
		case 1:
			fmt.Println("Graph:")
			printNodes()

		case 2:
			fmt.Println("Enter start node:")
			scanner.Scan()
			node, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter a number.")
				continue
			}
			fmt.Printf("Running BFS from node %d:\n", node)
			BFS(node)

		case 3:
			fmt.Println("Enter start node:")
			scanner.Scan()
			node, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter a number.")
				continue
			}
			fmt.Printf("Running DFS from node %d:\n", node)
			DFS(node)

		case 4:
			fmt.Println("Enter start node:")
			scanner.Scan()
			node, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter a number.")
				continue
			}
			fmt.Printf("Running Dijkstra's Algorithm from node %d:\n", node)
			Dijkstra(node)
		default:
			fmt.Println("Invalid option")
		}
	}
}

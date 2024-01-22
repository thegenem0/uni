package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"strconv"
)

var (
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

	var runDFS func(int)
	runDFS = func(node int) {
		visited[node] = true
		fmt.Printf("-> Visited %d\n", node)

		for _, adjacent := range graph[node] {
			if !visited[adjacent] {
				runDFS(adjacent)
			}
		}
	}

	runDFS(startNode)
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
	fmt.Println("2. Add node")
	fmt.Println("3. Remove node")
	fmt.Println("4. Add edge")
	fmt.Println("5. Remove edge")
	fmt.Println("6. BFS")
	fmt.Println("7. DFS")
	fmt.Println("8. Dijkstra's Algorithm")
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
			graph = append(graph, []int{})
			fmt.Printf("Added node")

		case 3:
			fmt.Println("Enter node:")
			scanner.Scan()
			node, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter a number.")
				continue
			}
			if node >= len(graph) {
				fmt.Println("Invalid node")
				continue
			}
			graph = append(graph[:node], graph[node+1:]...)
			fmt.Printf("Removed node %d\n", node)

		case 4:
			fmt.Println("Enter node 1:")
			scanner.Scan()
			node1, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter a number.")
				continue
			}
			if node1 >= len(graph) {
				fmt.Println("Invalid node")
				continue
			}

			fmt.Println("Enter node 2:")
			scanner.Scan()
			node2, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter a number.")
				continue
			}
			if node2 >= len(graph) {
				fmt.Println("Invalid node")
				continue
			}
			graph[node1] = append(graph[node1], node2)
			if !isDirected {
				graph[node2] = append(graph[node2], node1)
			}

		case 5:
			fmt.Println("Enter node 1:")
			scanner.Scan()
			node1, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter a number.")
				continue
			}
			if node1 >= len(graph) {
				fmt.Println("Invalid node")
				continue
			}

			fmt.Println("Enter node 2:")
			scanner.Scan()
			node2, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter a number.")
				continue
			}
			if node2 >= len(graph) {
				fmt.Println("Invalid node")
				continue
			}

			for i, adjacent := range graph[node1] {
				if adjacent == node2 {
					graph[node1] = append(graph[node1][:i], graph[node1][i+1:]...)
					break
				}
			}
			if !isDirected {
				for i, adjacent := range graph[node2] {
					if adjacent == node1 {
						graph[node2] = append(graph[node2][:i], graph[node2][i+1:]...)
						break
					}
				}
			}

		case 6:
			fmt.Println("Enter start node:")
			scanner.Scan()
			node, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter a number.")
				continue
			}
			fmt.Printf("Running BFS from node %d:\n", node)
			BFS(node)

		case 7:
			fmt.Println("Enter start node:")
			scanner.Scan()
			node, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter a number.")
				continue
			}
			fmt.Printf("Running DFS from node %d:\n", node)
			DFS(node)

		case 8:
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

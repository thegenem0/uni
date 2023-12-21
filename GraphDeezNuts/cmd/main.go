package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Graph struct {
	adjacencyList map[int][]int
	directed      bool
}

func NewGraph(directed bool) *Graph {
	return &Graph{
		adjacencyList: make(map[int][]int),
		directed:      directed,
	}
}

func (self *Graph) AddNode(node int) {
	if _, exists := self.adjacencyList[node]; !exists {
		self.adjacencyList[node] = []int{}
	}
}

func (self *Graph) AddEdge(from, to int) {
	self.AddNode(from)
	self.AddNode(to)
	self.adjacencyList[from] = append(self.adjacencyList[from], to)
	if !self.directed {
		self.adjacencyList[to] = append(self.adjacencyList[to], from)
	}
}

func (self *Graph) Bfs() {
	visited := make(map[int]bool)
	var queue []int
	paths := make(map[int]int)

	for key := range self.adjacencyList {
		if !visited[key] {
			queue = append(queue, key)
			visited[key] = true
			for len(queue) > 0 {
				node := queue[0]
				queue = queue[1:]
				for _, neighbor := range self.adjacencyList[node] {
					if !visited[neighbor] {
						queue = append(queue, neighbor)
						visited[neighbor] = true
						paths[neighbor] = node
					}
				}
			}
		}
	}

	fmt.Println("\nPaths taken:")
	for dest, src := range paths {
		fmt.Printf("%d <- %d\n", dest, src)
	}
}

func (self *Graph) Dfs() {
	visited := make(map[int]bool)
	paths := make(map[int]int)
	var dfs func(node int, from int)

	dfs = func(node int, from int) {
		visited[node] = true
		if from != -1 {
			paths[node] = from
		}
		for _, neighbor := range self.adjacencyList[node] {
			if !visited[neighbor] {
				dfs(neighbor, node)
			}
		}
	}

	for key := range self.adjacencyList {
		if !visited[key] {
			dfs(key, -1)
		}
	}

	fmt.Println("\nPaths taken:")
	for dest, src := range paths {
		fmt.Printf("%d <- %d\n", dest, src)
	}
}

func (self *Graph) CheckConnection(from, to int) bool {
	visited := make(map[int]bool)
	var queue []int

	queue = append(queue, from)
	visited[from] = true

	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]
		if node == to {
			return true
		}
		for _, neighbor := range self.adjacencyList[node] {
			if !visited[neighbor] {
				queue = append(queue, neighbor)
				visited[neighbor] = true
			}
		}
	}
	return false
}

func (self *Graph) String() string {
	var builder strings.Builder
	for key, value := range self.adjacencyList {
		builder.WriteString(fmt.Sprintf("%d -> %v\n", key, value))
	}
	return builder.String()
}

func Clear() {
	// Fun CLI hack btw
	fmt.Print("\033[H\033[2J")
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	var graph *Graph = nil

	fmt.Println("Welcome to the Graph CLI!")
	for {
		fmt.Println("\nChoose an action:")
		if graph == nil {
			fmt.Println("1) Create Graph")
		} else {
			fmt.Println("1) Clear Graph")
		}
		fmt.Println("2) Add Node")
		fmt.Println("3) Add Edge")
		fmt.Println("4) Display Graph")
		fmt.Println("5) Traverse Graph")
		fmt.Println("6) Check Connection")
		fmt.Println("7) Exit")

		scanner.Scan()
		input := scanner.Text()
		option, err := strconv.Atoi(input)
		if err != nil {
			fmt.Println("Invalid input, please enter a number.")
			continue
		}

		Clear()

		switch option {
		case 1:
			if graph != nil {
				graph = nil
				fmt.Println("Graph cleared.")
				continue
			}

			fmt.Println("Enter 'directed' or 'undirected':")
			scanner.Scan()
			input := scanner.Text()
			if input == "directed" {

				graph = NewGraph(true)
				fmt.Println("Directed graph created.")
			} else if input == "undirected" {
				graph = NewGraph(false)
				fmt.Println("Undirected graph created.")
			} else {
				fmt.Println("Invalid input, please enter 'directed' or 'undirected'.")
				continue
			}
		case 2:
			fmt.Println("Enter node value:")
			scanner.Scan()
			node, err := strconv.Atoi(scanner.Text())
			if err != nil {
				fmt.Println("Invalid input, please enter an integer.")
				continue
			}
			graph.AddNode(node)
			fmt.Printf("Node %d added.\n", node)

		case 3:
			fmt.Println("Enter 'from' and 'to' node values separated by space:")
			scanner.Scan()
			nodes := strings.Fields(scanner.Text())
			if len(nodes) != 2 {
				fmt.Println("Please enter two integers separated by space.")
				continue
			}
			from, err1 := strconv.Atoi(nodes[0])
			to, err2 := strconv.Atoi(nodes[1])
			if err1 != nil || err2 != nil {
				fmt.Println("Invalid input, please enter integers.")
				continue
			}
			graph.AddEdge(from, to)
			fmt.Printf("Edge added from %d to %d.\n", from, to)

		case 4:
			fmt.Println("Current Graph:")
			fmt.Print(graph.String())

		case 5:
			fmt.Println("Choose a traversal method:")
			fmt.Println("1) Breadth First Search")
			fmt.Println("2) Depth First Search")
			scanner.Scan()
			input := scanner.Text()
			if input == "1" {
				graph.Bfs()
			} else if input == "2" {
				graph.Dfs()
			} else {
				fmt.Println("Invalid input, please enter '1' or '2'.")
				continue
			}

		case 6:
			fmt.Println("Enter 'from' and 'to' node values separated by space:")
			scanner.Scan()
			nodes := strings.Fields(scanner.Text())
			if len(nodes) != 2 {
				fmt.Println("Please enter two integers separated by space.")
				continue
			}
			from, err1 := strconv.Atoi(nodes[0])
			to, err2 := strconv.Atoi(nodes[1])
			if err1 != nil || err2 != nil {
				fmt.Println("Invalid input, please enter integers.")
				continue
			}

			if graph.CheckConnection(from, to) {
				fmt.Printf("There is a path from %d to %d.\n", from, to)
			} else {
				fmt.Printf("There is no path from %d to %d.\n", from, to)
			}

		case 7:
			fmt.Println("Exiting...")
			return

		default:
			fmt.Println("Invalid option, please choose again.")
		}
	}
}

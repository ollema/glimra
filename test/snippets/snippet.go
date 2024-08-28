package main

import "fmt"

// this is a comment
func add(a int, b int) int {
    return a + b
}

func main() {
    result := add(5, 3)
    fmt.Println("Result:", result)
}

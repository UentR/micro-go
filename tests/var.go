package main

import "fmt"

// Résultat attendu 42
func main() {
	var x, y int
	x = 1
	y = 6
	x = x + 2
	y = y * (x + 4)
	fmt.Print(y)

	var s string
	s = "Test"
	fmt.Print(s)
}

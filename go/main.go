package main

import (
	"fmt"
	"sync"
	"time"
)

// go build
// /usr/bin/time -f "%M" ./coroutines
// All goroutines finished.
// 2641924
func main() {
	numRoutines := 1_000_000

	var wg sync.WaitGroup
	for i := 0; i < numRoutines; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			time.Sleep(10 * time.Second)
		}()
	}
	wg.Wait()
	fmt.Println("All goroutines finished.")
}

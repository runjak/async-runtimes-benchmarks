package main

import (
	"fmt"
	"sync"
	"testing"
	"time"
)

// go test -benchmem -memprofile mem.out -run=^$ -bench ^BenchmarkFoo$ coroutines
// go tool pprof -http localhost:27374 ./coroutines mem.out
func BenchmarkFoo(b *testing.B) {
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

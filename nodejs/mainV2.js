const delay = () => new Promise(resolve => setTimeout(resolve, 10_000))

const runTasks = (numTasks) => {
  const tasks = new Array(numTasks);

  for (let i = 0; i < numTasks; i++) {
    tasks[i] = delay();
  }

  return tasks;
}

const numTasks = 1_000_000 // parseInt(process.argv[2]) || 10000;

Promise.all(runTasks(numTasks)).then(() => {
  console.log('All tasks completed');
}).catch((error) => {
  console.error('Error:', error);
});

// /usr/bin/time -f "%M" node ./mainV2.js 
// 506036
// /usr/bin/time -f "%M" deno run ./mainV2.js
// 1M: > 30m and continuing
// /usr/bin/time -f "%M" bun run ./mainV2.js
// 659024
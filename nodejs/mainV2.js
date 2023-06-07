const delay = (ms) => new Promise(resolve => setTimeout(resolve, ms))

const runTasks = (numTasks) => {
  const tasks = new Array(numTasks);

  for (let i = 0; i < numTasks; i++) {
    tasks[i] = delay(10000);
  }

  return tasks;
}

const numTasks = parseInt(process.argv[2]) || 10000;

Promise.all(runTasks(numTasks)).then(() => {
  console.log('All tasks completed');
}).catch((error) => {
  console.error('Error:', error);
});

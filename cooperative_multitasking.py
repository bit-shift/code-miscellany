def task_a():
    for i in range(1, 100):
        print("A: {0}".format(i))
        yield 2

def task_b():
    for i in range(1, 200):
        print("B: {0}".format(i))
        yield

def scheduler_loop(*task_generators):
    tasks = [[g(), 1] for g in task_generators]
    while tasks != []:
        for i in reversed(range(0, len(tasks))):
            if tasks[i][1] == 1:
                try:
                    tasks[i][1] = next(tasks[i][0]) or 1
                except StopIteration:
                    del tasks[i]
            else:
                tasks[i][1] -= 1

scheduler_loop(task_a, task_b)

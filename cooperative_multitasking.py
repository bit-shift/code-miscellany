def task_a():
    for i in range(1, 100):
        yield i, 2

def task_b():
    for i in range(1, 200):
        yield i

def scheduler_loop(*task_generators):
    tasks = [[g(), 1] for g in task_generators]
    while tasks != []:
        for i in reversed(range(0, len(tasks))):
            if tasks[i][1] == 1:
                try:
                    output = next(tasks[i][0])
                    if not isinstance(output, tuple):  # implicit sleep length 1
                        output = (output, 1)
                    print("{0} returned: {1}".format(
                        tasks[i][0].__name__,
                        output[0]
                        )
                    )
                    tasks[i][1] = output[1]
                except StopIteration:
                    del tasks[i]
            else:
                tasks[i][1] -= 1

scheduler_loop(task_a, task_b)

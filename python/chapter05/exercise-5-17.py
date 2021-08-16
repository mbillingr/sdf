from cps_interpreter import repl, eval_str

if __name__ == '__main__':
    with open('exercise-5-17.scm') as fd:
        eval_str(fd.read())
    repl()

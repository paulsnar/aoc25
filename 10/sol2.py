from ortools.linear_solver import pywraplp
import sys

def parse(inpline):
	parts = inpline.split(' ')
	parts = parts[1:] # remove lights
	reqs = tuple(int(x) for x in parts.pop().strip('{}').split(','))
	buttons = list()
	for btn in parts:
		buttons.append(tuple(int(x) for x in btn.strip('()').split(',')))
	return (buttons, reqs)

def main(inpline):
	solver = pywraplp.Solver.CreateSolver('SAT')
	if not solver:
		sys.exit(1)
	inf = solver.infinity()

	buttons, reqs = parse(inpline)

	fn = 0
	bvars = list()
	exprs = dict()
	for (i, btn) in enumerate(buttons):
		bvar = solver.IntVar(0, inf, f"b{i}")
		bvars.append(bvar)
		fn = fn + bvar

		for index in btn:
			if index in exprs:
				exprs[index] = exprs[index] + bvar
			else:
				exprs[index] = bvar

	for (index, expr) in exprs.items():
		cons = expr == reqs[index]
		solver.Add(cons)
	solver.Minimize(fn)

	status = solver.Solve()
	if status == pywraplp.Solver.OPTIMAL:
		print(' '.join(str(int(x.solution_value())) for x in bvars))
	else:
		sys.exit(1)

if __name__ == '__main__':
	main(sys.argv[1])


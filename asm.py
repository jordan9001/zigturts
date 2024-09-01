#!/bin/env python3
import sys

def main():
	with open("./server/src/main.zig", "r") as fp:
		src = fp.read()
	start = src.find('const TurtOp')
	end = src.find(';', start)

	sub = {x.split()[0]: int(x.split()[-1][:-1]) for x in src[start:end].split('\n') if '=' in x and ',' in x}

	if len(sys.argv) > 1 and ('-h' in sys.argv or '--help' in sys.argv):
		for k in sub:
			print(k)
		exit(0)

	while True:
		try:
			l = input()
		except EOFError:
			break

		if len(l) == 0:
			continue

		l = l.lower().strip()
	
		if l.startswith('#'):
			continue

		if len(l.split()) != 2:
			print("\nRequires <op> <imm>:", l)
			exit(-1)

		op, imm = l.split()

		found = False
		for k in sub:
			if op == k:
				print(chr(sub[k] + ord('a')), end='')
				found = True
				break

		if not found:
			print("\nUnknown op:", l)
			exit(-1)

		# expect immediate
		imm = int(imm, 0)
		if imm >= 26:
			print("\nInvalid immediate", l)
			exit(-1)
		print(chr(imm + ord('a')), end='')

	print("")

if __name__ == '__main__':
	main()


#!/usr/bin/python

import copy
import re
from functools import reduce
from operator import mul

classPattern = re.compile(
    '^(?P<clazzname>.*): (?P<leftLower>\d+)-(?P<leftUpper>\d+) or (?P<rightLower>\d+)-(?P<rightUpper>\d+)')

def parseClass(line):
    d = (classPattern.match(line).groupdict())
    return {
        'name': d['clazzname'],
        'left': {
            'lower': int(d['leftLower']),
            'upper': int(d['leftUpper']),
        },
        'right': {
            'lower': int(d['rightLower']),
            'upper': int(d['rightUpper'])
        }
    }

def parseClasses(lines):
    classes = []
    for line in lines:
        if line == "":
            return classes
        classes.append(parseClass(line))

def parseTicket(line):
    return list(map(int, line.split(',')))

def inRange(r, x):
    return x >= r['lower'] and x <= r['upper']

def invalidNums(classes, ticket):
    for num in ticket:
        invalid = [not candidate(k, num) for k in classes]
        if all(invalid):
            return num

def candidate(k, num):
    return inRange(k['left'], num) or inRange(k['right'], num)

def identifyCandidates(classes, tickets):
    candidates = {}
    for k in classes:
        name = k['name']
        for i in range(len(tickets[0])):
            if (all([candidate(k, t[i]) for t in tickets])):
                l = candidates.get(k['name'], [])
                l.append(i)
                candidates[k['name']] = l
    return candidates

def findSmallest(candidates):
    smallest = list(candidates.items())[0]
    for (k, v) in candidates.items():
        if len(v) < len(smallest[1]):
            smallest = (k,v)
    return smallest

def removeFrom(candidates, num):
    candidates = copy.deepcopy(candidates)
    for v in candidates.values():
        if num in v: v.remove(num)
    return candidates

def constrain(candidates):
    if len(candidates.items()) == 0:
        return {}
    smallest = findSmallest(candidates)
    if len(smallest[1]) == 0:
        return None # Also unreachable if there is a solution
    solution = {}
    for possibility in smallest[1]:
        solution[smallest[0]] = possibility
        newCandidates = removeFrom(candidates, possibility)
        del newCandidates[smallest[0]]
        restSol = constrain(newCandidates)
        if restSol is not None:
            return {**solution, **restSol}
    return None # Unreachable if there is a solution

def solve(lines):
    classes = parseClasses(lines)
    for k in classes:
        print(k)
    nearbyIndex = len(classes) + 5
    nearbyTickets = list(map(parseTicket, lines[nearbyIndex:]))
    myIndex = len(classes) + 2
    myTicket = parseTicket(lines[myIndex])
    invalids = [invalidNums(classes, ticket) for ticket in nearbyTickets]
    print(f'Part 1: {sum([x for x in invalids if x])}')

    validTickets = [t for t in nearbyTickets if not invalidNums(classes, t)]
    candidates = identifyCandidates(classes, validTickets)
    solution = constrain(candidates)
    indices = [x[1] for x in solution.items() if x[0].startswith('departure')]
    values = [myTicket[i] for i in indices]
    part2 = reduce(mul, values)
    print(f'Part 2: {part2}')


with open('input') as file:
    lines = file.read().strip().splitlines()
    solve(lines)
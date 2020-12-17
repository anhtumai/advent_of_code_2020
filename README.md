# Advent of code 2020

> My solution to Advent of code 2020

Link: [advent_of_code](https://adventofcode.com/2020/)

## Day 7

### Part 1

Sample Input:

```txt
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
```

About part 1, this puzzle can be viewed as a graph problem with nodes and edges.
The easiest solution is to use recursive search. Our target is __shiny gold bags__.
We find which nodes contain target, and which nodes contain nodes containing target.

### Part2

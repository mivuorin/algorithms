## Learning (Graph) algorithms

I came across [graph tutorial video](https://www.youtube.com/watch?v=tWVWeAqZ0WU) in youtube and decided to write them in F#.

[Structy.net's](https://structy.net/)  video explains graphs in really nice detail and mostly offers procedural approaches in solving problems.

I use same test examples explained in video and probably miss a lot of other tests cases.

In explaining depth and breadth first traversal they advice that breadth first is hard to do recursively because of call stack causing som problems..
This might be true in procedural language like Javascript.

In F# it was just difference of adding edges in front of the recurred list (stack) or in the end (queue).

I tried to solve problems in functional way and avoided using mutable state which means that most of the algorithms are not optimized.



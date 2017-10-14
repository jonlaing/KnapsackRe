# KnapsackRe

KnapsackRe is my attempt at making a generalized 0/1 Knapsack problem solver.
This could be useful for any project that needs to pack items that have size
into a finitely big container.

# Example

## Basic

```Reason
open Knapsack;

let items: list BasicItem.t = [
        {weight: 2, value: 1},
        {weight: 1, value: 6},
        {weight: 4, value: 3},
        {weight: 3, value: 2},
        {weight: 2, value: 3},
        {weight: 9, value: 1}
      ];

let knapsack = BasicKnapsack.make 18;

let (filled, leftovers) = BasicKnapsack.pack knapsack items;
```

## More advanced

```Reason
open Knapsack;

module Task: Item = {
    type t = {
        duration: int,
        priority: int,
        dueDate: int
    };

    let size { duration: d } => d;

    let sort task0 task1 => {
        let { duration: dur0, priority: p0, dueDate: d0 } = task0;
        let { duration: dur1, priority: p1, dueDate: d1 } = task1;

        switch ((dur0 - dur1), (priority0 - priority1), (d1 - d0)) {
            | (0,0,x) => x;
            | (0,x,_) => x;
            | (x,_,_) => x;
        }
    };
};

module TaskList = Make Task;

/* these are arbitrary input values */
let tasks: list Task.t = [
    { duration: 100, priority: 5, dueDate: 1291290 },
    { duration: 1000, priority: 3, dueDate: 930472309 },
    { duration: 2398, priority: 0, dueDate: 23907490 },
];

let emptySchedule = TaskList.make 86400;

let (filledSchedule, leftoverTasks) = TaskList.pack emptySchedule tasks;
```

# Build
```
npm run build
```

# Build + Watch

```
npm run watch
```

# Tests

```
npm run test
```

# Test + Watch

```
npm run watch:jest
```

# Editor
If you use `vscode`, Press `Windows + Shift + B` it will build automatically

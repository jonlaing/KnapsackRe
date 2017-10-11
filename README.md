# KnapsackRe

KnapsackRe is my attempt at making a generalized 0/1 Knapsack problem solver.
This could be useful for any project that needs to pack items that have size
into a finitely big container.

# Example

## Basic

```Reason
open Knapsack;

let items: list BasicItem.t = [ (1, 1), (2, 1), (3, 4), (6, 5), (7, 8) ];

let knapsack = BasicKnapsack.Sack 18 [];

let filled = BasicKnapsack.pack knapsack items;
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

        (dur0 - dur1) + (priority0 - priority1) + (d1 - d0);
    }
}

module TaskList = Make Task;

/* these are arbitrary input values */
let tasks: list Task.t = [
    { duration: 100, priority: 5, dueDate: 1291290 },
    { duration: 1000, priority: 3, dueDate: 930472309 },
    { duration: 2398, priority: 0, dueDate: 23907490 },
];

let emptySchedule = TaskList.Sack 86400 [];

let filledSchedule = TaskList.pack emptySchedule tasks;
```

# Build
```
npm run build
```

# Build + Watch

```
npm run watch
```


# Editor
If you use `vscode`, Press `Windows + Shift + B` it will build automatically

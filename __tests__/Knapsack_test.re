open Jest;

open Knapsack;

let _ =
  describe
    "Sort"
    (
      fun () =>
        describe
          "Item.sort"
          (
            fun () => {
              open Expect;
              test
                "unequal weights"
                (
                  fun () => {
                    let items: list BasicItem.t = [{weight: 2, value: 1}, {weight: 1, value: 6}];
                    let sorted = BatList.sort BasicItem.sort items;
                    switch sorted {
                    | [a, b] => expect (a.BasicItem.value, b.BasicItem.value) |> toEqual (6, 1)
                    | _ => expect (0, 0) |> toBe (1, 1) /* just make it fail */
                    }
                  }
                );
              test
                "equal weights"
                (
                  fun () => {
                    let items: list BasicItem.t = [{weight: 1, value: 1}, {weight: 1, value: 6}];
                    let sorted = BatList.sort BasicItem.sort items;
                    switch sorted {
                    | [a, b] => expect (a.BasicItem.value, b.BasicItem.value) |> toEqual (1, 6)
                    | _ => expect (0, 0) |> toBe (1, 1) /* just make it fail */
                    }
                  }
                )
            }
          )
    );

describe
  "Knapsack"
  (
    fun () => {
      open Expect;
      let items: list BasicItem.t = [
        {weight: 2, value: 1},
        {weight: 1, value: 6},
        {weight: 4, value: 3},
        {weight: 3, value: 2},
        {weight: 2, value: 3},
        {weight: 9, value: 1}
      ];
      let totalSize = List.fold_left (fun acc (i: BasicItem.t) => acc + i.weight) 0 items;
      describe
        "finding the best"
        (
          fun () =>
            test
              "gets the right index for one element"
              (
                fun () => {
                  let sack = BasicKnapsack.make 1;
                  let res = BasicKnapsack.findBest sack items;
                  switch res {
                  | Some i => expect i |> toEqual 0
                  | _ => expect 1 |> toEqual 0
                  }
                }
              )
        );
      describe
        "appending"
        (
          fun () => {
            let sack = BasicKnapsack.make 3;
            test
              "can append"
              (
                fun () => {
                  let item: BasicItem.t = {weight: 2, value: 1};
                  let res = BasicKnapsack.append sack item;
                  switch res {
                  | Some {items: [x]} => expect x |> toEqual item
                  | _ => expect item |> not_ |> toBe item
                  }
                }
              );
            test
              "can't append"
              (
                fun () => {
                  let item: BasicItem.t = {weight: 4, value: 1};
                  let res = BasicKnapsack.append sack item;
                  switch res {
                  | None => expect 0 |> toEqual 0
                  | _ => expect 0 |> not_ |> toEqual 0
                  }
                }
              )
          }
        );
      test
        "filled knapsack"
        (
          fun () => {
            let sack = BasicKnapsack.make totalSize;
            let res = BasicKnapsack.pack sack items;
            switch res {
            | (sack, []) => expect (List.length sack.items) |> toEqual (List.length items)
            | (_, _) => expect 1 |> toBe 0
            }
          }
        );
      test
        "unfilled knapsack"
        (
          fun () => {
            let sack = BasicKnapsack.make (totalSize - 5);
            let res = BasicKnapsack.pack sack items;
            switch res {
            | (sack, []) => expect (List.length sack.items) |> not_ |> toEqual (List.length items)
            | (_, _) => expect 1 |> toBe 0
            }
          }
        )
    }
  );

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

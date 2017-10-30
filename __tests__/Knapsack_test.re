open Jest;

open Knapsack;

let totalItemSize = List.fold_left((acc, i: BasicItem.t) => acc + i.weight, 0);

let _ =
  describe(
    "Sort",
    () =>
      describe(
        "Item.sort",
        () => {
          open Expect;
          test(
            "unequal weights",
            () => {
              let items: list(BasicItem.t) = [{weight: 2, value: 1}, {weight: 1, value: 6}];
              let sorted = List.sort(BasicItem.sort, items);
              switch sorted {
              | [a, b] => expect((a.BasicItem.value, b.BasicItem.value)) |> toEqual((6, 1))
              | _ => expect((0, 0)) |> toBe((1, 1)) /* just make it fail */
              }
            }
          );
          test(
            "equal weights",
            () => {
              let items: list(BasicItem.t) = [{weight: 1, value: 1}, {weight: 1, value: 6}];
              let sorted = List.sort(BasicItem.sort, items);
              switch sorted {
              | [a, b] => expect((a.BasicItem.value, b.BasicItem.value)) |> toEqual((1, 6))
              | _ => expect((0, 0)) |> toBe((1, 1)) /* just make it fail */
              }
            }
          )
        }
      )
  );

describe(
  "Knapsack",
  () => {
    open Expect;
    let unsorted: list(BasicItem.t) = [
      {weight: 2, value: 1},
      {weight: 1, value: 6},
      {weight: 4, value: 3},
      {weight: 3, value: 2},
      {weight: 2, value: 3},
      {weight: 9, value: 1}
    ];
    let items = List.sort(BasicItem.sort, unsorted);
    let totalSize = totalItemSize(items);
    test(
      "filled knapsack",
      () => {
        let sack = BasicKnapsack.make(totalSize);
        let res = BasicKnapsack.pack(sack, items);
        switch res {
        | (sack, []) => expect(List.length(sack.items)) |> toEqual(List.length(items))
        | (_, _) => expect(1) |> toBe(0)
        }
      }
    );
    test(
      "too small knapsack",
      () => {
        let sack = BasicKnapsack.make(totalSize - 5);
        let (sack, xs) = BasicKnapsack.pack(sack, items);
        expect((List.length(sack.items), List.length(xs)))
        |> not_
        |> toEqual((List.length(items), 0))
      }
    );
    test(
      "will append to partially fillded knapsack",
      () => {
        let sack = BasicKnapsack.make(totalSize + 5);
        let oneMore: BasicItem.t = {weight: 5, value: 6};
        let (newSack, _) = BasicKnapsack.pack(sack, items);
        let (final, _) = BasicKnapsack.pack(newSack, [oneMore]);
        expect(List.length(final.items)) |> toEqual(List.length(items) + 1)
      }
    );
    describe(
      "monadic and functor operations",
      () => {
        describe(
          "functor",
          () => {
            test(
              "will transform items if they fit",
              () => {
                let sack = BasicKnapsack.make(5);
                let newItem: BasicItem.t = {weight: 5, value: 1};
                let newSack = BasicKnapsack.map((items) => [newItem, ...items], sack);
                expect((newSack.size, totalItemSize(newSack.items))) |> toEqual((5, 5))
              }
            );
            test(
              "won't transform items if they don't fit",
              () => {
                let sack = BasicKnapsack.make(4);
                let newItem: BasicItem.t = {weight: 5, value: 1};
                let newSack = BasicKnapsack.map((items) => [newItem, ...items], sack);
                expect((newSack.size, totalItemSize(newSack.items))) |> toEqual((4, 0))
              }
            )
          }
        );
        describe(
          "bind",
          () => {
            test(
              "binds sacks if they fit",
              () => {
                let addItem = (i) => BasicKnapsack.map((items) => [i, ...items]);
                let sack = BasicKnapsack.make(5);
                let item0: BasicItem.t = {weight: 2, value: 1};
                let item1: BasicItem.t = {weight: 3, value: 2};
                let newSack = BasicKnapsack.(sack >>= addItem(item0) >>= addItem(item1));
                expect((newSack.size, totalItemSize(newSack.items))) |> toEqual((5, 5))
              }
            );
            test(
              "binds only as many sacks as fit",
              () => {
                let addItem = (i) => BasicKnapsack.map((items) => [i, ...items]);
                let sack = BasicKnapsack.make(4);
                let item0: BasicItem.t = {weight: 2, value: 1};
                let item1: BasicItem.t = {weight: 3, value: 2};
                let newSack = BasicKnapsack.(sack >>= addItem(item0) >>= addItem(item1));
                expect((newSack.size, totalItemSize(newSack.items))) |> toEqual((4, 2))
              }
            )
          }
        )
      }
    )
  }
);

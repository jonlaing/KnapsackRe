module type Item = {
  type t;

  /**
   * The size of the [Item]. This is used to figure out if the [Item] will fit
   * in its designated [Knapsack].
   */
  let size: t => int;

  /**
   * A function for sorting [Items] in an order conducive for packing. This
   * will change depending on the specific implementation. Sometimes it will
   * make sense to sort smallest to largest, or not, or by something completely
   * different.
   */
  let sort: t => t => int;
};

module type KnapsackF =
  (I: Item) =>
  {
    type t;

    /**
     * Append an item to the Knapsack. Returns [Some Sack] if the item
     * fits, and [None] if it doesn't.
     */
    let append: t => I.t => option t;

    /**
     * Searches an ordered list of [Items] for the next best fit. If it can't
     * find an [Item] that fits, it will return [None], otherwise it returns
     * [Some int].
     */
    let findBest: t => list I.t => option int;

    /**
     * Searches through an ordered list of [Items] and packs the best fitting
     * [Items] in, returning a type of the new [Sack] and a list of the
     * remaining [Items].
     */
    let pack: t => list I.t => (t, list I.t);
  };

module Make: KnapsackF =
  fun (I: Item) => {
    type t =
      | Sack int (list I.t);

    /** The amount of space in the [Sack] filled with [Items] */
    let filledSpace (Sack _ items) =>
      List.fold_left (fun total item => total + I.size item) 0 items;

    /** The remaining amount of space in teh [Sack] not filled with [Items] */
    let emptySpace sack => {
      let Sack size _ = sack;
      size - filledSpace sack
    };
    let willFit sack item => I.size item <= emptySpace sack;
    let append sack item => {
      let Sack size items = sack;
      let itemSize = I.size item;
      willFit sack item ? Some (Sack (size + itemSize) [item, ...items]) : None
    };

    /** A recursive definition of [findBest] that takes an index as a parameter */
    let rec findBest_ i sack items =>
      switch items {
      | [] => None
      | [x, ...xs] => willFit sack x ? Some i : findBest_ (i + 1) sack xs
      };
    let findBest sack items => findBest_ 0 sack items;
    let rec pack sack items =>
      switch (findBest sack items) {
      | Some i =>
        let item = BatList.at items i;
        let s = append sack item |> Option.default sack;
        let xs = willFit sack item ? BatList.remove_at i items : items;
        pack s xs
      | None => (sack, items)
      };
  };

module BasicItem: Item = {
  type t = (int, int);
  let size = fst;
  let sort (w0, v0) (w1, v1) => w0 - w1 + (v0 - v1);
};

module BasicKnapsack = Make BasicItem;

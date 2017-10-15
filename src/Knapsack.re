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

module type Knapsack =
  (I: Item) =>
  {
    type t = {
      size: int,
      items: list I.t
    };

    /**
     * Makes an empty [Knapsack.t] of a specified size
     */
    let make: int => t;

    /**
     * Functor map. You can only manipulate the items through this
     */
    let map: (list I.t => list I.t) => t => t;

    /**
     * Monadic bind. You only have access to the items through this.
     */
    let bind: (list I.t => t) => t => t;
    let (>>=): (list I.t => t) => t => t;
    let return: list I.t => t;

    /**
     * Append an item to the Knapsack. Returns [Some Sack] if the item
     * fits, and [None] if it doesn't.
     */
    let append: t => I.t => BatPervasives.result t t;

    /**
     * Searches an ordered list of [Items] for the next best fit. If it can't
     * find an [Item] that fits, it will return [None], otherwise it returns
     * [Some int].
     */
    let findBest: t => list I.t => option int;

    /**
     * Searches through an ordered list of [Items] and packs the best fitting
     * [Items] in, returning a type of the new [Knapsack.t] and a list of the
     * remaining [Items].
     */
    let pack: t => list I.t => (t, list I.t);
  };

module Make: Knapsack =
  fun (I: Item) => {
    type t = {
      size: int,
      items: list I.t
    };
    let make size => {size, items: []};
    let map f sack => {...sack, items: f sack.items};
    let bind (f: list I.t => t) sack => f sack.items;
    let (>>=) = bind;
    let return items => {size: List.length items, items};

    /** The amount of space in the [Knapsack.t] filled with [Items] */
    let filledSpace {items} => List.fold_left (fun total item => total + I.size item) 0 items;

    /** The remaining amount of space in teh [Knapsack.t] not filled with [Items] */
    let emptySpace sack => sack.size - filledSpace sack;

    /** Whether a new item will fit in the [Knapsack.t] */
    let willFit sack item => I.size item <= emptySpace sack;
    let append sack item =>
      willFit sack item ?
        BatPervasives.Ok {...sack, items: [item, ...sack.items]} : BatPervasives.Bad sack;

    /** A recursive definition of [findBest] that takes an index as a parameter */
    let rec findBest_ i sack items =>
      switch items {
      | [] => None
      | [x, ...xs] =>
        let fits = willFit sack x;
        fits ? Some i : findBest_ (i + 1) sack xs
      };
    let findBest sack items => findBest_ 0 sack items;

    /**
     * Plucks an item out of a list of items. It returns the item, and the
     * list of items without the plucked item
     */
    let pluckItem (items: list I.t) i => (BatList.at items i, BatList.remove_at i items);

    /**
     * Moves an item out of a list of items and into a [Knapsack.t]. Returns a
     * tuple of the new sack and items.
     */
    let packItem i sack items => {
      let (item, xs) = pluckItem items i;
      switch (append sack item) {
      | Ok s => (s, xs)
      | _ => (sack, items)
      }
    };
    let rec pack sack items =>
      switch (findBest sack items) {
      | Some i =>
        let (newSack, newItems) = packItem i sack items;
        pack newSack newItems
      | None => (sack, items)
      };
  };

module BasicItem = {
  type t = {
    weight: int,
    value: int
  };
  let size {weight} => weight;
  let sort {weight: s0, value: v0} {weight: s1, value: v1} => s0 == s1 ? v0 - v1 : s0 - s1;
};

module BasicKnapsack = Make BasicItem;

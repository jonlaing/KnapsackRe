module type Item = {
  type t;

  /**
   * The size of the [Item]. This is used to figure out if the [Item] will fit
   * in its designated [Knapsack].
   */
  let size: t => int;
  let value: t => int;

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
    let filledSpace: t => int;

    /**
     * Functor map. You can only manipulate the items through this
     */
    let map: (list I.t => list I.t) => t => t;

    /**
     * Monadic bind.
     */
    let bind: t => (t => t) => t;
    let (>>=): t => (t => t) => t;
    let return: list I.t => t;

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
    let totalItemSize = List.fold_left (fun acc i => acc + I.size i) 0;

    /** The amount of space in the [Knapsack.t] filled with [Items] */
    let filledSpace {items} => totalItemSize items;

    /** The remaining amount of space in teh [Knapsack.t] not filled with [Items] */
    let emptySpace sack => sack.size - filledSpace sack;
    let make size => {size, items: []};

    /**
     * [map] will only expose the items and will not allow a list of items bigger
     * than the size of the knapsack. So: [
     *  make 0 |> map (fun items => [anotherItem, ...items])
     * ]
     * will return an empty sack.
     */
    let map f sack => {
      let items = f sack.items;
      totalItemSize items <= sack.size ? {...sack, items} : sack
    };

    /**
     * [bind] will not allow you to make an item list larger than the size of
     * the knapsack.
     */
    let bind sack (f: t => t) => {
      let newSack = f sack;
      totalItemSize newSack.items <= newSack.size ? newSack : sack
    };
    let (>>=) = bind;
    let return items => {size: totalItemSize items, items};

    /**
     * This is figures out the maximum size that could fit in the knapsack.
     * This is the real heavy lifter of the module, and is based on the actual
     * algorithmic solution to this problem.
     */
    let rec maxFit i maxSize items =>
      switch i {
      | 0 => 0
      | i =>
        let item = List.nth items (i - 1);
        let itemSize = I.size item;
        let itemValue = I.value item;
        if (itemSize > maxSize) {
          maxFit (i - 1) maxSize items
        } else {
          max
            (maxFit (i - 1) maxSize items) (maxFit (i - 1) (maxSize - itemSize) items + itemValue)
        }
      };

    /** Memoized version of [maxFit] */
    let mem_maxFit = Util.memoize maxFit;

    /**
     * Determines the proper items to pack based on the results from [maxFit]
     */
    let rec accept i w items a =>
      switch i {
      | 0 => a
      | i =>
        let item = List.nth items (i - 1);
        let (accepted, rejected) = a;
        mem_maxFit i w items === mem_maxFit (i - 1) w items ?
          accept (i - 1) w items (accepted, [item, ...rejected]) :
          accept (i - 1) w items ([item, ...accepted], rejected)
      };
    let pack sack items => {
      let (accepted, rejected) = accept (List.length items) (emptySpace sack) items ([], []);
      ({...sack, items: List.append sack.items accepted}, rejected)
    };
  };

module BasicItem = {
  type t = {
    weight: int,
    value: int
  };
  let size {weight} => weight;
  let value {value} => value;
  let sort {weight: s0, value: v0} {weight: s1, value: v1} => s0 == s1 ? v0 - v1 : s0 - s1;
};

module BasicKnapsack = Make BasicItem;

open BatList;

module type Item = {
    type t;
    let benefit: t => (int, int);
};

module MakeKnapsack = fun (I: Item) => {
    type t = Sack int (list I.t);

    let make items => Sack (List.length items) items;

    let weight item => item |> I.benefit |> fst;

    let itemSize (Sack _ items) =>
        List.fold_left
            (fun total item => total + (fst (I.benefit item))) 0 items;

    let emptySpace sack => {
        let (Sack size _) = sack;

        size - itemSize sack;
    };

    let willFit sack item => (weight item) <= emptySpace sack;

    let append sack item => {
        let (Sack size items) = sack;
        let weight = weight item;

        willFit sack item ?
            Some(Sack (size + weight) [item, ...items]) :
            None
    };

    let rec findBest_ i sack items => 
        switch items {
        | [] => None
        | [x, ...xs] => (willFit sack x) ? Some(i) : findBest_ (i + 1) sack xs;
        };

    let findBest sack items => findBest_ 0 sack items;

    /* let rec slurp sack items =>
        switch (findBest sack items) {
        | Some i =>  {
            let s = append sack (List.nth i items);
            let xs = List.split_at i items |> fun (x, [y])
        }
        } */
        
};
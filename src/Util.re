let memoize f => {
  let table = Hashtbl.create 0;
  let g x =>
    switch (Hashtbl.find table x) {
    | exception Not_found =>
      let y = f x;
      Hashtbl.add table x y;
      y
    | y => y
    };
  g
};

type tree =
  | Leaf of int
  | Node of tree * tree

type t =
  { length : int; (* = 30 * (2 ** depth) *)
    depth : int;
    tree : tree
  }

let empty =
  { length = 30;
    depth = 0;
    tree = Leaf 0
  }

let get k t =
  let path = k / 30 in
  let rec descend p tree =
    match tree with
      | Leaf x ->
          ((x lsr (k mod 30)) land 1) <> 0
      | Node(left, right) ->
          if p land path <> 0 then
            descend (p lsr 1) right
          else
            descend (p lsr 1) left in
  if k < 0 || k > t.length then
    false
  else
    descend ((1 lsl t.depth) - 1) t.tree

let enlarge_step t =
  (* increases depth by 1 *)
  let rec build d =
    if d = 0 then
      Leaf 0
    else
      let sub = build (d-1) in
      Node(sub, sub) in
  { length = 2 * t.length;
    depth = t.depth + 1;
    tree = Node(t.tree, build t.depth);
  }

let rec enlarge_length new_length t =
  if t.length < new_length then
    enlarge_step t |> enlarge_length new_length
  else
    t

let rec enlarge_depth new_depth t =
  if t.depth < new_depth then
    enlarge_step t |> enlarge_depth new_depth
  else
    t

let bvalue =
  function
  | false -> 0
  | true -> 0x3fff_ffff

let set k value t =
  let bval = bvalue value in
  let path = k / 30 in
  let rec descend p tree =
    match tree with
      | Leaf x ->
          let m = 1 lsl (k mod 30) in
          let x' = (x land (m lxor 0x3fff_ffff)) lor (bval land m) in
          if x = x' then raise Exit;
          Leaf x'
      | Node(left, right) ->
          if p land path <> 0 then
            Node(left, descend (p lsr 1) right)
          else
            Node(descend (p lsr 1) left, right) in
  if k < 0 then
    invalid_arg "Wc_bitvector.set: negative index";
  let t = enlarge_length (k+1) t in
  let tree =
    try descend (1 lsl (t.depth - 1)) t.tree
    with Exit -> t.tree in
  { t with tree }

let union t1 t2 =
  (* perf is slightly better when t1 is the larger set of the two *)
  let d = max t1.depth t2.depth in
  let t1 = enlarge_depth d t1 in
  let t2 = enlarge_depth d t2 in
  let unchanged = ref false in
  let rec recurse tree1 tree2 =
    match tree1, tree2 with
      | Leaf x1, Leaf x2 ->
          let x = x1 lor x2 in
          if x = x1 then
            tree1
          else (
            unchanged := false;
            Leaf (x1 lor x2)
          )
      | (Node(l1, r1)), (Node(l2, r2)) ->
          let old_unchanged = !unchanged in
          unchanged := true;
          let l = recurse l1 l2 in
          let r = recurse r1 r2 in
          let unchanged_here = !unchanged in
          unchanged := old_unchanged && unchanged_here;
          if unchanged_here then
            tree1
          else
            Node(l, r)
      | _ ->
          assert false in
  { t1 with
    tree = recurse t1.tree t2.tree
  }

let elements t =
  let rec enum b x =
    if x = 0 then
      []
    else
      if x land 1 <> 0 then
        b :: enum (b+1) (x lsr 1)
      else
        enum (b+1) (x lsr 1) in
  let rec recurse b tree =
    match tree with
      | Leaf x ->
          enum (30 * b) x
      | Node(left, right) ->
          recurse (b lsl 1) left @ recurse ((b lsl 1) lor 1) right in
  recurse 0 t.tree

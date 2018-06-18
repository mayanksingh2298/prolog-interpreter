type symbol = Sym of string;;
type variable = Var of string;;
type term = V of variable | Node of symbol*(term list) | Cut | Fail;;
let signature = [(Sym "0",0);(Sym "1",0);(Sym "+",2)];;
exception NOT_UNIFIABLE;;
(* 1 *)
(* I've checked for negative arrities, repeated symbols, atleast one zero arrity*)
(* This function has y as an accumulator for symbols (only) already checked *)
(* z is a boolean that flags whether or not the program has received at least one symbol with 0 arrity*)
(* x is the remaining signature list to check. List.mem a b checks if a is in b *)
let rec check_sig_tmp x y z= match x with
  [] -> z
  |s::xs-> (match s with (a,b)->if(b<0 || List.mem a y) then false 
                                else if (b=0) then check_sig_tmp xs (a::y) true
                                else check_sig_tmp xs (a::y) (false||z));;
let check_sig x = if (List.length x = 0) then true else check_sig_tmp x [] false;;

(* 2 *)
(* There are two helper functions. helper 1 recurses over the signature set and tries to *)
(* checks if there is a matching symbol with the t's(term) symbol *)
(* helper 2 is just a predicate. for_all runs this function over the all *)
(* the argusments of this term and returns their and *)
let rec wfterm t sign = match t with
  V x -> true
 |Node (s,li)-> 
   let rec helper smallSign = (match smallSign with
     []->false
    |(a,b)::xs->if (a=s && b=List.length li) then true else helper xs) in
   let rec helper2 tmpTerm = wfterm tmpTerm sign in
   if (helper sign = false) then false else List.for_all helper2 li;;

(* 3.1 *)
let rec ht t = match t with
  V x -> 0
 |Node(s,li)-> 1 + List.fold_left max (-1) (List.map ht li);;

(* 3.2 *)
let rec size t = match t with
  V x -> 1
 |Node(s,li)-> 
   let helper x y = x+y in
   List.fold_left helper 1 (List.map size li);;

(* 3.3 *)
(* Helper 2 is used to remove duplicates *)
let rec vars t = match t with
  V x -> [x]
 |Node(s,li) -> 
   let helper x y = x@y in
   let rec helper2 lst= match lst with
     [] -> []
    |h::t -> h::(helper2 (List.filter (fun x -> x<>h) t)) in
   helper2 (List.fold_left helper [] (List.map vars li))
|_ -> [];;

(* 4 *)
(* Hash tables are used to implement substitutions *)
let sigma = Hashtbl.create 5;;
Hashtbl.add sigma (Var "x") (V(Var "y"));;
Hashtbl.add sigma (Var "y") (Node(Sym "0",[]));;
Hashtbl.add sigma (Var "z") (Node(Sym "1",[]));;

let sigma2 = Hashtbl.create 5;;
Hashtbl.add sigma2 (Var "y") (V(Var "z"));;


(* 6 *)
let rec subst t subs = match t with
  V x-> if (Hashtbl.mem subs x) then (Hashtbl.find subs x) else V x
  (* V x-> if (Hashtbl.mem subs x) then (subst (Hashtbl.find subs x) subs) else V x *)
 |Node(s,li)->
   let rec helper t' = subst t' subs in
   Node(s,List.map helper li)
 |Fail->Fail
 |Cut->Cut;;

(* 5 *)
(* Helper gives the keys in dictionary *)
(* helper2 removes the duplicates in a list *)
(* the copy of hash tables is made so that it doesn't effect the original tables *)
(* l1 is the list of keys in first hash table *)
(* final answer is getting stored in r2 *)
(* if x is mapped to a Node in s1, this substitution is added in s2 *)
(* if it gets mapped to a variable and this is final variable is present in keys of s2, then a->b->c is added in s2 *)
(* if it is not is s2, then a->b is added in s2 *)
(* ********************************************************x->f(y) and y->f(z) then x->f(f(z)) 3938 *)
let rec composeSubs_tmp s1 s2 l1= match l1 with
  []->s2
 |x::xs->(* (match (Hashtbl.find s1 x) with
            Node(a,b)->
              let tmp = Hashtbl.add s2 x (Node(a,b)) in
              composeSubs_tmp s1 s2 xs
            |V(var)-> if (Hashtbl.mem s2 var) then
              let tmp = Hashtbl.add s2 x (Hashtbl.find s2 var) in
              composeSubs_tmp s1 s2 xs
              else
              let tmp = Hashtbl.add s2 x (V(var)) in
              composeSubs_tmp s1 s2 xs
            );; *)
    let tmp = Hashtbl.find s1 x in

    let oneNewHashtbl = Hashtbl.create 1 in
    let tnp3 = Hashtbl.add oneNewHashtbl x tmp in
    let helper h = Hashtbl.fold (fun k v acc -> k :: acc) h [] in
    let keysIns2 = helper s2 in
    let rec returnNewS2 lst tbl= match lst with 
      []-> tbl
      |x'::xs'->
        let valueOfx' = Hashtbl.find s2 x' in
        match valueOfx' with
          V someVaraible->
            if (Hashtbl.mem oneNewHashtbl someVaraible) then
              let tnp1 = Hashtbl.add tbl x' (Hashtbl.find oneNewHashtbl someVaraible) in
              returnNewS2 xs' tbl
            else
              let tnp1 = Hashtbl.add tbl x' (Hashtbl.find s2 x') in
              returnNewS2 xs' tbl 
          |_ -> 
              let tnp1 = Hashtbl.add tbl x' (Hashtbl.find s2 x') in
              returnNewS2 xs' tbl in
    let emptyTable = Hashtbl.create 10 in
    let s2 = returnNewS2 keysIns2 emptyTable in

    let tmp2 = Hashtbl.add s2 x (subst tmp s2) in
    composeSubs_tmp s1 s2 xs;;

let rec composeSubs s1 s2 = 
  let helper h = Hashtbl.fold (fun k v acc -> k :: acc) h [] in
  let rec helper2 lst= match lst with
     [] -> []
    |h::t -> h::(helper2 (List.filter (fun x -> x<>h) t)) in
  let s1' = Hashtbl.copy s1 in
  let s2' = Hashtbl.copy s2 in
  composeSubs_tmp s1' s2' (helper2 (helper s1'));;


(* 7 *)
(* proceeded exactly the way we did in class *)
(* case 1 (x,x) *)
(* case 2 (x,node). if not inside the variables of node, just map it else raise error *)
(* case 3 (node,x). if not inside the variables of node, just map it else raise error *)
(* case 4 (node,node). if same symbol implement as we did in class*)
let rec mgu t1 t2 = match (t1,t2) with
   (V a, V b)->      
      let unif = Hashtbl.create 5 in
      if a=b then unif 
      else 
        let tmp = Hashtbl.add unif a (V(b)) in
      unif
  |(V a, Node(s,li))-> if (List.mem a (vars t2)) then raise NOT_UNIFIABLE
                       else
                         let unif = Hashtbl.create 5 in
                         let tmp = Hashtbl.add unif a (Node(s,li)) in
                        unif
  |(Node(s,li), V a)-> if (List.mem a (vars t1)) then raise NOT_UNIFIABLE
                       else
                         let unif = Hashtbl.create 5 in
                         let tmp = Hashtbl.add unif a (Node(s,li)) in
                        unif

  |(Node(s1,li1),Node(s2,li2))-> if (s1<>s2) then raise NOT_UNIFIABLE
                                 else
                                   let rec helper l1 l2 func = match (l1,l2) with
                                                       ([],[])->func
                                                      |(p::ps,q::qs)->helper ps qs (composeSubs func (mgu (subst p func) (subst q func))) in
                                   let unif = Hashtbl.create 5 in
                                   helper li1 li2 unif
  |_->raise NOT_UNIFIABLE;;
(* Clause is a rule, with a heaf and a body *)
(* Atomic formula is same as term *)
type clause = { head: term; body: term list };;
type program = Program of clause list;;
type goal = Goal of term list;;
type answer = { res: bool; table: ((variable, term) Hashtbl.t)};;
(* let newtbl = Hashtbl.create 5;; *)
let pg1 = Program ([
			{head=
				Node(Sym "edge",[Node(Sym "a",[]);Node(Sym "b",[])]);
			body=[
			]};
			{head=
				Node(Sym "edge",[Node(Sym "b",[]);Node(Sym "c",[])]);
			body=[
			]};
			{head=
				Node(Sym "edge",[Node(Sym "c",[]);Node(Sym "d",[])]);
			body=[
			]};
			{head=
				Node(Sym "edge",[Node(Sym "c",[]);Node(Sym "e",[])]);
			body=[
			]};
			{head=
				Node(Sym "path",[V(Var "S");V(Var "S")]);
			body=[
			]}; 
			{head=
				Node(Sym "path",[V(Var "X");V(Var "Y")]);
			body=[
				Node(Sym "edge",[V(Var "X");V(Var "Z")]);
				Node(Sym "path",[V(Var "Z");V(Var "Y")]);
			]};
			

		]);;
let pg2 = Program ([
			{head=
				Node(Sym "f",[V(Var "X")]);
			body=[
				Node(Sym "g",[V(Var "X")]);
				Cut
			]};
			{head=
				Node(Sym "g",[Node(Sym "1",[])]);
			body=[
			]};
			{head=
				Node(Sym "g",[Node(Sym "2",[])]);
			body=[
			]};
			

		]);;
let pgMohit = Program ([
			{head=
				Node(Sym "f",[Node(Sym "1",[])]);
			body=[
			]};
			{head=
				Node(Sym "g",[Node(Sym "2",[])]);
			body=[
			]};
			

		]);;

let pg3 = Program ([
			{head=
				Node(Sym "meal",[V(Var "X")]);
			body=[
				Node(Sym "food",[V(Var "X")]);
			]};
			{head=
				Node(Sym "food",[Node(Sym "burger",[])]);
			body=[
			]};
			{head=
				Node(Sym "food",[Node(Sym "sandwich",[])]);
			body=[
			]};
			{head=
				Node(Sym "food",[Node(Sym "pizza",[])]);
			body=[
			]};
			{head=
				Node(Sym "lunch",[Node(Sym "sandwich",[])]);
			body=[
			]};
			{head=
				Node(Sym "dinner",[Node(Sym "pizza",[])]);
			body=[
			]};

			
		]);;
let term1 =Node(Sym "path",[V(Var "P");V(Var "Q")]);;
let term2 =Node(Sym "f",[V(Var "X")]);;
let term3 =Node(Sym "meal",[V(Var "P")]);;
let term4 =Node(Sym "dinner",[V(Var "Q")]);;
let termMohit1 =Node(Sym "f",[V(Var "P")]);;
let termMohit2 =Node(Sym "g",[V(Var "P")]);;


(* query is the list of terms (goals)
stack is a kind of dump
program is the running clauses
programFull is the full code always
currTbl is the currnt Hashtbl to be used by anyone
ans stores true or false
tbl is the final table Hashtbl
Hashtbl
originalVars = vars(query[0])
finalAnswer is the list of list of final answers *)
let rec run (query, stack, program, programFull, currTbl, (ans,tbl), originalVars, finalAnswer)= 
	match query with
		[]-> 
			(match stack with
				[]-> 
					let mergedTable = composeSubs currTbl tbl in
					let getHash x= Hashtbl.find mergedTable x in
					let incrementList = List.map getHash originalVars in
					(true,mergedTable,finalAnswer@[incrementList])
				|(query',program',(ans',tbl'))::stack'-> 
					let mergedTable = composeSubs currTbl tbl in
					let getHash x= Hashtbl.find mergedTable x in
					(* let getHash x= subst (V(x)) mergedTable in *)
					let incrementList = List.map getHash originalVars in
					run (query', stack', program', programFull, tbl', (true,mergedTable), originalVars, (finalAnswer@[incrementList]))
			)
		|Cut::qs-> (* run (qs, [], program, programFull, currTbl, (ans,tbl), originalVars, finalAnswer) *)
					(match stack with
						[]-> run (qs, [], program, programFull, currTbl, (ans,tbl), originalVars, finalAnswer)
						|(query',program',(ans',tbl'))::stack'-> 
							if List.mem Cut query' then
								run (Cut::qs, stack', program, programFull, currTbl, (ans,tbl), originalVars, finalAnswer)
							else
								run (qs, stack', program, programFull, currTbl, (ans,tbl), originalVars, finalAnswer)
					)
		|Fail::qs->	(match stack with
						[]-> (ans,tbl,finalAnswer)
						|(query',program',(ans',tbl'))::stack'-> 
						run (query', stack', program', programFull, tbl', (ans',tbl), originalVars, finalAnswer)
					)
		|q::qs->
			(match program with
				Program([])-> (match stack with
						[]-> (ans,tbl,finalAnswer)
						|(query',program',(ans',tbl'))::stack'-> 
						run (query', stack', program', programFull, tbl', ((ans' || ans),tbl), originalVars, finalAnswer)
					)
				|Program(cl::cls)->
					try
						let table = mgu q cl.head in
						let foo x = subst x table in
						let substitutedBody = List.map foo cl.body in
						let substitutedQs = List.map foo qs in
						let tbl2 = composeSubs table currTbl in
						let newStack = (query,Program(cls),(ans,currTbl))::stack in
						let newQuery = substitutedBody@substitutedQs in
						run (newQuery, newStack, programFull, programFull, tbl2, (ans,tbl), originalVars, finalAnswer)
					with NOT_UNIFIABLE-> run (query, stack, (Program(cls)), programFull, currTbl, (ans,tbl), originalVars, finalAnswer)

			);;


(* Sample:
let a = run [term1] [] pg1 pg1 newtbl (false,newtbl) (vars term1) [];;
 *)
(* programFull always stores the initial full copy of the program *)
(* tbl is an initially empty table, used to store values of variables accross function calls *)
(* tmp_tmp is an initially empty table, used to preserve the table, because when a body sub goal(not first) fails, we have to run the code further with the original table *)
(* let rec runTerm t program programFull tbl tbl_tmp=
	(* let emptyHashtbl = Hashtbl.create 5 in *)
	match program with
		Program([])-> {res=false;table=tbl} (** nothing to search more**)
		|Program(cl::cls)->
			try 
				let table = mgu t cl.head in
				let tbl2 = composeSubs table tbl in
				(match cl.body with
					[]->{res=true;table=tbl2} (* body checked fully *)
					|x::xs-> 
						let substitutedTerm = subst x table in (* substitute X in side the first element of body *)
						let substitutedTermResult = runTerm substitutedTerm programFull programFull tbl2 tbl_tmp in (* right now we are totally solving the first element, and checkung next *)
						if (substitutedTermResult).res = true then
							let foo x = subst x substitutedTermResult.table in
							let substitutedBody = List.map foo xs in
							runTerm t (Program({head=cl.head;body=substitutedBody}::cls)) programFull substitutedTermResult.table tbl_tmp
						else  
							runTerm t (Program(cls)) programFull tbl_tmp tbl_tmp

						(* ---- *)
						(* Kuch karke yahe pe, sare clauses pe search karo, and then dhund ke baaki code pe run karo *)
						let rec helper t' program = 
							match program with
								Program([])-> runTerm t (Program(cls)) programFull tbl_tmp tbl_tmp (** nothing to search more**)
								|Program(cl'::cls')->
									try
										let substitutedTermResult = runTerm substitutedTerm Program([cl']) programFull tbl2 tbl_tmp in
										if (substitutedTermResult).res = true then
											let foo x = subst x substitutedTermResult.table in
											let substitutedBody = List.map foo xs in
											runTerm t (Program({head=cl.head;body=substitutedBody}::cls)) programFull substitutedTermResult.table tbl_tmp
										else
											helper t' cls'
									with
									| NOT_UNIFIABLE -> helper t' cls'


						
						(* ---- *)
						
				)
			with NOT_UNIFIABLE-> runTerm t (Program(cls)) programFull tbl tbl_tmp;; *)(* if not matched, go to next clause *)

(* let rec printProgram Program(x)=match x with
	[] -> flush stdout
	 *)

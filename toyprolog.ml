open A6

let program = 
    let filename = Sys.argv.(1) in 
    let file = open_in filename in
    let lexbuf = Lexing.from_channel file in
    let rec createProgram acc = 
      let result = Parser.main Lexer.token lexbuf in
        match result with x::xs->
        (match x with 
        	(Node(Sym "file_end",[]),[]) -> Program acc
       		 | (a,b) -> 
       		 (createProgram (acc@[{head=a;body=b}]))
        ) 
      in 
    (* (printTable(createTable [])); *)
    (createProgram [])
;;

let printSimpleTerm t = match t with
	V(Var x)->Printf.printf "%s " x;flush stdout;
	|Node (Sym x,[])->Printf.printf "%s " x;flush stdout;
;;


let _ = 
	Printf.printf "?-"; flush stdout;
	let lexbuf = Lexing.from_channel stdin in
		while true do
			let result = Parser.main Lexer.token lexbuf in
			let rec getGoals res acc= match res with
				[]->acc
				|(goal,[])::xs->
				(* Printf.printf "debug \n"; flush stdout; *)
				getGoals xs (acc@[goal])
				(* |_-> Printf.printf "INVALID INPUT GOAL\n";Printf.printf "\n?-"; flush stdout; *)
			in
			let goals = getGoals result [] in
			let tmpterm = Node(Sym "getvars",goals) in
			let variables = vars tmpterm in
			let newtbl = Hashtbl.create 10 in
			let a = run (goals,[],program,program,newtbl,(false,newtbl),variables,[]) in
			let rec finishGoal ans = match ans with
				(boolean,table,[])->
					(match boolean with 
						true -> Printf.printf "true.\n?-";flush stdout;
						|false -> Printf.printf "false.\n?-";flush stdout;
					)
				|(boolean,table,[[]])->
					(match boolean with 
						true -> Printf.printf "true.\n?-";flush stdout;
						|false -> Printf.printf "false.\n?-";flush stdout;
					)
				|(boolean,table,x::xs)->
					let rec printAnswers v a= match (v,a) with
						([],[])->Printf.printf "\n";flush stdout;
						|(v'::restv,a'::resta)->
							printSimpleTerm v';
							Printf.printf "= ";flush stdout;
							printSimpleTerm a';
							printAnswers restv resta
					in
					
					let rec foo lis acc = match lis with
						[]->acc
						|(x::xs)-> foo xs (acc@[V(x)])	
					in
					let vvariables = foo variables [] in
					printAnswers vvariables x;
					let result' = Parser.main Lexer.token lexbuf in
					(match result' with
						[(Node(Sym "semicolon",[]),[])]->finishGoal (boolean,table,xs)
						|_->finishGoal (boolean,table,x::xs)
					)
			in
			finishGoal a
				(* (true,a,b)->Printf.printf "true.\n?-";flush stdout; *)
				(* |(false,a,b)->Printf.printf "false.\n?-";flush stdout; *)
		done
;;

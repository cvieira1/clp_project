open AvaliaSintaxe;;

(* mostra a lista *)
let rec print_list = function
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l;;

(* Função que transforma string em lista tendo cada um dos caracteres *)
let explode s =
	let rec exp i l =
		if i<0 then l else exp (i - 1) (s.[i] :: l) in
	exp (String.length s -1) []

(* VERIFICA SE É LETRA MINÚSCULA *)
let e_variavel_min c = ((int_of_char(c) >= int_of_char('a')) && (int_of_char(c) <= int_of_char('z')));;

(* VERIFICA SE É LETRA MAIÚSCULA *)
let e_variavel_max c = ((int_of_char(c) >= int_of_char('A')) && (int_of_char(c) <= int_of_char('Z')));;

(* VERIFICA SE É DÍGITO *)
let e_digito c = let zero = int_of_char('0') in 
	int_of_char(c) - zero >= 0 && int_of_char(c) - zero <= 9;;

(* !!!!INICIO FUNCAO CAIO!!!! *)
(* Divide String em Lista *)
let verifica_sintaxeFun = fun s lista ->
	  let tamanho = String.length s in
		let lista_var = ref [] in
		let var = ref "" in
		for i = 0 to (tamanho - 1) do
			(* Se o respectivo char for 1 desses especificados sera adicionado direto a lista *)
			if (String.get s i) = '(' || (String.get s i) = ')' || (String.get s i) = ',' then
				begin
				lista_var := (Char.escaped(String.get s i) :: !lista_var);
				end
			else
				begin
				(* Se for um espaço a variavel sera a mesma *)
				if (String.get s i) = ' ' then
					var := !var 
				(* Se não for 1 espaço a variavel sera concatenada com o respectivo char *)	
				else	
			 	  var := !var ^ Char.escaped(String.get s i);						
				if(i+1) < tamanho then 
					begin
					(* Se o proximo char for 1 dos especificados a variavel sera adicionada a lista e ela sera 'zerada' *)	
			    if (String.get s (i+1)) = '(' || (String.get s (i+1)) = ')' || (String.get s (i+1)) = ',' || (String.get s (i+1)) = ' ' then
					  begin
						if (String.get s i) = ' ' then
							begin
							lista_var := !lista_var;
							end
						else	
							begin 		
				      lista_var := (!var :: !lista_var);
					    var := "";
							end
					  end
					end;
				if (i+1) = tamanho then
					begin
				   lista_var := (!var :: !lista_var);					
					end
				end;		
		done;
		lista := List.rev !lista_var;;


(*Função que converte uma lista normal numa lista de TOKENS*)
let token = fun lista listaAritmetico listaLogico listaPalavraChave listaComparativo lista_final ->
	let tamanho = List.length !lista in
	let resposta_int = ref false in
	let resposta_rac = ref false in
	let regexp_div = Str.regexp "[:]" in
	let rodada1 = ref true in
	let resposta_aritmetico = ref '0' in
	let resposta_logico = ref '0' in
	let resposta_comparativo = ref '0' in
	let resposta_palavraChave = ref '0' in
	for i = 0 to (tamanho - 1) do
		verifica_int (List.nth !lista i) resposta_int;
		verifica (List.nth !lista i) listaAritmetico resposta_aritmetico;
		verifica (List.nth !lista i) listaLogico resposta_logico;
		verifica (List.nth !lista i) listaComparativo resposta_comparativo;
		verifica (List.nth !lista i) listaPalavraChave resposta_palavraChave;
		let tamanho_string = String.length (List.nth !lista i) in
		for j = 0 to (tamanho_string-1) do
		if (Str.string_match regexp_div (List.nth !lista i) j) then
			begin
		  AvaliaSintaxe.verifica_rac (List.nth !lista i) resposta_rac;
			if !resposta_rac then
				rodada1 := false;
			end;
		done;	
		rodada1 := true;
		if !resposta_aritmetico = 's' then
			begin
				lista_final := ("<opAritmetico,"^(List.nth !lista i)^">") :: !lista_final;
			end;	
		if !resposta_logico = 's' then
			begin
				lista_final := ("<opLogico,"^(List.nth !lista i)^">") :: !lista_final;
			end;	
		if !resposta_comparativo = 's' then
			begin
				lista_final := ("<opComparativo,"^(List.nth !lista i)^">") :: !lista_final;
			end;	
		if !resposta_palavraChave = 's' then
			  begin
				lista_final := (List.nth !lista i) :: !lista_final;
				end			
		else
			begin		
		if (ehVariavel (List.nth !lista i) 0 0 true) then
			  begin
				if i = 0 then
					begin	
				  lista_final := ("<fun,"^(List.nth !lista i)^">") :: !lista_final;
					end
				else
					begin
				  lista_final := ("<var,"^(List.nth !lista i)^">") :: !lista_final;						
					end
				end;
			end;	
		if (List.nth !lista i) = "(" then	
			  begin
				lista_final := "<sep,abreParenteses>" :: !lista_final;
				end;
		if (List.nth !lista i) = ")" then	
			  begin
				lista_final := "<sep,fechaParenteses>" :: !lista_final;
				end;
		if (List.nth !lista i) = "," then	
			  begin
				lista_final := "<sep,virgula>" :: !lista_final;
				end;
		if !resposta_int then	
			  begin
				lista_final := ("<int,"^(List.nth !lista i)^">") :: !lista_final;
				end;
		if !resposta_rac then	
			  begin
				lista_final := ("<rac,"^(List.nth !lista i)^">") :: !lista_final;
				end;
		resposta_rac := false;		
	done;
  lista_final := List.rev !lista_final;;

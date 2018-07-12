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
	let resposta_cadeia = ref false in
	let resposta_int = ref false in
	let resposta_rac = ref false in
	let resposta_valLogico = ref false in
	let regexp_div = Str.regexp "[:]" in
	let resposta_aritmetico = ref '0' in
	let resposta_logico = ref '0' in
	let resposta_comparativo = ref '0' in
	let resposta_palavraChave = ref '0' in
	for i = 0 to (tamanho - 1) do
		verifica_cadeia (List.nth !lista i) resposta_cadeia;
		verifica_int (List.nth !lista i) resposta_int;
		verifica_logico (List.nth !lista i) resposta_valLogico;
		verifica (List.nth !lista i) listaAritmetico resposta_aritmetico;
		verifica (List.nth !lista i) listaLogico resposta_logico;
		verifica (List.nth !lista i) listaComparativo resposta_comparativo;
		verifica (List.nth !lista i) listaPalavraChave resposta_palavraChave;
		let tamanho_string = String.length (List.nth !lista i) in
		for j = 0 to (tamanho_string-1) do
		if (Str.string_match regexp_div (List.nth !lista i) j) then
			begin
		  verifica_rac (List.nth !lista i) resposta_rac;
			end;
		done;
	
		if !resposta_aritmetico = 's' then
			begin
				lista_final := ("<aplAritmetica,"^(List.nth !lista i)^","^(List.nth !lista (i-1))^","^(List.nth !lista (i+1))^">") :: !lista_final;
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
		if not(!resposta_valLogico) then		
		  if (ehVariavel (List.nth !lista i) 0 0 true) then
			  begin
				if i < (tamanho - 1) then
					begin		
				  if (List.nth !lista (i+1)) = "(" then
					  begin	
				    lista_final := ("<fun,"^(List.nth !lista i)^">") :: !lista_final;
					  end
					else	
					  begin
				    lista_final := ("<var,"^(List.nth !lista i)^">") :: !lista_final;						
					  end
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
		if (List.nth !lista i) = ":=" then	
			  begin
				lista_final := ("<aplAtr,"^(List.nth !lista (i-1))^","^(List.nth !lista (i+1))^">") :: !lista_final;
				end;
		if !resposta_valLogico then
			  begin
				lista_final := ("<bol,"^(List.nth !lista i)^">") :: !lista_final;
				end;	
		if !resposta_cadeia then
			  begin
				lista_final := ("<cad,"^(List.nth !lista i)^">") :: !lista_final;	
				end			
	done;
  lista_final := List.rev !lista_final;;

(* Funçao para trocar o valor de um elemento de uma lista *)
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;

(* Funçao para remover elemento de uma lista *)
let remove l num  = List.filter (fun x -> x != num) l;;
																																																																																
(* Funçao que aplica a funçao para atribuir um valor a uma variavel *)
let aplAtr = fun variavel valor listaVariavel listaValor listaTipo ->
		let resposta_var = ref 'n' in
	  let resposta_cadeia = ref false in
	  let resposta_int = ref false in
	  let resposta_rac = ref false in
		let tamanho_valor = String.length valor in
	  let regexp_div = Str.regexp "[:]" in
		let tamanho_lista_var = (List.length !listaVariavel) in
		let aplica = ref false in
    if tamanho_lista_var > 0 then 
			begin
			aplica := verifica variavel !listaVariavel resposta_var;
			end;
		verifica_cadeia valor resposta_cadeia;
		verifica_int valor resposta_int;
		for j = 0 to (tamanho_valor-1) do
		  if (Str.string_match regexp_div valor j) then
			  begin
		    verifica_rac valor resposta_rac;
			  end;
		done;
		if !resposta_var = 'n' then
			begin
	    listaVariavel := !listaVariavel@[variavel];
	    listaValor := !listaValor@[valor];
		  if !resposta_int then
			  listaTipo := !listaTipo@["inteiro"];
		  if !resposta_rac then
			  listaTipo := !listaTipo@["racional"];
		  if valor = "true" || valor = "false" then
			  listaTipo := !listaTipo@["booleano"];
      if !resposta_cadeia then
			  listaTipo := !listaTipo@["cadeia"];
			end
		else
			begin
		  let var_indice = ref 0 in                                          
			pega_indice variavel !listaVariavel var_indice;
			listaValor := replace !listaValor !var_indice valor;
		  if !resposta_int then
			  listaTipo := replace !listaTipo !var_indice "inteiro";
		  if !resposta_rac then
			  listaTipo := replace !listaTipo !var_indice "racional";
		  if valor = "true" || valor = "false" then
			  listaTipo := replace !listaTipo !var_indice "booleano";
      if !resposta_cadeia then
			  listaTipo := replace !listaTipo !var_indice "cadeia";
		  end;;

(* Calcula potencia entre inteiros *)
let potencia = fun x n ->
	let total = ref x in
	for i = 0 to (n-2) do
		total := !total * x;
	done;
	if n = 0 then total := 1;
	!total;;	

let aplAritmetica = fun operador fator1 fator2 total ->
	  let separa_rac = Str.regexp ":" in
	  let resposta_int_fator1 = ref false in
	  let resposta_int_fator2 = ref false in
	  let resposta_rac_fator1 = ref false in
	  let resposta_rac_fator2 = ref false in
		let resposta_cadeia_fator1 = ref false in
		let resposta_cadeia_fator2 = ref false in
		let int_total = ref 0 in
		let tamanho_fator1 = String.length fator1 in 
		let tamanho_fator2 = String.length fator2 in 
		verifica_int fator1 resposta_int_fator1;
		verifica_int fator2 resposta_int_fator2;
		for j = 0 to (tamanho_fator1-1) do
		if (Str.string_match separa_rac fator1 j) then
			begin
		  verifica_rac fator1 resposta_rac_fator1;
			end;
		done;
		for j = 0 to (tamanho_fator2-1) do
		if (Str.string_match separa_rac fator2 j) then
			begin
		  verifica_rac fator2 resposta_rac_fator2;
			end;
		done;
		verifica_cadeia fator1 resposta_cadeia_fator1;
		verifica_cadeia fator2 resposta_cadeia_fator2;	
		if !resposta_cadeia_fator1 || !resposta_cadeia_fator2 then
			begin
				let aspas_regexp = Str.regexp "\"" in
				total := "\"" ^ (Str.global_replace aspas_regexp "" fator1) ^ (Str.global_replace aspas_regexp "" fator2) ^ "\"";
			end
		else
			begin		
		  if !resposta_int_fator1 && !resposta_int_fator2 then
			  begin
	      if operador = "+" then
			    int_total := (int_of_string fator1) + (int_of_string fator2);
	      if operador = "-" then
			    int_total := (int_of_string fator1) - (int_of_string fator2);
	      if operador = "*" then
			    int_total := (int_of_string fator1) * (int_of_string fator2);
	      if operador = "/" then
			    int_total := (int_of_string fator1) / (int_of_string fator2);
	      if operador = "%" then
			    int_total := (int_of_string fator1) mod (int_of_string fator2);
	      if operador = "^" then    
				  begin                                        
	   	    int_total := potencia (int_of_string fator1) (int_of_string fator2);
				  if (int_of_string fator2) = 0 then int_total := 1;
				  end;
			  total := (string_of_int) !int_total;
        end
		  else
			  begin	
			  let new_fator1 = ref "" in
			  let new_fator2 = ref "" in
			  new_fator1 := fator1;
			  new_fator2 := fator2;	
		    if !resposta_int_fator1 then
				  begin
				  new_fator1 := fator1 ^ ":1";
				  end;
		    if !resposta_int_fator2 then
				  begin
				  new_fator2 := fator2 ^ ":1";
				  end;
		    let lista_fator1 = Str.split separa_rac !new_fator1 in
			  let lista_fator2 = Str.split separa_rac !new_fator2 in
			  let num_fator1 = List.nth lista_fator1 0 in
			  let den_fator1 = List.nth lista_fator1 1 in
			  let num_fator2 = List.nth lista_fator2 0 in
			  let den_fator2 = List.nth lista_fator2 1 in
			  let a = (int_of_string num_fator1) in
			  let b = (int_of_string den_fator1) in
			  let c = (int_of_string num_fator2) in
			  let d = (int_of_string den_fator2) in
			  let num_total = ref 0 in
			  let den_total = ref 0 in
	      if operador = "+" then
				  begin
			    num_total := (a*d) + (b*c);
				  den_total := b*d;
				  end;
	      if operador = "-" then
				  begin
			    num_total := (a*d) - (b*c);
				  den_total := b*d;
				  end;
	      if operador = "*" then
				  begin
			    num_total := a*c;
				  den_total := b*d;
				  end;
	      if operador = "/" then
				  begin
			    num_total := a*d;
				  den_total := b*c;				
				  end;
			  total := (string_of_int !num_total) ^ ":" ^ (string_of_int !den_total);	
 		    end;
			end;;
	
(* Verifica se string contem uma substring *)
let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false		
						
(* Funçao que executa as funcoes*)
let rec executaAplicacoes = fun lista_final listaVariaveis listaValores listaTipos var_atribuida->
	let tamanho_lista_final = (List.length !lista_final) in
  let listaLogico = ["!";"&";"@"] in
  let listaAritmetico = ["+";"-";"*";"/";"%";"^"] in
  let listaComparativo = ["<";">";"="] in 
  let listaPalavraChave = ["if";"or"] in
	let separa_token = Str.regexp "," in
  for i = 0 to (tamanho_lista_final - 1) do
		if not (!var_atribuida) then
			begin
	    let tamanho_original = String.length (List.nth !lista_final i) in
	    let lista_token = ref (Str.split separa_token (String.sub (List.nth !lista_final i) 1 (tamanho_original - 2))) in
	    if (List.nth !lista_token 0) = "aplAritmetica" then
			  begin
			  let total = ref "" in
			  let list_total = ref [] in
			  let token_total = ref [] in
	      if contains (List.nth !lista_final (i-1)) "var" then
			    begin
			    let var_indice1 = ref 0 in
			    let valor1 = (List.nth !listaValores !var_indice1) in
				  lista_token := replace !lista_token 2 valor1;
				  end
				else
					begin
	        let tamanho_original_anterior = String.length (List.nth !lista_final (i-1)) in
	        let lista_token_anterior = ref (Str.split separa_token (String.sub (List.nth !lista_final (i-1)) 1 (tamanho_original_anterior - 2))) in
					lista_token := replace !lista_token 2	(List.nth !lista_token_anterior 1);
					end;						
	      if contains (List.nth !lista_final (i+1)) "var" then
			    begin
			    let var_indice2 = ref 0 in
			    let valor2 = (List.nth !listaValores !var_indice2) in
				  lista_token := replace !lista_token 3 valor2;
				  end
				else
					begin
	        let tamanho_original_proximo = String.length (List.nth !lista_final (i+1)) in
	        let lista_token_proximo = ref (Str.split separa_token (String.sub (List.nth !lista_final (i+1)) 1 (tamanho_original_proximo - 2))) in
					lista_token := replace !lista_token 3	(List.nth !lista_token_proximo 1);
					end;						
			  aplAritmetica (List.nth !lista_token 1) (List.nth !lista_token 2) (List.nth !lista_token 3) total;
			  list_total := !total :: !list_total;
			  token list_total listaAritmetico listaLogico listaPalavraChave listaComparativo token_total;
			  lista_final := replace !lista_final (i-1) (List.hd !token_total);
			  lista_final := remove !lista_final (List.nth !lista_final i);
			  lista_final := remove !lista_final (List.nth !lista_final i);
			  executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
			  end;
			end;	
  done;
  for i = 0 to (tamanho_lista_final - 2) do
		if not (!var_atribuida) then
			begin
	    let tamanho_original = String.length (List.nth !lista_final i) in
	    let tamanho_original2 = String.length (List.nth !lista_final (i+1)) in
		  let lista_token_final = ref [] in
	    let lista_token = Str.split separa_token (String.sub (List.nth !lista_final i) 1 (tamanho_original - 2)) in
	    let lista_token2 = Str.split separa_token (String.sub (List.nth !lista_final (i+1)) 1 (tamanho_original2 - 2)) in
	    if (List.nth lista_token 0) = "aplAtr" then
			  begin
			  lista_token_final := replace lista_token 2 (List.nth lista_token2 1);
		    aplAtr (List.nth !lista_token_final 1) (List.nth !lista_token_final 2) listaVariaveis listaValores listaTipos;
				var_atribuida := true;
			  end;
			end;	
  done;;

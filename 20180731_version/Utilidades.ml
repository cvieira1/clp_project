open AvaliaSintaxe;;

(* algoritmo de Euclides para achar o MDC *)				
let rec mdc_euclides num dem =
	let dividendo = num in
	let divisor = dem in
	if divisor = 0 then begin 
		dividendo
	end				  	 	
	else begin
		mdc_euclides divisor (dividendo mod divisor);
	end;;	

(* Verifica se string contem uma substring *)
let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false		

(* Funçao que verifica se a lista de tokens contem um *)
(* token com os seguintes opradores: * , / , % , ^ e o retorna*)						
let rec exists_1ordem l = match l with 
   | [] -> "" 
	 | h::t -> if Str.string_match (Str.regexp ".+\*,.+\|.+/,.+\|.+%,.+\|.+^,.+")	h 0 then h			
    else exists_1ordem t;;

(* Funçao que verifica se a lista de tokens contem um *)
(* token com os seguintes opradores: + , - e o retorna*)						
let rec exists_2ordem l = match l with 
   | [] -> "" 
	 | h::t -> if Str.string_match (Str.regexp ".+\+,.+\|.+-,.+")	h 0 then h			
    else exists_2ordem t;;

(* Funçao que verifica se a lista de tokens contem um *)
(* token com aspas dupla(caracterisica de uma cadeia) e o retorna*)						
let rec exists_cadeia l = match l with 
   | [] -> "" 
	 | h::t -> if Str.string_match (Str.regexp ".+\".+")	h 0 then h			
    else exists_cadeia t;;

(* Funçao que verifica se a lista de tokens contem um *)
(* token com a palavra passada como argumento e o retorna*)						
let rec exists l palavra= match l with 
   | [] -> false 
	 | h::t -> if Str.string_match (Str.regexp (".+"^palavra^".+"))	h 0 then true			
    else exists t palavra;;


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
				if (i+1) = tamanho && !var <> "" then
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
		resposta_cadeia := false;
		resposta_int := false;
		resposta_valLogico := false;
		resposta_aritmetico := '0';
		resposta_logico := '0';
		resposta_comparativo := '0';
		resposta_palavraChave := '0';
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
				lista_final := !lista_final@[("<aplAritmetica,"^(List.nth !lista i)^","^(List.nth !lista (i-1))^","^(List.nth !lista (i+1))^">")];
			end;				
		if !resposta_logico = 's' then
			begin
				lista_final := !lista_final@[("<opLogico,"^(List.nth !lista i)^">")];
			end;	
		if !resposta_comparativo = 's' then
			begin
				lista_final := !lista_final@[("<opComparativo,"^(List.nth !lista i)^">")];
			end;	
		if !resposta_palavraChave = 's' then
			  begin
				lista_final := !lista_final@[(List.nth !lista i)];
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
				    lista_final := !lista_final@[("<fun,"^(List.nth !lista i)^">")];
					  end
					else	
					  begin
						if (String.get (List.nth !lista i) 0) = '-' then
							begin
				      lista_final := !lista_final@[("<int,0>")];						
				      lista_final := !lista_final@[("<aplAritmetica,-,0,"^(String.sub (List.nth !lista i) 1 (String.length (List.nth !lista i)))^">")];						
				      lista_final := !lista_final@[("<var,"^(String.sub (List.nth !lista i) 1 (String.length (List.nth !lista i)))^">")];						
							end	
						else
							begin
				      lista_final := !lista_final@[("<var,"^(List.nth !lista i)^">")];						
						  end;
					  end
					end
				else	
					begin
						if (String.get (List.nth !lista i) 0) = '-' then
							begin
				      lista_final := !lista_final@[("<int,0>")];						
				      lista_final := !lista_final@[("<aplAritmetica,-,0,"^(String.sub (List.nth !lista i) 1 (String.length (List.nth !lista i)))^">")];						
				      lista_final := !lista_final@[("<var,"^(String.sub (List.nth !lista i) 1 (String.length (List.nth !lista i)))^">")];						
							end	
						else
							begin
				      lista_final := !lista_final@[("<var,"^(List.nth !lista i)^">")];						
						  end;
					end	
				end;
			end;	
	  if i < (tamanho - 1) then
			begin
   		if (List.nth !lista (i+1)) = "(" then
				begin	
		    if not (ehVariavel (List.nth !lista i) 0 0 true) then
					begin
					Printf.printf "Error de sintaxe: Nome de funcao com caracteres invalidos";
					exit 0;
					end;
				end;
			end;		
		if (List.nth !lista i) = "(" then	
			  begin
				lista_final := !lista_final@["<sep,abreParenteses>"];
				end;
		if (List.nth !lista i) = ")" then	
			  begin
				lista_final := !lista_final@["<sep,fechaParenteses>"];
				end;		
		if exists !lista_final "fun" && exists !lista_final "abreParenteses" then
			begin		
		  if contains	(List.nth !lista_final (i-1)) "sep" && (List.nth !lista i) = "," then
			begin	
				Printf.printf "Error de sintaxe: Separador em posicao invalida";
				exit 0;
			end
			else
			begin 
				if (List.nth !lista i) = "," then
				lista_final := !lista_final@["<sep,virgula>"];
			end;			
			if i < (tamanho - 1) then
   	  if (List.nth !lista (i+1)) = ":=" || List.nth !lista (i-1) = "(" || (List.nth !lista (i-1) = "," && List.nth !lista (i+1) = ",") || (List.nth !lista (i+1)) = ")" then
				begin	
		    if not (ehVariavel (List.nth !lista i) 0 0 true) then
					begin
					Printf.printf "Error de sintaxe: Nome de variavel com caracteres invalidos";
					exit 0;
					end;
				end;	
			end;		
		if !resposta_int then	
			  begin
				lista_final := !lista_final@[("<int,"^(List.nth !lista i)^">")];
				end;
		if contains (List.nth !lista i) ":" && (List.nth !lista i) <> ":=" then		
			begin
		  if !resposta_rac then	
			  begin
				lista_final := !lista_final@[("<rac,"^(List.nth !lista i)^">")];
				end
			else	
			  begin
			  Printf.printf "Error de sintaxe: Racional em formato incorreto";		
			  exit 0;
			  end;
		  end;	
		resposta_rac := false;		
		if i < (tamanho - 1) then
			begin
		  if (List.nth !lista i) = ":=" then
				begin	
			  if (String.get (List.nth !lista (i+1)) 0) = '-' then
					begin
				  let teste_var = (String.sub (List.nth !lista (i+1)) 1 (String.length (List.nth !lista (i+1))-1)) in
				  if (ehVariavel teste_var 0 0 true) then
						begin
				    lista_final := !lista_final@[("<aplAtr,"^(List.nth !lista (i-1))^","^(teste_var)^">")];					
						lista_final := !lista_final@[("<trocaSinal,"^(teste_var)^">")];
						end
				  else	
			      begin
				    lista_final := !lista_final@[("<aplAtr,"^(List.nth !lista (i-1))^","^(List.nth !lista (i+1))^">")];
				    end;
					end
				else
					begin
				  lista_final := !lista_final@[("<aplAtr,"^(List.nth !lista (i-1))^","^(List.nth !lista (i+1))^">")];
					end;			
				end;		
			end;			
		if !resposta_valLogico then
			  begin
				lista_final := !lista_final@[("<bol,"^(List.nth !lista i)^">")];
				end;	
		if !resposta_cadeia then
			  begin
				lista_final := !lista_final@[("<cad,"^(List.nth !lista i)^">")];	
				end			
	done;;

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
	  let regexp_div = Str.regexp ":" in
		let tamanho_lista_var = (List.length !listaVariavel) in
		let aplica = ref false in
		let valor_rac = ref "" in
		let tipo_atual = ref "" in
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
		if !resposta_int then
			tipo_atual := "inteiro";
		if !resposta_rac then
			tipo_atual := "racional";
		if valor = "true" || valor = "false" then
			tipo_atual := "booleano";
    if !resposta_cadeia then
			tipo_atual := "cadeia";
		if !resposta_rac then
			begin
			let real_valor = ref valor in	
			if (String.get valor 0) = '-' then
				begin
		    let lista_rac = Str.split regexp_div (String.sub !real_valor 1 (String.length !real_valor -1)) in
			  let num_valor = int_of_string(List.nth lista_rac 0) in
			  let dem_valor = int_of_string(List.nth lista_rac 1) in
			  let mdc_valor = mdc_euclides num_valor dem_valor in
			  valor_rac := "-"^string_of_int(num_valor / mdc_valor) ^ ":" ^ string_of_int(dem_valor / mdc_valor);
				end
			else	
				begin
		    let lista_rac = Str.split regexp_div !real_valor in
			  let num_valor = int_of_string(List.nth lista_rac 0) in
			  let dem_valor = int_of_string(List.nth lista_rac 1) in
			  let mdc_valor = mdc_euclides num_valor dem_valor in
			  valor_rac := string_of_int(num_valor / mdc_valor) ^ ":" ^ string_of_int(dem_valor / mdc_valor);
				end;
			end;	
		if !resposta_var = 'n' then
			begin
	    listaVariavel := !listaVariavel@[variavel];
			if not !resposta_rac then
				begin
	      listaValor := !listaValor@[valor];
				end
			else
				begin
	      listaValor := !listaValor@[!valor_rac];
				end;		
			listaTipo := !listaTipo@[!tipo_atual];
			end
		else
			begin
		  let var_indice = ref 0 in                                          
			pega_indice variavel !listaVariavel var_indice;
			let tipo_anterior = (List.nth !listaTipo !var_indice) in
			if (!resposta_int && tipo_anterior = "racional") || (tipo_anterior = !tipo_atual) then	
			begin
			listaValor := replace !listaValor !var_indice valor;
			listaTipo := replace !listaTipo !var_indice !tipo_atual;
			end
			else
			begin
				Printf.printf "\nError de conversao: Conversao de tipo de variavel incorreto";
				exit 0;
			end;
			end;;

(* Calcula potencia entre inteiros *)
let potencia = fun x n ->
	let total = ref x in
	for i = 0 to (n-2) do
		total := !total * x;
	done;
	if n = 0 then total := 1;
	!total;;	

(* Funçao que executa aplicao a partir de um operador aritmetico *)
(* entre dois operandos *)
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
				if operador = "+" then
				begin	
				let aspas_regexp = Str.regexp "\"" in
				total := "\"" ^ (Str.global_replace aspas_regexp "" fator1) ^ (Str.global_replace aspas_regexp "" fator2) ^ "\"";
				end
				else
				begin
				Printf.printf "Error: Operador nao compativel com tipo de dado!";
				exit 0;
				end;		
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
				let mdc_total = ref 0 in
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
				if !num_total < 0 || !den_total < 0 then
					begin
					let new_num_total = ref (abs !num_total) in
					let new_den_total = ref (abs !den_total) in
				  mdc_total := mdc_euclides	!new_num_total !new_den_total;
				  num_total := !new_num_total / !mdc_total;
				  den_total := !new_den_total / !mdc_total;
			    total := "-"^(string_of_int !num_total) ^ ":" ^ (string_of_int !den_total);	
					end
				else
					begin
				  mdc_total := mdc_euclides	!num_total !den_total;
				  num_total := !num_total / !mdc_total;
				  den_total := !den_total / !mdc_total;
			    total := (string_of_int !num_total) ^ ":" ^ (string_of_int !den_total);	
					end		
 		    end;
			end;;
	
(* Função que aplica a troca de sinal de uma variavel e retorna *)
(* o valor da variavel com o sinal trocado*)	
let trocaSinal_inteiro = fun valor_antigo valor_novo ->
		valor_novo := valor_antigo * -1;;

let trocaSinal_racional = fun valor_antigo valor_novo ->
		aplAritmetica "*" "-1" valor_antigo valor_novo;;

(* Funçao que executa as funcoes*)
let rec executaAplicacoes = fun lista_final listaVariaveis listaValores listaTipos var_atribuida->
	let tamanho_lista_final = (List.length !lista_final) in
	let ind_op1 = ref 0 in
	let ind_op2 = ref 0 in
	let ind_opCadeia = ref 0 in
  let listaLogico = ["!";"&";"@"] in
  let listaAritmetico = ["+";"-";"*";"/";"%";"^"] in
  let listaComparativo = ["<";">";"="] in 
  let listaPalavraChave = ["if";"or"] in
	let separa_token = Str.regexp "," in
	let ind1 = exists_1ordem !lista_final in
	let ind2 = exists_2ordem !lista_final in
	let ind_cadeia = exists_cadeia !lista_final in
	pega_indice ind1 !lista_final ind_op1;
	pega_indice ind2 !lista_final ind_op2;
	pega_indice ind_cadeia !lista_final ind_opCadeia;
	(* Percorrer a minha lista com os tokens *)
  for i = 0 to (tamanho_lista_final - 1) do
		if not (!var_atribuida) then
			begin
	    let tamanho_original = String.length (List.nth !lista_final i) in
	    let lista_token = ref (Str.split separa_token (String.sub (List.nth !lista_final i) 1 (tamanho_original - 2))) in
			if (List.nth !lista_token 0) = "trocaSinal" then
				begin
			  let total = ref "" in
			  let list_total = ref [] in
			  let token_total = ref [] in
				let var_indice = ref 0 in
				let valor_novo_int = ref 0 in
				let valor_novo_rac = ref "" in
				pega_indice (List.nth !lista_token 1) !listaVariaveis var_indice;
				let verifica_tipo = (List.nth !listaTipos !var_indice) in
				if verifica_tipo = "inteiro" || verifica_tipo = "racional" then
					begin
					if verifica_tipo = "inteiro" then
						begin
				    let valor_antigo = int_of_string(List.nth !listaValores !var_indice) in	
				    trocaSinal_inteiro valor_antigo valor_novo_int;
			      list_total := string_of_int(!valor_novo_int) :: !list_total;
						end;
					if verifica_tipo = "racional" then
						begin
				    let valor_antigo = List.nth !listaValores !var_indice in	
				    trocaSinal_racional valor_antigo valor_novo_rac;
			      list_total := !valor_novo_rac :: !list_total;
						end;
			    token list_total listaAritmetico listaLogico listaPalavraChave listaComparativo token_total;
				  lista_final := replace !lista_final i (List.hd !token_total);
					end
				else
					begin
				  Printf.printf "Error: Operador nao compativel com tipo de dado!";
				  exit 0;
					end;
				end;
	    if (List.nth !lista_token 0) = "aplAritmetica" then
			  begin
			  let total = ref "" in
			  let list_total = ref [] in
			  let token_total = ref [] in
				(* Se o termo anterior do meu token atual for uma variavel *)
				(* deve pegar o valor dela e trocar no token atual *)
	      if contains (List.nth !lista_final (i-1)) "var" then
			    begin
			    let var_indice1 = ref 0 in
				  pega_indice (List.nth !lista_token 1) !listaVariaveis var_indice1;
			    let valor1 = (List.nth !listaValores !var_indice1) in
				  lista_token := replace !lista_token 2 valor1;
				  end
				(* Se não	tem que atualizar o valor no token atual *)
				else
					begin
	        let tamanho_original_anterior = String.length (List.nth !lista_final (i-1)) in
	        let lista_token_anterior = ref (Str.split separa_token (String.sub (List.nth !lista_final (i-1)) 1 (tamanho_original_anterior - 2))) in
					lista_token := replace !lista_token 2	(List.nth !lista_token_anterior 1);
					end;						
				(* Se o termo seguinte do meu token atual for uma variavel *)
				(* deve pegar o valor dela e trocar no token atual *)
	      if contains (List.nth !lista_final (i+1)) "var" then
			    begin
			    let var_indice2 = ref 0 in
			    let valor2 = (List.nth !listaValores !var_indice2) in
				  lista_token := replace !lista_token 3 valor2;
				  end
				(* Se não	tem que atualizar o valor no token atual *)
				else
					begin
	        let tamanho_original_proximo = String.length (List.nth !lista_final (i+1)) in
	        let lista_token_proximo = ref (Str.split separa_token (String.sub (List.nth !lista_final (i+1)) 1 (tamanho_original_proximo - 2))) in
					lista_token := replace !lista_token 3	(List.nth !lista_token_proximo 1);
					end;						
				if !ind_opCadeia > 0 then	
					begin
			      aplAritmetica (List.nth !lista_token 1) (List.nth !lista_token 2) (List.nth !lista_token 3) total;
			      list_total := !total :: !list_total;
			      token list_total listaAritmetico listaLogico listaPalavraChave listaComparativo token_total;
			      lista_final := replace !lista_final (i-1) (List.hd !token_total);
			      lista_final := remove !lista_final (List.nth !lista_final i);
			      lista_final := remove !lista_final (List.nth !lista_final i);
			      executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
					end
				else		
					begin
				  if !ind_op1 != -1 then 
					  begin
					  if i = !ind_op1 then
					    begin
			        aplAritmetica (List.nth !lista_token 1) (List.nth !lista_token 2) (List.nth !lista_token 3) total;
			        list_total := !total :: !list_total;
			        token list_total listaAritmetico listaLogico listaPalavraChave listaComparativo token_total;
			        lista_final := replace !lista_final (i-1) (List.hd !token_total);
			        lista_final := remove !lista_final (List.nth !lista_final i);
			        lista_final := remove !lista_final (List.nth !lista_final i);
			        executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
					    end;
					  end	
				  else
						begin
						if !ind_op2 != -1 then		
					    begin
					    if i = !ind_op2 then
					      begin
			          aplAritmetica (List.nth !lista_token 1) (List.nth !lista_token 2) (List.nth !lista_token 3) total;
			          list_total := !total :: !list_total;
			          token list_total listaAritmetico listaLogico listaPalavraChave listaComparativo token_total;
			          lista_final := replace !lista_final (i-1) (List.hd !token_total);
			          lista_final := remove !lista_final (List.nth !lista_final i);
			          lista_final := remove !lista_final (List.nth !lista_final i);
			          executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
							  end;
					    end;
						end;
					end	
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

(* VERIFICACAO DE EXPRESSAO BOOLEANA *)

(* verifica se é booleano *)
let eh_booleano input = 
	let regex = "!?(*!?true)*$\\|!?(*!?false)*$"in
	Str.string_match (Str.regexp regex) input 0

(* transforma um operador lógico da linguagem L para um operador lógico da linguagem ocaml *)
let equivalente_operador input = 
	match input with
	| "&" -> "&&"
	| "@" -> "||"
	| "!" -> "not"
	| _ -> ""

																	(* AVALIASINTAXE FIM *)

																								(* UTILIDADES *)
(* conta quantos termos da lista aparecem na string *)		
let count_element_List entrada lista =
	let cont = ref 0 in
	let char_list = explode entrada in
	let c = ref " " in
	for indice=0 to ((List.length char_list)-1) do
		c := (String.make 1 (List.nth char_list indice));
		for i=0 to ((List.length lista)-1) do
			if !c = (List.nth lista i) then begin
				cont := !cont + 1;
			end;
		done;
	done;
	if !cont > 0 then begin 
		!cont 
	end
	else begin 
		0 
	end;;

				
(* retorna o respectivo valor lógico dependendo do operador onde n1 e n2 são inteiros *)		
let exp_logica_inteiro n1 operador n2 =
	let r = ref false in 
	if operador = "=" then begin
		r := (n1 = n2)
	end
	else if operador = ">" then begin
		r := (n1>n2)
	end
	else if operador = "<" then begin
		r := (n1<n2)
	end;
	!r;;


(* transforma uma string em um valor boolean *)
let exp_logica_bool boolean = 
	(* verifica se há unário *)
	let temNegacao = (Str.string_match (Str.regexp ".*!.*") boolean 0) in
	let indice = ref (-1) in
	let temp = ref "" in
	let retorno = ref false in
	
	(* caso tenha unário eu retiro ele e transformo para o not da linguagem L e aplico esse not*)
	if temNegacao then begin
		for i=0 to ((String.length boolean)-1) do
			if String.make 0 (String.get boolean i) = "!" then begin
				indice := i;
			end;		
		done;
		if !indice <> (-1) then begin
			temp := Str.global_replace (Str.regexp "!") " " boolean;
			temp := String.trim !temp;
			retorno := not(bool_of_string(!temp));
		end;
		!retorno
	end
	else begin
		(* caso contrário, ele é um booleano 'simples' *)
		retorno := bool_of_string(boolean);
		!retorno;
	end;;				
				
(* comparação entre racionais *)
(* recebe o numerador e o denominador de ambos os racionais juntamente com o operador e retorna se a operação é verdadeira ou falsa *)	
let exp_logica_racional num1 dem1 operador num2 dem2 =
	let n1 = ref num1 in
	let d1 = ref dem1 in
	let n2 = ref num2 in
	let d2 = ref dem2 in
	
	let mdc1 = mdc_euclides num1 dem1 in
	n1 := num1/mdc1;
	d1 := dem1/mdc1;
	
	let mdc2 = mdc_euclides num2 dem2 in
	
	n2 := num2/mdc2;
	d2 := dem2/mdc2;
	
	let r = ref (-1) in 
	if (operador = "=") && ((!n1 = !n2) && (!d1 = !d2))then begin
		r := 1
	end
	else if operador = ">" && ((!n1 * !d2) > (!n2 * !d1)) then begin	
		r := 1
	end
	else if operador = "<" && ((!n1 * !d2) < (!n2 * !d1)) then begin
		r := 1
	end;
	
	if !r = 1 then begin
		true
	end
	else
		false;;		


(* recebe duas expressões e um operador e retorna verdadeiro ou falso dependendo da relação entre os operadores *)
(* exemplo de uso: exp_logica "10" ">" "20" -> retorno = false *)
(* AVISO: trata casos simples de expressão booleana !(!true) dará erro *)
let exp_logica e1 op e2 =
	let operador = ref (String.trim(op)) in
	
	(* essas variáveis podem não ser utilizados *)
	let valorInteiroAntes = ref 0 in
	let valorBoolAntes = ref false in
	
	let valorInteiroDepois = ref 0 in
	let valorBoolDepois = ref false in
	
	let numerador_Rac_Antes = ref (-1) in
	let denominador_Rac_Antes = ref (-1) in
	
	let numerador_Rac_Depois = ref (-1) in
	let denominador_Rac_Depois = ref (-1) in
	
	let tipoAntes = ref "" in
	let tipoDepois = ref "" in
	(* fim das variáveis temporárias *)
	
	(* r pega a resposta da expressão avalia int e entrou é apenas uma variável de controle*)
	let r = ref false in
	let entrou = ref false in 
	
	verifica_int e1 r;
	if !r = true && not (!entrou) then begin
		(* se entrar aqui é inteiro a expressão antes *)
		valorInteiroAntes := int_of_string(e1); 
		tipoAntes := "int";
		entrou := true;	
	end
	else
		(* a função verifica_rac dá pau se não tiver : *)
		if (Str.string_match (Str.regexp "[1-9][0-9]*:[1-9][0-9]*$") e1 0) then begin
			verifica_rac e1 r; 
		end;
		if eh_booleano e1 then begin
			valorBoolAntes := exp_logica_bool e1;
			tipoAntes := "bool";
			entrou := true;
		end
		else if not (!entrou) && !r = true then begin 
			(* separando os termo antes e depois do :*)
			let listaTemp = (Str.split (Str.regexp ":") e1) in
			numerador_Rac_Antes := int_of_string(List.nth listaTemp 0);
			denominador_Rac_Antes := int_of_string(List.nth listaTemp 1);
			entrou := true;
			tipoAntes := "rac";
		end;			
	
	(* reinicializando os valores *)
	entrou := false;
	r := false;
	
	verifica_int e2 r;
	if !r = true && not (!entrou) then begin
		valorInteiroDepois := int_of_string(e2);
		entrou := true;	
		tipoDepois := "int";
	end
	else
		if (Str.string_match (Str.regexp "[1-9][0-9]*:[1-9][0-9]*$") e2 0) then begin
			verifica_rac e2 r; 
		end; 
		if not (!entrou) && eh_booleano e2 then begin
			valorBoolDepois := exp_logica_bool e2;
			entrou := true;
			tipoDepois := "bool";
		end	
		else if not (!entrou) &&  !r = true then begin
			let listaTemp = (Str.split (Str.regexp ":") e2) in
			numerador_Rac_Depois := int_of_string(List.nth listaTemp 0);
			denominador_Rac_Depois := int_of_string(List.nth listaTemp 1);
			entrou := true;
			tipoDepois := "rac";
		end;	
	
	let resposta = ref false in
	let entrouFinal = ref 0 in
	
	if !tipoAntes = "int" && !tipoDepois = "int" then begin
		resposta := exp_logica_inteiro !valorInteiroAntes !operador !valorInteiroDepois;
		entrouFinal := 1;	
	end
	else 
		if !tipoAntes = "bool" && !tipoDepois = "bool" && !operador = "=" then begin
			resposta := (!valorBoolAntes = !valorBoolDepois);
			entrouFinal := 1;	
		end
		else if !tipoAntes = "rac" && !tipoDepois = "rac" then begin
			resposta := exp_logica_racional !numerador_Rac_Antes !denominador_Rac_Antes !operador !numerador_Rac_Depois !denominador_Rac_Depois;
			entrouFinal := 1;
		end
		else if (!tipoAntes = "int" && !tipoDepois = "rac") || (!tipoAntes = "rac" && !tipoDepois = "int") then begin 	
			(* se entrar aqui o tipoAntes é rac ou o tipoDepois é *)
			if !tipoAntes = "int" then begin
				resposta := exp_logica_racional !valorInteiroAntes 1 !operador !numerador_Rac_Depois !denominador_Rac_Depois;
				entrouFinal := 1;
			end
			else begin 
				resposta := exp_logica_racional !numerador_Rac_Antes !denominador_Rac_Antes !operador !valorInteiroDepois 1;
				entrouFinal := 1;
			end												 			
		end;
		
	if !entrouFinal <> 0 then begin
		!resposta
	end
	else
		raise Exit;;	

open AvaliaSintaxe;;

(* verifica se � booleano *)
let eh_booleano input = 
	let regex = "!?(*!?true)*$\\|!?(*!?false)*$"in
Str.string_match (Str.regexp regex) input 0

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

(* Fun�ao que verifica se a lista de tokens contem um *)
(* token com os seguintes opradores: * , / , % , ^ e o retorna*)						
let rec exists_1ordem l = match l with 
   | [] -> "" 
	 | h::t -> if Str.string_match (Str.regexp ".+\*,.+\|.+/,.+\|.+%,.+\|.+^,.+")	h 0 then h			
    else exists_1ordem t;;

(* Fun�ao que verifica se a lista de tokens contem um *)
(* token com os seguintes opradores: + , - e o retorna*)						
let rec exists_2ordem l = match l with 
   | [] -> "" 
	 | h::t -> if Str.string_match (Str.regexp ".+\+,.+\|.+-,.+")	h 0 then h			
    else exists_2ordem t;;

(* Fun�ao que verifica se a lista de tokens contem um *)
(* token com aspas dupla(caracterisica de uma cadeia) e o retorna*)						
let rec exists_cadeia l = match l with 
   | [] -> "" 
	 | h::t -> if Str.string_match (Str.regexp ".+\".+")	h 0 then h			
    else exists_cadeia t;;

(* Fun�ao que verifica se a lista de tokens contem um *)
(* token com a palavra passada como argumento e o retorna*)						
let rec exists l palavra= match l with 
   | [] -> false 
	 | h::t -> if Str.string_match (Str.regexp (".+"^palavra^".+"))	h 0 then true			
    else exists t palavra;;


(* mostra a lista *)
let rec print_list = function
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l;;

let rec print_list_file oc = function
[] -> ()
| e::l -> Printf.fprintf oc "%s" e ; print_list_file oc l;;

(* Fun��o que transforma string em lista tendo cada um dos caracteres *)
let explode s =
	let rec exp i l =
		if i<0 then l else exp (i - 1) (s.[i] :: l) in
	exp (String.length s -1) []

(* VERIFICA SE � LETRA MIN�SCULA *)
let e_variavel_min c = ((int_of_char(c) >= int_of_char('a')) && (int_of_char(c) <= int_of_char('z')));;

(* VERIFICA SE � LETRA MAI�SCULA *)
let e_variavel_max c = ((int_of_char(c) >= int_of_char('A')) && (int_of_char(c) <= int_of_char('Z')));;

(* VERIFICA SE � D�GITO *)
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
				(* Se for um espa�o a variavel sera a mesma *)
				if (String.get s i) = ' ' then
					var := !var 
				(* Se n�o for 1 espa�o a variavel sera concatenada com o respectivo char *)	
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


(*Fun��o que converte uma lista normal numa lista de TOKENS*)
let token = fun lista listaAritmetico listaLogico listaPalavraChave listaComparativo lista_final ->
  let arquivo2 = "erro.txt" in
  let oc2 = open_out arquivo2 in  
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
	(* Percorrer cada lexema da lista *)
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
		(* Percorre cada caractere de lexema *)
		for j = 0 to (tamanho_string-1) do	
		(* Verifica se o lexema contem : *)
		(* Se tiver verifica se eh um racional *)	
		if (Str.string_match regexp_div (List.nth !lista i) j) then
			begin
		  verifica_rac (List.nth !lista i) resposta_rac;
			end;
		done;
		if i > 0 then
			begin
		(* Se for um operador aritmetico se torna uma aplica��o aritmetica*)
		  if !resposta_aritmetico = 's' then
			  begin
				  lista_final := !lista_final@[("<aplAritmetica,"^(List.nth !lista i)^","^(List.nth !lista (i-1))^","^(List.nth !lista (i+1))^">")];
			  end;				
		(* Se for um operador comparativo ou logico se torna uma *)
		(* aplica��o booleana*)
      if !resposta_comparativo = 's' || !resposta_logico = 's' then
			  begin
				  lista_final := !lista_final@[("<aplBooleana,"^(List.nth !lista i)^","^(List.nth !lista (i-1))^","^(List.nth !lista (i+1))^">")];
			  end;
			end;
		(* Se for uma palavra-chave mantem o lexema *)	
			if !resposta_palavraChave = 's' then
			  begin
				lista_final := !lista_final@[(List.nth !lista i)];
				end			
		  else
			  begin
				(* Se n�o for true ou false *)			
		    if not (!resposta_valLogico) then
					(* Se tiver nos parametros de variavel *)		
					begin
		      if (ehVariavel (List.nth !lista i) 0 0 true) then
			      begin	
				    if i < (tamanho - 1) then
					    begin
		          (* Se o proximo for ( eh uma fun��o *)
				      if (List.nth !lista (i+1)) = "(" then
					      begin	
				        lista_final := !lista_final@[("<fun,"^(List.nth !lista i)^">")];
					      end
					    else	
					      begin	
				        lista_final := !lista_final@[("<var,"^(List.nth !lista i)^">")];						
						    end;
					    end;
						end;	
				end;
			end;	
		if i < (tamanho - 1) && (i > 0) then
			begin
		  if (List.nth !lista i) = ":=" then
				begin	
				(* Se o proximo tiver o primeiro caractere '-' *)	
			  if (String.get (List.nth !lista (i+1)) 0) = '-' then
					begin
				  let teste_var = (String.sub (List.nth !lista (i+1)) 1 (String.length (List.nth !lista (i+1))-1)) in
					(* Verifca se eh uma variavel *)
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
	  if i < (tamanho - 1) then
			begin
   		if (List.nth !lista (i+1)) = "(" then
				begin	
				(* Verifica se n�o eh esta nos parametros de variavel *)	
		    if not (ehVariavel (List.nth !lista i) 0 0 true) then
					begin
					if (!resposta_aritmetico = '0') && (!resposta_logico = '0') && (!resposta_comparativo = '0') && ((List.nth !lista i)  <> ":=") then
					  begin	
					  Printf.printf "Error de sintaxe: Nome de funcao com caracteres invalidos";
					  Printf.fprintf oc2 "Error de sintaxe: Nome de funcao com caracteres invalidos\n";
					  exit 0;
					  end;
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
		if i > 0 then
			begin				
		  if contains	(List.nth !lista_final (i-1)) "sep" && (List.nth !lista i) = "," then
			  begin	
		    Printf.printf "Error de sintaxe: Separador em posicao invalida";
			  exit 0;
			  end;
			if (contains (List.nth !lista_final (i-1)) "fun") && ((List.nth !lista i) = "(") && (List.nth !lista (i+1)) = ")" then
				begin	 	
				Printf.printf "Erro de sintaxe: Argumentos nao fornecidos";
				exit 0;
				end;	
		  end;
		if (List.nth !lista i) = "," then
			begin	
			lista_final := !lista_final@["<sep,virgula>"];
			end;
		if i < (tamanho - 1) then
			begin
   	  if (List.nth !lista (i+1)) = ":=" || ((List.nth !lista (i+1)) = ",") || ((List.nth !lista (i+1)) = ")") then
				begin	
		    if not (ehVariavel (List.nth !lista i) 0 0 true) && not !resposta_int && not !resposta_cadeia && not !resposta_valLogico && not !resposta_rac then
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
		if !resposta_valLogico then
			  begin
				lista_final := !lista_final@[("<bol,"^(List.nth !lista i)^">")];
				end;	
		if !resposta_cadeia then
			  begin
				lista_final := !lista_final@[("<cad,"^(List.nth !lista i)^">")];	
				end			
	done;;

(* Fun�ao para trocar o valor de um elemento de uma lista *)
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;

(* Fun�ao para remover elemento de uma lista *)
let remove l num  = List.filter (fun x -> x != num) l;;
																																																																																
(* Fun�ao para remover elemento de certo indice de uma lista *)
let rec new_remove pos = function
	| [] -> []
	| h :: t -> if pos = 0 then t else
		h :: new_remove (pos - 1) t;;

(* Fun�ao que aplica a fun�ao para atribuir um valor a uma variavel *)
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

(* Fun�ao que executa aplicao a partir de um operador aritmetico *)
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
	
(* Fun��o que aplica a troca de sinal de uma variavel e retorna *)
(* o valor da variavel com o sinal trocado*)	
let trocaSinal_inteiro = fun valor_antigo valor_novo ->
		valor_novo := valor_antigo * -1;;

let trocaSinal_racional = fun valor_antigo valor_novo ->
		aplAritmetica "*" "-1" valor_antigo valor_novo;;

(* verifica se � o la�o de repeti��o DO *)
let eh_do lista_entrada = 
	let tamanho_entrada = (List.length lista_entrada)-1 in
	let controle = ref 0 in
	for i=0 to (tamanho_entrada) do
		if (List.nth lista_entrada i) = "do" then begin
			if (List.nth lista_entrada (i+1)) = "<sep,abreParenteses>" then begin
				controle := 1
			end;	
		end
		(* desse jeito pode dar erro caso tenha algum par�metro dentro do corpo do DO *)
		else if (List.nth lista_entrada i) = "<sep,fechaParenteses>" && (!controle == 1) then begin
			controle := 2
		end					
	done;
	if !controle = 2 then begin
		true
	end
	else
false;;

(* pega o indice na string dos char's c1 e c2 *)
let pega_indice_string string c1 c2 = 
	let tamanho_total = (String.length string)-1 in
	let pos1 = ref 0 in
	let pos2 = ref 0 in
	for j=0 to tamanho_total do
		if (String.get string j) = c1 then begin
			pos1 := j+1
		end
		else if	(String.get string j) = c2 then begin
			pos2 := j
		end
	done;

	let retorno = [!pos1;!pos2] in
retorno;;

(* pega os 4 par�metros do DO *)
(* problemas: ele ignora os parenteses dentro do corpo do DO e tudo que vem depois do corpo ele adiciona a lista de tokens*)
(* tem que dar um jeito de pegar a posi��o do ultimo parenteses do DO *)
let pegar_parametros_do lista_entrada lista_corpo_do =
	let tamanho_entrada = (List.length lista_entrada)-1 in
	
	(* posicao do primeiro par�metro *)
	let posicao1 = ref (-1) in
	
	(* lista com todos os par�metros *)
	let lista_parametro = ref [] in
	
	(* serve para travar quando encontrar os 3 primeiros par�metros *)
	let controle = ref 0 in
	
	(* pos_temp pega a posi��o seguinte ao terceiro par�metro*)
	let pos_temp = ref 0 in
	
	(* pego a posi��o inicial dos par�metros *)
	if eh_do lista_entrada then begin
		for i=0 to (tamanho_entrada) do
			if (List.nth lista_entrada i) = "do" then begin
				if (List.nth lista_entrada (i+1)) = "<sep,abreParenteses>" then begin
					posicao1 := (i+1);
				end;
			end;
		done;
		
		(* pego os 3 primeiros par�metros *)
		for i=(!posicao1) to tamanho_entrada do
			let lista_posicao = pega_indice_string (List.nth lista_entrada i) ',' '>' in 
			let p1 = (List.nth lista_posicao 0) in
			let p2 = (List.nth lista_posicao 1) in
			(* se for do tipo vari�vel e n�o tiver pego os 3 par�metros ainda eu entro aqui *)
			if (Str.string_match (Str.regexp "<var,[a-z].*>") (List.nth lista_entrada i) 0) && (!controle < 3) then begin
				lista_parametro := List.append (!lista_parametro) ([String.sub (List.nth lista_entrada i) p1 (p2-p1)]);
				controle := !controle + 1;
			end
			(* se for do tipo int ou rac e n�o tiver pego os 3 par�metros ainda eu entro aqui *)
			else if (Str.string_match (Str.regexp "<int\\|rac,*>") (List.nth lista_entrada i) 0) && (!controle < 3) then begin
				lista_parametro := List.append (!lista_parametro) ([String.sub (List.nth lista_entrada i) p1 (p2-p1)]);
				controle := !controle + 1;					
			end;
			if !controle = 3 then begin
				controle := 4;
				pos_temp := i+1;
			end;		 	
		done;
		(* adiciona todos os tokens dentro do corpo do DO para uma lista, menos os separadores *)
		for k= (!pos_temp) to tamanho_entrada do
			if not((Str.string_match (Str.regexp "<sep,.*>") (List.nth lista_entrada k) 0)) then begin
				lista_corpo_do := List.append (!lista_corpo_do) [(List.nth lista_entrada k)]  
			end	
		done;	 
	end;
!lista_parametro;;


let aplica_if = fun lista_final ->
		let tamanho_lista_final = (List.length !lista_final) in
		let falso = ref [] in
		let verdadeiro = ref [] in
		let virgula =  ref false in
		let virgulaDois =  ref false in
	for i = 0 to (tamanho_lista_final - 1) do			
			if (List.nth !lista_final i) = "<sep,virgula>" && !virgula = false then
				begin
					virgula := true;
					for j = i+1 to (tamanho_lista_final - 1) do
					if (List.nth !lista_final j) = "<sep,virgula>" || (List.nth !lista_final j) = "<sep,fechaParenteses>" then
						begin
						virgulaDois := true;
						end
					else 
					begin
						if !virgulaDois = false then
							begin
							verdadeiro := !verdadeiro@[(List.nth !lista_final j)];
							end
					end;
				done;
			
			  end
			else
				begin
					if i > 0 then
						begin 
					    if ((List.nth !lista_final i) = "<sep,virgula>" && !virgula = true) ||  ((List.nth !lista_final (i-1)) = "or" && !virgula = true) then
						    begin
					      for j = i+1 to (tamanho_lista_final - 2) do							
					        falso := !falso@[(List.nth !lista_final j)];
						    done;
						    end;
				    end;
					end;
	done;
	try
	for k = 0 to (tamanho_lista_final - 1) do
		if (List.nth !lista_final k) = "<bol,false>"  then
			begin
			lista_final := !falso;
			raise Exit;
			end;
		if (List.nth !lista_final k) = "<bol,true>"  then
			begin
			lista_final := !verdadeiro;
			raise Exit;
			end;
	done;
	false
	with Exit -> true;;

(* aplica o do *)
let aplica_do lista_entrada lista_saida=
	(* a posi��o zero da lista de par�metros armazena o primeiro par�metro do DO *)
	(* armazena o corpo do DO *)
	let lista_tokens_do = ref [] in
	
	(* armazena os par�metros do DO *)
	let lista_parametro = pegar_parametros_do lista_entrada lista_tokens_do in
	
	(* valor inicial do DO *)
	let valor_inicial = int_of_string(List.nth lista_parametro 1) in
	
	(* valor final do DO *)
	let valor_final = int_of_string(List.nth lista_parametro 2) in
	
	for i=valor_inicial to valor_final do
		lista_saida := !lista_saida @ !lista_tokens_do;
done;;

(* transforma uma string em um valor boolean *)
let exp_logica_bool boolean = 
	(* verifica se h� un�rio *)
	let temNegacao = (Str.string_match (Str.regexp ".*!.*") boolean 0) in
	let indice = ref (-1) in
	let temp = ref "" in
	let retorno = ref false in
	
	(* caso tenha un�rio eu retiro ele e transformo para o not da linguagem L e aplico esse not*)
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
		(* caso contr�rio, ele � um booleano 'simples' *)
		retorno := bool_of_string(boolean);
		!retorno;
end;;

(* retorna o respectivo valor l�gico dependendo do operador onde n1 e n2 s�o inteiros *)		
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

(* compara��o entre racionais *)
(* recebe o numerador e o denominador de ambos os racionais juntamente com o operador e retorna se a opera��o � verdadeira ou falsa *)	
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


(* Fun�ao que executa aplicao a partir de um operador logico *)
(* ou booleano entre dois operandos *)
let exp_logica e1 op e2 =
	let operador = ref (String.trim(op)) in
	
	(* essas vari�veis podem n�o ser utilizados *)
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
	(* fim das vari�veis tempor�rias *)
	
	(* r pega a resposta da express�o avalia int e entrou � apenas uma vari�vel de controle*)
	let r = ref false in
	let entrou = ref false in 
	
	verifica_int e1 r;
	if !r = true && not (!entrou) then begin
		(* se entrar aqui � inteiro a express�o antes *)
		valorInteiroAntes := int_of_string(e1); 
		tipoAntes := "int";
		entrou := true;	
	end
	else
		(* a fun��o verifica_rac d� pau se n�o tiver : *)
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
			(* se entrar aqui o tipoAntes � rac ou o tipoDepois � *)
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


(* Fun�ao que executa as funcoes*)
let rec executaAplicacoes = fun lista_final listaVariaveis listaValores listaTipos var_atribuida->
	let tamanho_lista_final = (List.length !lista_final) in
	let ind_op1 = ref 0 in
	let ind_op2 = ref 0 in
	let ind_opCadeia = ref 0 in
  let listaLogico = ["!";"&";"@"] in
  let listaAritmetico = ["+";"-";"*";"/";"%";"^"] in
  let listaComparativo = ["<";">";"="] in 
  let listaPalavraChave = ["if";"or";"do"] in
	let separa_token = Str.regexp "," in
	let ind1 = exists_1ordem !lista_final in
	let ind2 = exists_2ordem !lista_final in
	let ind_aplcomParenteses = ref (0-1) in
	let ind_cadeia = exists_cadeia !lista_final in
	let resposta_palavrachave = ref '0' in
  let aplica = ref false in
	let lista_saida = ref [] in
	let num_atr = ref 0 in
	let ind_segundoAtr = ref (0-1) in
	pega_indice ind1 !lista_final ind_op1;
	pega_indice ind2 !lista_final ind_op2;
	pega_indice ind_cadeia !lista_final ind_opCadeia;
	(* Percorrer a minha lista com os tokens *)
	for i = 2 to (tamanho_lista_final - 3) do
    if contains (List.nth !lista_final i) "aplAritmetica" || contains (List.nth !lista_final i) "aplBooleana" then
			begin
			if (contains (List.nth !lista_final (i-2)) "abreParenteses") && (contains (List.nth !lista_final (i+2)) "fechaParenteses") then
				begin
				ind_aplcomParenteses := i;
				end;
		end;
  done;
  for i = 1 to (tamanho_lista_final - 2) do
		if not (!var_atribuida) then
			begin
		  if (contains (List.nth !lista_final (i-1)) "abreParenteses") && (contains (List.nth !lista_final (i+1)) "fechaParenteses") && not (exists !lista_final "fun") then
			  begin
			    lista_final := new_remove (i-1) !lista_final ;
			    lista_final := new_remove i !lista_final;
				  executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
			  end;
			end;
	done;
	for i = 0 to (tamanho_lista_final - 1) do
		if contains (List.nth !lista_final i) "aplAtr" then
			begin
			num_atr := !num_atr + 1;
			end;
		if !num_atr = 2 && contains (List.nth !lista_final i) "aplAtr" then
			begin
			ind_segundoAtr := i;
			end;
	done;		
  for i = 0 to (tamanho_lista_final - 1) do
		if not (!var_atribuida) then
			begin
			resposta_palavrachave := '0';	
	    let tamanho_original = String.length (List.nth !lista_final i) in
			verifica (List.nth !lista_final i) listaPalavraChave resposta_palavrachave;
      if !resposta_palavrachave = 's' then
				begin
					if (contains (List.nth !lista_final i) "if") && (contains (List.nth !lista_final (i+3)) "virgula") && (contains (List.nth !lista_final (i+2)) "bol") then
						begin
						aplica := aplica_if lista_final;
						print_list !lista_final;
				    executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
						end;
					if (contains (List.nth !lista_final i) "do") then
						begin
						aplica_do !lista_final lista_saida;
						lista_final := !lista_saida;
				    executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
						end;
				end
			else
				begin
	    	let lista_token = ref (Str.split separa_token (String.sub (List.nth !lista_final i)
				 1 (tamanho_original - 2))) in					
				if (List.nth !lista_token 0) = "aplBooleana" then
			    begin		
			    let total = ref false in
			    let totalString = ref "" in
				  let list_total = ref [] in
			    let token_total = ref [] in					
			    total := exp_logica (List.nth !lista_token 2) (List.nth !lista_token 1) (List.nth !lista_token 3);
				  totalString := string_of_bool !total;			
			    list_total := !totalString :: !list_total;
			    token list_total listaAritmetico listaLogico listaPalavraChave listaComparativo token_total;
			    lista_final := replace !lista_final (i-1) (List.hd !token_total);			
			    lista_final := new_remove i !lista_final;
			    lista_final := new_remove i !lista_final;
				  executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
				end;			
			if (List.nth !lista_token 0) = "trocaSinal" then
				begin
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
				  pega_indice (List.nth !lista_token 2) !listaVariaveis var_indice1;
			    let valor1 = (List.nth !listaValores !var_indice1) in
				  lista_token := replace !lista_token 2 valor1;
				  end
				(* Se n�o	tem que atualizar o valor no token atual *)
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
				  pega_indice (List.nth !lista_token 2) !listaVariaveis var_indice2;
			    let valor2 = (List.nth !listaValores !var_indice2) in
				  lista_token := replace !lista_token 3 valor2;
				  end
				(* Se n�o	tem que atualizar o valor no token atual *)
				else
					begin
	        let tamanho_original_proximo = String.length (List.nth !lista_final (i+1)) in
	        let lista_token_proximo = ref (Str.split separa_token (String.sub (List.nth !lista_final (i+1)) 1 (tamanho_original_proximo - 2))) in
					lista_token := replace !lista_token 3	(List.nth !lista_token_proximo 1);
					end;						
				if i < !ind_segundoAtr || !ind_segundoAtr = -1 then
					begin	
				if !ind_aplcomParenteses != -1 then
					begin
						if !ind_aplcomParenteses = i then	
					    begin
			          aplAritmetica (List.nth !lista_token 1) (List.nth !lista_token 2) (List.nth !lista_token 3) total;
			          list_total := !total :: !list_total;
			          token list_total listaAritmetico listaLogico listaPalavraChave listaComparativo token_total;
			          lista_final := replace !lista_final (i-1) (List.hd !token_total);
			          lista_final := new_remove i !lista_final;
			          lista_final := new_remove i !lista_final;
			          executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
					    end;
						end
					else
						begin		
				if !ind_opCadeia > 0 then	
					begin
			      aplAritmetica (List.nth !lista_token 1) (List.nth !lista_token 2) (List.nth !lista_token 3) total;
			      list_total := !total :: !list_total;
			      token list_total listaAritmetico listaLogico listaPalavraChave listaComparativo token_total;
			      lista_final := replace !lista_final (i-1) (List.hd !token_total);
			      lista_final := new_remove i !lista_final;
			      lista_final := new_remove i !lista_final;
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
			        lista_final := new_remove i !lista_final;
			        lista_final := new_remove i !lista_final;
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
			          lista_final := new_remove i !lista_final;
			          lista_final := new_remove i !lista_final;
			          executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
							  end;
					    end;
						end;
					end	
			  end;
				end;
				end;
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
			  lista_final := new_remove (i-1) !lista_final;
			  lista_final := new_remove (i-1) !lista_final;
			  lista_final := new_remove (i-1) !lista_final;
			  executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
			  end;
			end;	
  done;
	print_list !lista_final;
	if (exists !lista_final "Atr") then
		begin
		Printf.printf "teste";	
		var_atribuida := false;
		end
	else	
		begin
	  var_atribuida := true;
		end;;

(* VERIFICACAO DE EXPRESSAO BOOLEANA *)

(* verifica se � booleano *)
let eh_booleano input = 
	let regex = "!?(*!?true)*$\\|!?(*!?false)*$"in
	Str.string_match (Str.regexp regex) input 0

(* transforma um operador l�gico da linguagem L para um operador l�gico da linguagem ocaml *)
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

				
(* retorna o respectivo valor l�gico dependendo do operador onde n1 e n2 s�o inteiros *)		
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
	(* verifica se h� un�rio *)
	let temNegacao = (Str.string_match (Str.regexp ".*!.*") boolean 0) in
	let indice = ref (-1) in
	let temp = ref "" in
	let retorno = ref false in
	
	(* caso tenha un�rio eu retiro ele e transformo para o not da linguagem L e aplico esse not*)
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
		(* caso contr�rio, ele � um booleano 'simples' *)
		retorno := bool_of_string(boolean);
		!retorno;
	end;;				
				
(* compara��o entre racionais *)
(* recebe o numerador e o denominador de ambos os racionais juntamente com o operador e retorna se a opera��o � verdadeira ou falsa *)	
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


(* recebe duas express�es e um operador e retorna verdadeiro ou falso dependendo da rela��o entre os operadores *)
(* exemplo de uso: exp_logica "10" ">" "20" -> retorno = false *)
(* AVISO: trata casos simples de express�o booleana !(!true) dar� erro *)
let exp_logica e1 op e2 =
	let operador = ref (String.trim(op)) in
	
	(* essas vari�veis podem n�o ser utilizados *)
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
	(* fim das vari�veis tempor�rias *)
	
	(* r pega a resposta da express�o avalia int e entrou � apenas uma vari�vel de controle*)
	let r = ref false in
	let entrou = ref false in 
	
	verifica_int e1 r;
	if !r = true && not (!entrou) then begin
		(* se entrar aqui � inteiro a express�o antes *)
		valorInteiroAntes := int_of_string(e1); 
		tipoAntes := "int";
		entrou := true;	
	end
	else
		(* a fun��o verifica_rac d� pau se n�o tiver : *)
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
			(* se entrar aqui o tipoAntes � rac ou o tipoDepois � *)
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

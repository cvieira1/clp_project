																(* INÍCIO AVALIA SINTAXE *)
																
																
(* verifica se é o laço de repetição DO *)
let eh_do lista_entrada = 
	let tamanho_entrada = (List.length lista_entrada)-1 in
	let controle = ref 0 in
	for i=0 to (tamanho_entrada) do
		if (List.nth lista_entrada i) = "do" then begin
			if (List.nth lista_entrada (i+1)) = "<sep,abreParenteses>" then begin
				controle := 1
			end;	
		end
		(* desse jeito pode dar erro caso tenha algum parâmetro dentro do corpo do DO *)
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

(* pega os 4 parâmetros do DO *)
(* problemas: ele ignora os parenteses dentro do corpo do DO e tudo que vem depois do corpo ele adiciona a lista de tokens*)
(* tem que dar um jeito de pegar a posição do ultimo parenteses do DO *)
let pegar_parametros_do lista_entrada lista_corpo_do =
	let tamanho_entrada = (List.length lista_entrada)-1 in
	
	(* posicao do primeiro parâmetro *)
	let posicao1 = ref (-1) in
	
	(* lista com todos os parãmetros *)
	let lista_parametro = ref [] in
	
	(* serve para travar quando encontrar os 3 primeiros parâmetros *)
	let controle = ref 0 in
	
	(* pos_temp pega a posição seguinte ao terceiro parâmetro*)
	let pos_temp = ref 0 in
	
	(* pego a posição inicial dos parâmetros *)
	if eh_do lista_entrada then begin
		for i=0 to (tamanho_entrada) do
			if (List.nth lista_entrada i) = "do" then begin
				if (List.nth lista_entrada (i+1)) = "<sep,abreParenteses>" then begin
					posicao1 := (i+1);
				end;
			end;
		done;
		
		(* pego os 3 primeiros parâmetros *)
		for i=(!posicao1) to tamanho_entrada do
			let lista_posicao = pega_indice_string (List.nth lista_entrada i) ',' '>' in 
			let p1 = (List.nth lista_posicao 0) in
			let p2 = (List.nth lista_posicao 1) in
			(* se for do tipo variável e não tiver pego os 3 parâmetros ainda eu entro aqui *)
			if (Str.string_match (Str.regexp "<var,[a-z].*>") (List.nth lista_entrada i) 0) && (!controle < 3) then begin
				lista_parametro := List.append (!lista_parametro) ([String.sub (List.nth lista_entrada i) p1 (p2-p1)]);
				controle := !controle + 1;
			end
			(* se for do tipo int ou rac e não tiver pego os 3 parâmetros ainda eu entro aqui *)
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
			
(* aplica o do *)
let aplica_do lista_entrada =
	(* a posição zero da lista de parâmetros armazena o primeiro parâmetro do DO *)
	(* armazena o corpo do DO *)
	let lista_tokens_do = ref [] in
	
	(* armazena os parâmetros do DO *)
	let lista_parametro = pegar_parametros_do lista_entrada lista_tokens_do in
	
	(* valor inicial do DO *)
	let valor_inicial = int_of_string(List.nth lista_parametro 1) in
	
	(* valor final do DO *)
	let valor_final = int_of_string(List.nth lista_parametro 2) in
	
	for i=valor_inicial to valor_final do
		for i=0 to ((List.length !lista_tokens_do)-1) do
			print_string(List.nth !lista_tokens_do i);
			print_newline();
		done;	
	done;;					

(* FIM AVALIA SINTAXE *)

(* INÍCIO DO ROLE DO MARCO *)


(* Funçao que executa as funcoes*)
let rec executaAplicacoes = fun lista_final listaVariaveis listaValores listaTipos var_atribuida->
	
	let tamanho_lista_final = (List.length !lista_final) in
  let listaLogico = ["!";"&";"@"] in
  let listaAritmetico = ["+";"-";"*";"/";"%";"^"] in
  let listaComparativo = ["<";">";"="] in 
  let listaPalavraChave = ["if";"or"] in
	let separa_token = Str.regexp "," in
  for i = 0 to (tamanho_lista_final - 1) do
		if !var_atribuida = false then 
		let tamanho_original = String.length (List.nth !lista_final i) in
			if (List.nth !lista_final i) = "if" then
				begin			
				
				end
			else
				begin		
						
	    		let lista_token = ref (Str.split separa_token (String.sub (List.nth !lista_final i) 1 (tamanho_original - 2))) in	
			if (List.nth !lista_token 0) = "exp_bool" then
			  begin		
				var_atribuida := true;
			  let total = ref false in
			  let totalString = ref "" in
				let list_total = ref [] in
			  let token_total = ref [] in					
			  total := exp_logica (List.nth !lista_token 2) (List.nth !lista_token 1) (List.nth !lista_token 3);
				totalString := string_of_bool !total;			
			  list_total := !totalString :: !list_total;
			  token list_total listaAritmetico listaLogico listaPalavraChave listaComparativo token_total;
			  lista_final := replace !lista_final (i-1) (List.hd !token_total);			
			  lista_final := remove !lista_final (List.nth !lista_final i);
			  lista_final := remove !lista_final (List.nth !lista_final i);
				executaAplicacoes lista_final listaVariaveis listaValores listaTipos var_atribuida;
				end;			
			  end;	
	done;;

let partedois = fun lista_final ->
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
					if (List.nth !lista_final j) = "<sep,virgula>" then
						begin
						virgulaDois := true;
						end
					else 
					begin
						if !virgulaDois = false then
							begin
							verdadeiro := List.nth !lista_final j :: !verdadeiro;
							end
					end;
				done;
			
			  end
			else
				begin
					if (List.nth !lista_final i) = "<sep,virgula>" && !virgula = true then
					for j = i+1 to (tamanho_lista_final - 2) do
							
					falso := List.nth !lista_final j :: !falso;
						done;
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

(* FIM DO ROLE DO MARCO *)


		
																				(* CÓDIGO ABAIXO SEM UTILIDADE NO MOMENTO *)									
						
(*								
let exp_logica entrada listaT listaV listaVar =
	let operador = ref "" in
	let indice = ref (-1) in
	(* pega o indice e o operador da expressão *)
	for i=0 to ((String.length entrada)-1) do
		if (String.make 1 entrada.[i]) = "=" then begin
			operador := "=";
			indice := i;
		end
		else 
			if (String.make 1 entrada.[i]) = ">" then begin
				operador := ">";
				indice := i;
			end
			else 
				if (String.make 1 entrada.[i]) = "<" then begin
					operador := "<";
					indice := i;
				end;
	done;
	if !indice = -1 then begin
		(* tem que tratar esse valor e usar outro nome se tiver*)
		raise Exit
	end;
	
	let exp_antes = String.trim(Str.string_before entrada !indice) in
	let exp_depois = String.trim(Str.string_after entrada (!indice+1)) in
	
	operador := String.trim(!operador);
	
	let retorno_antes = ref (-1) in 
	let retorno_depois = ref (-1) in 
	
	(* o retorno antes e depois servem para ver se a expressão é variável ou não *)
	retorno_antes := AvaliaSintaxe.pega_indice_string exp_antes listaVar;
	retorno_depois := AvaliaSintaxe.pega_indice_string exp_depois listaVar;
	
	(* essas variáveis podem não ser utilizados *)
	let valorInteiroAntes = ref 0 in
	let valorBoolAntes = ref false in
	
	let valorInteiroDepois = ref 0 in
	let valorBoolDepois = ref false in
	
	let tipoAntes = ref "" in
	let tipoDepois = ref "" in
	(* fim das variáveis temporárias *)
	
	(* variáveis para armazenar o índice da lista *)
	let indice_tipo = ref (-1) in 
	let indice_valor = ref (-1) in
	
	(* variáveis para armazenar o índice da lista *)
	let indice_tipo_depois = ref (-1) in
	let indice_valor_depois = ref (-1) in
	
	(* r pega a resposta da expressão avalia int e entrou é apenas uma variável de controle*)
	let r = ref false in
	let entrou = ref false in 
	
	(* nesse caso a expressão não é uma variável *)
	if !retorno_antes = (-1) then begin
		AvaliaSintaxe.verifica_int exp_antes r;
		if !r = true && not (!entrou) then begin
			(* se entrar aqui é inteiro a expressão antes *)
			valorInteiroAntes := int_of_string(exp_antes);
			tipoAntes := "int";
			entrou := true;	
		end;
		
		if AvaliaSintaxe.eh_booleano exp_antes then begin
			(* se entrar aqui ela é booleana, essa função abaixo pega o tipo simples true ou false, mas dá pau para !true tem que arrumar isso depois*)
			valorBoolAntes := Utilidades.exp_logica_bool (exp_antes);
			tipoAntes := "bool";
			entrou := true;
		end;	
	end
	else begin
		(* já nesse ela é uma variável *)
		for i=0 to ((List.length listaVar)-1) do
			if (List.nth listaVar i) = exp_antes then begin
				indice_tipo := i;
				indice_valor := i;
				tipoAntes := List.nth listaT i;
			end;	
		done;
		
		if (List.nth listaT !indice_tipo) = "int" then begin
			valorInteiroAntes := int_of_string(List.nth listaV !indice_valor);
		end;
		
		if (List.nth listaT !indice_tipo) = "bool" then begin
			valorBoolAntes := Utilidades.exp_logica_bool (List.nth listaV !indice_valor)
		end;		
	end;
	
	entrou := false;
	r := false;
	
	(* nesse caso a expressão não é uma variável *)
	if !retorno_depois = (-1) then begin
		AvaliaSintaxe.verifica_int exp_depois r;
		if !r = true && not (!entrou) then begin
			(* se entrar aqui é inteiro a expressão depois *)
			valorInteiroDepois := int_of_string(exp_depois);
			entrou := true;	
			tipoDepois := "int";
		end;
		
		if not (!entrou) && AvaliaSintaxe.eh_booleano exp_depois then begin
			valorBoolDepois := Utilidades.exp_logica_bool (exp_depois);
			entrou := true;
			tipoDepois := "bool";
		end;	
	end
	else begin
		(* já nesse ela é uma variável *)
		for i=0 to ((List.length listaVar)-1) do
			if (List.nth listaVar i) = exp_depois then begin
				indice_tipo_depois := i;
				indice_valor_depois := i;
				tipoDepois := List.nth listaT i;
			end;	
		done;
		
		if (List.nth listaT !indice_tipo_depois) = "int" then begin
			valorInteiroDepois := int_of_string(List.nth listaV !indice_valor_depois);
		end;
		
		if (List.nth listaT !indice_tipo_depois) = "bool" then begin
			valorBoolDepois := Utilidades.exp_logica_bool (List.nth listaV !indice_valor_depois);
		end;		
	end;
	
	let resposta = ref false in
	let entrouFinal = ref 0 in
	
	if !tipoAntes = "int" && !tipoDepois = "int" then begin
		resposta := Utilidades.exp_logica_inteiro !valorInteiroAntes !operador !valorInteiroDepois;
		entrouFinal := 1;	
	end
	else if !tipoAntes = "bool" && !tipoDepois = "bool" && !operador = "=" then begin
		resposta := !valorBoolAntes = !valorBoolDepois;
		entrouFinal := 1;	
	end;	
	
	if !entrouFinal <> 0 then begin
		!resposta
	end
	else
		raise Exit;;	
*)	
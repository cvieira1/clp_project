																		(* AVALIASINTAXE *)

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
	end;

				
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
		
				
			
																					(* UTILIDADES FIM *)				
	
									
						
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
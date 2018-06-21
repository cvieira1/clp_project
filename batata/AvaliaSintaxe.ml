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

(* Função que verifica se a string contem um termo de uma lista *)		
let verifica = fun s lista resposta ->
  let lista_string = explode s in
  let tamanho_lista_string = List.length lista_string in
  let tamanho_lista = List.length lista in	 
  let char = ref '0' in
  let existe = ref '0' in
	try
  for i = 0 to tamanho_lista_string - 1 do
	  for j = 0 to tamanho_lista - 1 do
	    char := List.nth lista_string i;
	    existe := List.nth lista j;
	    resposta := if char = existe
			then 's'
			else 'n';
			if !resposta = 's' then raise Exit;
	  done
  done;
	false
	with Exit -> true;;

(* Função que retorna o indice na lista da string do primeiro termo comum*)
(*  entre a string e a lista*)		
let pega_indice = fun s lista resposta ->
  let lista_string = explode s in
  let tamanho_lista_string = List.length lista_string in
  let tamanho_lista = List.length lista in	 
  let char = ref '0' in
  let existe = ref '0' in
	try
  for i = 0 to tamanho_lista_string - 1 do
	  for j = 0 to tamanho_lista - 1 do
	    char := List.nth lista_string i;
	    existe := List.nth lista j;
	    resposta := if char = existe
			then i
			else -1;
			if !resposta = i then raise Exit;
	  done
  done;
	false
	with Exit -> true;;


(* Função que verifica se é inteiro *)
let verifica_int = fun s resposta ->
  resposta := Str.string_match (Str.regexp "0$\|-?[1-9]$\|-?[1-9][0-9]+$") s 0;;

(* Função que verifica se é natural *)
let verifica_nat = fun s resposta ->
  resposta := Str.string_match (Str.regexp "[1-9]$\|[1-9][0-9]+$") s 0;;

(* Função que verifica se é racional *)
let verifica_rac = fun s resposta ->
	let regexp_div = Str.regexp "[:]" in
  let ind_div = Str.search_forward regexp_div s 0 in
	let int_part = Str.string_before s ind_div in
	let nat_part = Str.string_after s (ind_div+1) in
	let resposta1 = ref true in
	let resposta2 = ref true in
	verifica_int int_part resposta1;
	verifica_nat nat_part resposta2;
	resposta :=
		if !resposta1 && !resposta2 then
		  true
		else
			false;;

(* Função que verifica se há operador de comparação na string *)
let verifica_comparativo = fun input ->
  let exp = Str.regexp ".+>.+$\|.+<.+$\|.+=.+$" in
  let resultado = Str.string_match exp input 0 in
  if resultado then true else false;; 
  
(* Função que verifica se a quantidade e ordem dos parenteses esta correta *)  
let verifica_parenteses = fun s resposta->
	let tamanho_string = String.length s in
	let num_parenteses1 = ref 0 in
	let num_parenteses2 = ref 0 in
	let num_parenteses3 = ref 0 in
	let final = ref tamanho_string in
	let acha_par = ref 0 in
	let rodada_1 = ref true in
	for i = 0 to (tamanho_string-1) do
		if (String.get s i) == '(' then
			begin
		  num_parenteses1 := !num_parenteses1 + 1;
			acha_par := 1;
			for j = (i+1) to (!final-1) do
				if (String.get s j) == '(' then
			    acha_par := !acha_par + 1;					
				if (String.get s j) == ')' then
			    acha_par := !acha_par - 1;
				if !acha_par == 0 && !rodada_1 then	
					begin
					num_parenteses2 := !num_parenteses2 + 1;
					rodada_1 := false;
					end;
			done;
			rodada_1 := true;
			end;
		if (String.get s i) == ')' then
			num_parenteses3 := !num_parenteses3 + 1;	
	done;
	if !num_parenteses1 != 0 || !num_parenteses3 != 0 then		
	  if !num_parenteses1 == !num_parenteses2
		&& (!num_parenteses3 + !num_parenteses1) == (!num_parenteses1*2) then
		  resposta := true
	  else
		  resposta := false
  else
		resposta := false;;


(*A LÓGICA USADA FOI QUE SE A VARIÁVEL CONTROLE FOR IGUAL A STRING ENTÃO ELA PERCORREU TODA A STRING OBEDECENDO AS REGRAS, LOGO*)
(*ELA É UMA VARIÁVEL, CASO CONTRÁRIO NÃO *)

(* O argumento Retorno tem que ser sempre true, controle e pos devem ser ZERO *)
(*verifica se é uma variável ou não*)
let rec ehVariavel exp pos controle retorno = 
	(*POS INDICA A POSIÇÃO ATUAL, EXP É UMA STRING*)
	if (pos = (String.length exp)) then 
		if (controle = (String.length exp)) then 
			true
		else 
			false
	else 
		(*VERIFICA SE O PRIMEIRO CHAR É MINUSCULO*)
		if ((e_variavel_min (String.get exp pos)) && (pos = 0)) then
			retorno = ehVariavel exp (pos+1) (controle+1) retorno
	  else 
			if ((e_variavel_max (String.get exp pos) || e_variavel_min (String.get exp pos) || e_digito (String.get exp pos)) && (pos <> 0)) then
				retorno = ehVariavel exp (pos+1) (controle+1) retorno
			else
				retorno = ehVariavel exp (pos+1) (controle) retorno;;

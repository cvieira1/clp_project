open AvaliaSintaxe;;

(* mostra a lista *)
let rec print_list = function
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l;;

(*função para tirar todos espaços *)
let tira_espace exp =
	Str.global_replace (Str.regexp " \\|\t") "" exp;;

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

(* !!!!INICIO FUNCAO HACK!!!! *)
(* Divide String em Lista *)
(* /////////////////////////////////////OBS:TRANSFORMAR ESTA PARTE EM FUNÇÃO/////////////////////////////////////// *)
let separa_hack = fun expressao_real->
  let expressao = tira_espace expressao_real in
  let lista_char = explode expressao in
	let palavra = ref "" in
	let letra = ref ' ' in
	let lista = ref [""] in		
for indice=0 to ((List.length lista_char)-1) do
	letra := List.nth lista_char indice;
	if e_variavel_min(!letra) || e_variavel_max(!letra) || e_digito !letra then
		palavra := (!palavra)^(String.make 1 !letra);
		if indice = ((List.length lista_char)-1) then
			begin
				if (e_variavel_min(!letra) = false) && (e_variavel_max(!letra)=false) && (e_digito !letra = false)  then begin
					lista := List.append [!palavra] !lista;
					palavra := ""	;
					lista := List.append [String.make 1 !letra] !lista;end
				else begin
					lista := List.append [!palavra] !lista;
					palavra := "";
				end
			end
		else
			if (e_variavel_min(!letra) = false) && (e_variavel_max(!letra)=false) && (e_digito !letra = false)  then begin
				lista := List.append [!palavra] !lista;
				palavra := ""	;
				lista := List.append [String.make 1 !letra] !lista;end
			else begin
				if (e_variavel_max(!letra)=true) then begin
				  lista := List.append [!palavra] !lista;
				end	
			end
done;
print_list (List.rev !lista);;
(* !!!!INICIO FUNCAO CAIO!!!! *)
(* Divide String em Lista *)
let verifica_sintaxeFun = fun s lista ->
	  let tamanho = String.length s in
		let lista_s = explode s in
		let lista_var = ref [] in
		let indice_atual = ref 0 in 
		let var = ref "" in
		for i = 0 to (tamanho - 1) do
			if (String.get s i) = '(' || (String.get s i) = ')' || (String.get s i) = ',' then
				begin
				lista_var := (Char.escaped(String.get s i) :: !lista_var);
				end
			else
				begin
				var := !var ^ Char.escaped(String.get s i);						
				if(i+1) < tamanho then 
					begin
			    if (String.get s (i+1)) = '(' || (String.get s (i+1)) = ')' || (String.get s (i+1)) = ',' then
					  begin
				    lista_var := (!var :: !lista_var);
					  var := "";
					  end
					end
				end		
		done;
		lista := List.rev !lista_var;;


(*Função que converte uma lista normal numa lista de TOKENS*)
let token = fun lista lista_final ->
	let tamanho = List.length !lista in
	let resposta_int = ref false in
	let resposta_rac = ref false in
	let regexp_div = Str.regexp "[:]" in
	let rodada1 = ref true in
	for i = 0 to (tamanho - 1) do
		verifica_int (List.nth !lista i) resposta_int;
		let tamanho_string = String.length (List.nth !lista i) in
		for j = 0 to (tamanho_string) do
		if (Str.string_match regexp_div (List.nth !lista i) j) then
			begin
		  AvaliaSintaxe.verifica_rac (List.nth !lista i) resposta_rac;
			if !resposta_rac then
				rodada1 := false;
			end;
		done;	
		rodada1 := true;
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
	print_list (List.rev !lista_final);;

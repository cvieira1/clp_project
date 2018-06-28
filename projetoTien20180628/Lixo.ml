(* open AvaliaSintaxe;;                                                                           *)

(* (* Função que separa a string em uma lista contendo os 'tokens' *)                           *)
(* let rec separa_exp = fun s lista ->                                                            *)
(*   let resposta = ref '0' in                                                                    *)
(* 	let teste = ref true in                                                                      *)
(* 	  teste := verifica s listaAritmetico resposta;                                              *)
(* 		if !resposta = 's' then                                                                    *)
(*    	  let teste2 = ref true in                                                                *)
(* 	    let resposta_indice = ref 0 in                                                           *)
(*       teste2 := pega_indice s listaAritmetico resposta_indice;                                 *)
(* 		  let char = ref '0' in                                                                    *)
(* 		  let lista_s = explode s in                                                               *)
(* 		  let tamanho = List.length lista_s in                                                     *)
(* 		  char := List.nth lista_s !resposta_indice;                                               *)
(* 		  lista := List.append [!char] !lista;                                                     *)
(*       if (tamanho == 1) then                                                                   *)
(* 			  lista else                                                                             *)
(* 			  separa_exp (String.sub s (!resposta_indice+1) (tamanho - !resposta_indice - 1)) lista; *)
(*     else lista;;                                                                               *)
(*função para tirar todos espaços *)
let tira_espace exp =
	Str.global_replace (Str.regexp " \\|\t") "" exp;;
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

(* Função que verifica se há operador de comparação na string *)
let verifica_comparativo = fun input ->
  let exp = Str.regexp ".+>.+$\|.+<.+$\|.+=.+$" in
  let resultado = Str.string_match exp input 0 in
  if resultado then true else false;; 


(* Função que separa a string em uma lista contendo os 'tokens' *)
let rec separa_exp = fun s lista ->
  let resposta = ref '0' in
	let teste = ref true in
	  teste := verifica s listaAritmetico resposta;
		if !resposta = 's' then
   	  let teste2 = ref true in
	    let resposta_indice = ref 0 in
      teste2 := pega_indice s listaAritmetico resposta_indice;
		  let char = ref '0' in
		  let lista_s = explode s in
		  let tamanho = List.length lista_s in
		  char := List.nth lista_s !resposta_indice;
		  lista := List.append [!char] !lista;
      if (tamanho == 1) then
			  lista else
			  separa_exp (String.sub s (!resposta_indice+1) (tamanho - !resposta_indice - 1)) lista;
    else lista;;

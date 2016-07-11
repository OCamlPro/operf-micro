type t =
  | Dict of (string * t) list
  | List of t list
  | String of string
  | Int of int

type file = t

open Format

let rec print ppf = function
  | Dict l ->
    fprintf ppf "{@[@<2> ";
    let rec aux = function
      | [] -> ()
      | [k, v] ->
        fprintf ppf "%s:@ %a@ " k print v
      | (k, v) :: t ->
        fprintf ppf "%s:@ %a,@ " k print v;
        aux t
    in
    aux l;
    fprintf ppf "@]}"
  | List l ->
    fprintf ppf "[@[@<2> ";
    let rec aux = function
      | [] -> ()
      | [v] ->
        fprintf ppf "%a@ " print v
      | v :: t ->
        fprintf ppf "%a,@ " print v;
        aux t
    in
    aux l;
    fprintf ppf "@]]"
  | String s ->
    pp_print_char ppf '\"';
    pp_print_string ppf (String.escaped s);
    pp_print_char ppf '\"'
  | Int f ->
    pp_print_int ppf f

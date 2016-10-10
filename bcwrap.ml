
(* Global *)

let inc_dirs = ref []
let option_r = ref "/opt/atelierb-4.3/bbin/linux_x64/AtelierB"
let bcomp_bin = ref "/opt/atelierb-4.3/bbin/linux_x64/bcomp"
let check_syntax_only = ref false

(* System functions *)

let safe_file_exists (fn:string) : bool =
  try Sys.file_exists fn
  with Sys_error _ -> false
                      
let safe_is_directory dir =
  try Sys.is_directory dir
  with Sys_error _ -> false

let safe_readdir dir =
  try Sys.readdir dir
  with Sys_error _ -> [||]

(* Error function *)

let error (msg:string) =
  prerr_endline msg;
  exit(1)

(* Find the machine in the path *)

(*
 let get_subdirs (dir:string) : string list =
    Array.fold_left (
      fun acc sf ->
        let fp = dir ^ "/" ^ sf in
        if safe_is_directory fp then fp::acc else acc
    ) [] (safe_readdir dir)
*)

let get_full_path (fn:string) =
  let rec aux : string list -> string = function
    | [] -> raise Not_found
    | dir::tl ->
      let fullname = dir ^ "/" ^ fn in
      if safe_file_exists fullname then fullname
      else aux tl
  in
  try aux ("."::!inc_dirs)
  with Not_found -> fn

(* Wrapper arount bcomp *)

let open_process_in (cmd:string) : in_channel =
  try Unix.open_process_in cmd
  with Unix.Unix_error (n,_,_) ->
    error ("Error: failed to execute the bcomp ("^ Unix.error_message n^").")

let close_process_in (input:in_channel) : unit =
  try ignore (Unix.close_process_in input)
  with Unix.Unix_error (_,_,_) -> ()

let reformat (line:string) : unit =
  match Str.bounded_split (Str.regexp ":") line 2 with
  | [fn;s] -> Printf.eprintf "%s:%s\n" (get_full_path fn) s
  | _ -> ()

let check_file (mch:string) : unit =
  let inc_dirs_str = String.concat " " (List.map (fun dir -> "-I " ^ dir ) !inc_dirs) in
  let cmd =
    if !check_syntax_only then
      Printf.sprintf "%s -s -r %s -i %s %s 2>&1" !bcomp_bin !option_r mch inc_dirs_str
    else
      Printf.sprintf "%s -a -r %s -i %s %s 2>&1" !bcomp_bin !option_r mch inc_dirs_str
  in
  let input = open_process_in cmd in
  try
    while true do
      reformat (input_line input)
    done 
  with End_of_file -> close_process_in input

(* Reading config file *)

let rec rev_concat = function
  | [] -> "/"
  | hd::tl -> (rev_concat tl) ^ hd ^ "/"

let find_config_file () : string option =
  let current_path = List.rev (Str.split (Str.regexp "/") (Sys.getcwd ())) in
  let rec all_paths dirs =
    let path =  rev_concat dirs in
    match dirs with
    | [] -> [path] 
    | _::dirs ->  path::(all_paths dirs)
  in
  let rec find = function
    | [] -> None
    | dir::tl ->
      let fullname = dir ^ "/.bcwrap" in
      if safe_file_exists fullname then Some fullname
      else find tl 
  in
  find (all_paths current_path)

let read_config_file () : unit =
  match find_config_file () with
    | None -> prerr_endline "No file .bcwrap found."
    | Some cfile ->
      begin
        let input =
         ( try open_in cfile
           with _ -> error ("Error: failed to open '"^cfile^"'.") )
        in
        try
          while true do
            let line = input_line input in
            match Str.bounded_split (Str.regexp_string " ") line 2 with
            | [cmd;arg] ->
              begin
                let arg = String.trim arg in
                if String.equal cmd "AB" then
                  begin
                    if safe_file_exists arg then option_r := arg
                    else error ( "Error: cannot find '" ^ arg ^ "'." )
                  end
                else if String.equal cmd "SRC" then
                  begin
                    if safe_is_directory arg then inc_dirs := arg :: !inc_dirs
                    else error ("Error: '"^arg^"' is not a directory.")
                  end
                else if String.equal cmd "BC" then
                  begin
                    if safe_file_exists arg then bcomp_bin := arg
                    else error ( "Error: cannot find '" ^ arg ^ "'." )
                  end
                else
                  error ("Error: unknown command '"^cmd^"'.")
              end
            | _ -> error ("Error: unknown command '"^line^"'.")
          done 
        with End_of_file -> ()
      end


  (* Program entry *)
let opts = [ ( "-s", Arg.Set check_syntax_only , "Syntactic check only.") ]
let _ =
  read_config_file ();
  Arg.parse opts check_file "bcwrap [options] file1.mch file2.mch ..." 

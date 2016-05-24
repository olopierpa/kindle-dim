
(****************************************************************)

let tospun group_length separator string =
  let rec fa i acc =
    let next = i - group_length in
    if next <= 0 then String.sub string 0 i :: acc
    else fa next (String.sub string next group_length :: acc)
  in String.concat separator (fa (String.length string) []);;
  
let ultospun ?(length = 3) ?(separator = "_") n =
  if n >= 0 then tospun length separator (string_of_int n)
  else "-" ^  tospun length separator (string_of_int (-n));;
  
(****************************************************************)
  
let kindle_default_mount_point = "G:";;
let kindle_directory = "/documents";;
  
let max_displayed_title_length = 32;;
  
let discardable_suffixes =
  [".ea"; ".han"; ".upd"; ".mbp"; ".apnx"; ".phl"; ".tan"];;
  
let error_report_and_user_help () =
  Printf.fprintf stderr "Usage: %s [mount_point] [frugal]\n" Sys.argv.(0);
  Array.iter
    (fun s -> Printf.fprintf stderr "%s\n" s)
    [|
      "  - mount_point is the point where the the Kindle is mounted (default \"G:\").";
      "    On Windows this is the drive letter used (followed by a colon).";
      "  - frugal can be yes or no. Default is yes.";
      "    When yes, a format using less horizontal space is selected.";
    |];
  if not !Sys.interactive then begin
      exit 1
    end;;
  
let file_ok filename stats =
  stats.Unix.st_kind = Unix.S_REG &&
    let rec loop = function
      | suffix :: suffixes ->
         not (Filename.check_suffix filename suffix) &&
           loop suffixes
      | _ -> true
    in loop discardable_suffixes;;
  
let re_name = Str.regexp "^\\(.+\\)-asin_.+-type_\\([A-Z]+\\)-.*$";;
  
let decompose_name filename =
  if Str.string_match re_name filename 0 then begin
    let name = Str.matched_group 1 filename in
    let atype = Str.matched_group 2 filename in
    (name, atype)
  end else
    (filename, "");;
  
let line length = String.make length '-';;
  
let fa mount_point print_headers print_frugal print_double_space =
  let dir_0 = Unix.getcwd () in
  let dir_name = Printf.sprintf "%s%s" mount_point kindle_directory in
  Unix.chdir dir_name;
  let dh = Unix.opendir "." in
  let max_file_size = ref 0 in
  let max_filename_length = ref 0 in
  let vspace = if print_double_space then "\n\n"
               else "\n" in
  let list =
    let rec loop acc =
      match Unix.readdir dh with
      | exception End_of_file ->
         (* chdir and close so when run in the repl the wd does
            not remain in the kindle, so it can be umounted *)
         Unix.chdir dir_0;
         Unix.closedir dh;
         acc
      | filename ->
         let ss = Unix.stat filename in
         if file_ok filename ss then
           let (name, atype) = decompose_name filename in
           let size = ss.Unix.st_size in
           if size > !max_file_size then begin
             max_file_size := size;
           end;
           let name_length = String.length name in
           if name_length > !max_filename_length then begin
             max_filename_length := name_length
           end;
           loop ((atype, size, name) :: acc)
         else loop acc
    in loop [] in
  max_filename_length := min !max_filename_length max_displayed_title_length;
  let sorted = List.sort
                 (fun (atype1, size1, name1) (atype2, size2, name2) ->
                   compare (atype1, -size1, name1) (atype2, -size2, name2))
                 list in
  let amp_ord = String.length (ultospun (List.length sorted)) + 2 in
  let amp_size = String.length (ultospun !max_file_size) in
  let amp_type = 6 in
  let header_1 = if print_frugal then begin
                   Printf.sprintf "%*s %s"
                                  amp_size
                                  "Size"
                                  "Title"
                   end else begin
                     Printf.sprintf "%*s %*s %*s %s"
                                  amp_ord "N "
                                  amp_size "Size"
                                  amp_type "Type "
                                  "Title"
                   end in
  let header_2 = if print_frugal then
                   Printf.sprintf "%s %s"
                                  (line amp_size)
                                  (line !max_filename_length) 
                 else
                   Printf.sprintf "%s %s %s %s"
                                (line amp_ord)
                                (line amp_size)
                                (line amp_type)
                                (line !max_filename_length) in
  ignore begin
      List.fold_left
        (fun (k, last_printed_type) (atype, size, name) ->
          let displayed_name =
            if String.length name <= max_displayed_title_length then begin
                name
              end else begin
                String.sub name 0 (max_displayed_title_length - 1) ^ "+"
              end in
          begin match print_headers, last_printed_type with
          | true, last_type when last_type <> Some atype ->
             if print_frugal then begin
                 Printf.printf "\n%s [%*s]%s" header_1 (amp_type - 2) atype vspace;
               end else begin
                 Printf.printf "\n%s%s" header_1 vspace;
               end;
             Printf.printf "%s%s" header_2 vspace;
          | _ -> ()
          end;
          if print_frugal then begin
              Printf.printf "%*s %s%s"
                            amp_size (ultospun size)
                            displayed_name
                            vspace
            end else begin
              Printf.printf "%*s %*s [%*s] %s%s"
                            amp_ord (Printf.sprintf "[%s]" (ultospun k))
                            amp_size (ultospun size)
                            (amp_type - 2) atype
                            displayed_name
                            vspace
            end;
          (succ k, Some atype))
        (1, None)
        sorted
    end;
  ();;
  
let default_frugal = true;;
let default_double_space = true;;
let print_headers = true;;
  
let () =
  if not !Sys.interactive then begin
      try match Array.length Sys.argv with
          | 1 -> fa kindle_default_mount_point true default_frugal default_double_space
          | 2 -> fa Sys.argv.(1) true default_frugal default_double_space
          | 3 -> begin match String.lowercase Sys.argv.(2) with
                 | "y" | "yes"-> fa Sys.argv.(1) print_headers true default_double_space
                 | "n" | "no" -> fa Sys.argv.(1) print_headers false default_double_space
                 | _ -> error_report_and_user_help ()
                 end
          | _ -> error_report_and_user_help ()
      with
      | Unix.Unix_error (error, func, args) ->
         Printf.fprintf stderr "Something's gone wrong while trying to get the Kindle file list.\n";
         if args = "" then begin
             Printf.fprintf stderr "The function %s got back a \"%s\"\n"
                            func (Unix.error_message error)
           end else begin
             Printf.fprintf stderr "The function %s applied to \"%s\" got back a \"%s\"\n"
                            func args (Unix.error_message error)
           end;
         exit 1
    end
;;

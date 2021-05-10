open Core

type entry_info =
  { name  : string
  ; title : string
  ; date  : Date.t
  ; tags  : string list
  ; draft : bool
  }

let entry_info_to_string {name; title; date; tags; draft} =
  sprintf "%s \"%s\" %s %s %B" name title (Date.to_string date) (String.concat ~sep:"," tags) draft

let is_gmi_file dir filename =
  let open Poly in
    Sys.is_file (Filename.concat dir filename) = `Yes
    && String.is_suffix filename ~suffix:".gmi"

let get_all_files dir =
  Sys.ls_dir dir
  |> List.filter ~f:(fun filename ->
     let open Poly in is_gmi_file dir filename && filename <> "index.gmi")

let absolute_entries_dir gemlog_dir entries_dir =
  if Filename.is_absolute entries_dir
  then entries_dir
  else Filename.concat gemlog_dir entries_dir

let find_title lines =
  List.filter ~f:(String.is_prefix ~prefix:"# ") lines
  |> List.hd
  |> (function
     | None -> "Untitled"
     | Some x -> String.drop_prefix x 2)
  |> String.strip

let find_date lines =
  List.filter ~f:(String.is_prefix ~prefix:"### ") lines
  |> List.hd
  |> (function
     | None -> "1900-01-01"
     | Some x -> String.drop_prefix x 4)
  |> Date.of_string

let find_tags lines =
  List.filter ~f:(String.is_prefix ~prefix:"tagi: ") lines
  |> List.hd
  |> function
      | None -> []
      | Some x -> String.drop_prefix x 6
                  |> String.split ~on:' '
                  |> List.map ~f:String.strip

let find_draft lines =
  List.exists ~f:(String.(=) "DRAFT") lines

let read_file_info dir filename =
  let lines = Stdio.In_channel.read_lines (Filename.concat dir filename)
  in { name = filename
     ; title = find_title lines
     ; date = find_date lines
     ; tags = find_tags lines
     ; draft = find_draft lines
  }

let build_url base_url gemlog_dir name =
  sprintf "%s/%s/%s" base_url gemlog_dir name

let build_tags = function
  | [] -> ""
  | tag_list -> " · " ^ String.concat ~sep:" " tag_list

let format_link base_url gemlog_dir {name; title; date; tags; _} =
  sprintf "=> %s %s · %s%s"
    (build_url base_url gemlog_dir name)
    (Date.to_string date)
    title
    (build_tags tags)

let format_entries base_url gemlog_dir entries =
  List.map ~f:(fun entry -> format_link base_url gemlog_dir entry) entries
  |> String.concat ~sep:"\n"

let create_processor base_url gemlog_dir entries =
  Template.template_processor [
    ("entries", fun _ -> format_entries base_url gemlog_dir entries) ;
    ("last_entries", fun command ->
      let num = String.split ~on:' ' command |> fun parts -> List.nth parts 1 |> function
        | None -> 0
        | Some n -> Int.of_string n
      in
      format_entries base_url gemlog_dir (List.take entries num))
  ]

let generate_gemlog_index base_url template_file gemlog_dir entries =
  let not_drafts = List.filter ~f:(fun {draft;_} -> not draft) entries in
  let processor = create_processor base_url gemlog_dir not_drafts in
  Template.process_template_file processor template_file

let write_index dir index =
  Stdio.Out_channel.write_lines (Filename.concat dir "index.gmi") index

let build_gemlog base_url directory entries_dir =
  let full_entries_dir = (Filename.concat directory entries_dir) in
  let gemlog_template_file = (Filename.concat full_entries_dir "index.gmi.tpl") in
  let capsule_template_file = (Filename.concat directory "index.gmi.tpl") in
  absolute_entries_dir directory entries_dir
  |> get_all_files
  |> List.map ~f:(read_file_info full_entries_dir)
  |> List.sort ~compare:(fun {date=date1; _} {date=date2; _} -> - Date.compare date1 date2)
  |> fun x ->
     write_index full_entries_dir (generate_gemlog_index base_url gemlog_template_file entries_dir x);
     write_index directory (generate_gemlog_index base_url capsule_template_file entries_dir x)

let command =
  Command.basic
    ~summary:"Generate gemlog"
    Command.Let_syntax.(
      let%map_open
          entry_dir = flag "-e" (optional_with_default "gemlog" string) ~doc:"string (sub)directory with gemlog entries"
      and base_url = anon ("base_url" %: string)
      and directory = anon ("directory" %: string)
      in
        fun () -> build_gemlog base_url directory entry_dir)

let () = Command.run ~version:"0.1" ~build_info:"RWO" command

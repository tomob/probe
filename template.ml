open Core

type processor = (string, string->string) Hashtbl.t

let template_processor handlers =
  Hashtbl.of_alist_exn (module String) handlers

let process_line (processor:processor) line =
  let full_command = String.strip line
    |> String.strip ~drop:(Char.(=) '{')
    |> String.strip ~drop:(Char.(=) '}')
  in
  let command =
    String.split ~on:' ' full_command
    |> List.hd
    |> function
    | None -> line
    | Some x -> x
  in
  match Hashtbl.find processor command with
  | None -> print_endline ("Unknown template variable: " ^ command); line
  | Some fn -> fn full_command

let process_template (processor:processor) lines =
  List.map lines ~f:(function
    | line when String.is_prefix ~prefix:"{{" line -> process_line processor line
    | line -> line )

let process_template_file (processor:processor) filename =
  Stdio.In_channel.read_lines filename |> process_template processor

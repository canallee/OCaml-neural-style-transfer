open Project
open Command
open Img
open Artpic

exception File_not_found of string
exception Invalid_method of string

let print_list f l =
  List.fold_left (fun _ y -> print_endline (f y)) () l

let find name flags = List.find (fun (x, _) -> x = name) flags

let remove_tmp () =
  let _ = Sys.command "rm -rf tmp" in
  ()

let remove_GIF_tmp () =
  let _ = Sys.command "rm -rf GIF_tmp" in
  ()

let remove_clean () =
  let _ = remove_tmp () in
  let _ = remove_GIF_tmp () in
  ()

let make_GIF cmd =
  let output_name = get_gif_name cmd in
  let output_gif_cmd =
    "convert -delay 40 -loop 0 GIF_tmp/*.png " ^ output_name ^ ".gif"
  in
  let _ = Sys.command output_gif_cmd in
  ()

let read_content () =
  print_string "> Content image: ";
  read_line ()

let read_style () =
  print_string "> Style image: ";
  read_line ()

let read_model () =
  print_string "> Pre-trained model: ";
  read_line ()

let read_flags () =
  print_string "> Flags: ";
  read_line ()

let read_output () =
  print_string "> Output file name: ";
  read_line ()

let read_method () =
  print_endline "> Artwork or picture? [artwork/picture] ";
  print_string "> ";
  read_line ()

let exists get cmd =
  if not (Sys.file_exists (get cmd)) then
    raise (File_not_found (get cmd))

let read_resize () =
  print_endline "> Resize? [yes/no] ";
  print_string "> ";
  let s = read_line () in
  if s = "yes" then true
  else if s = "no" then false
  else raise (Invalid_method s)

let rec make () =
  let content = read_content () in
  let style = read_style () in
  let pre_trained_model = read_model () in
  let flags = read_flags () in
  let output = read_output () in
  let response = read_method () in
  let model_name = pre_trained_model in
  let cmd = parse_make content style pre_trained_model flags output in
  if model_name <> "vgg16" && model_name <> "vgg19" then
    raise (Invalid_method model_name);
  exists get_content cmd;
  exists get_style cmd;
  exists get_model cmd;
  if response = "artwork" then
    if read_resize () then begin
      remove_clean ();
      artwork_resize_512 model_name cmd
    end
    else begin
      remove_clean ();
      artwork model_name cmd
    end
  else if response = "picture" then
    if read_resize () then begin
      remove_clean ();
      picture_resize_512 model_name cmd
    end
    else begin
      remove_clean ();
      picture model_name cmd
    end
  else raise (Invalid_method response);
  make_GIF cmd;
  remove_clean ();
  print_endline
    ("Output location: data" ^ Filename.dir_sep ^ "output"
   ^ Filename.dir_sep);
  print_string "> ";
  start ()
(* TODO: preprocess image + ml stuff. () |> make |> preprocessing |> ml
   to be () in the end. *)

and start () =
  match parse_input (read_line ()) with
  | exception End_of_file ->
      print_string "Critical error. ";
      print_string "> ";
      start ()
  | exception Invalid_command s ->
      print_endline ("Invalid command: " ^ s);
      print_string "> ";
      start ()
  | Quit -> ()
  | Info ->
      print_list Fun.id all_flags;
      print_string "> ";
      start ()
  | Make -> (
      try make () with
      | Invalid_method s ->
          print_endline ("Invalid: " ^ s);
          print_string "> ";
          start ()
      | Invalid_flag f ->
          print_endline ("Invalid flag: " ^ f);
          print_string "> ";
          start ()
      | Type_mismatch ->
          print_endline "Incorrect arguent type. ";
          print_string "> ";
          start ()
      | File_not_found s ->
          print_endline ("File not found: " ^ s);
          print_string "> ";
          start ()
      | Failure s ->
          print_endline s;
          print_string "> ";
          start ())
  | Help s -> (
      try
        print_endline (flag_info s);
        print_string "> ";
        start ()
      with Invalid_flag f ->
        print_endline ("Invalid flag: " ^ f);
        print_string "> ";
        start ())
  | Clean ->
      let _ = Sys.command "rm -rf data/output && mkdir data/output" in
      remove_GIF_tmp ();
      remove_tmp ();
      print_endline "Done. ";
      print_string "> ";
      start ()

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 neural transfer engine.\n";
  print_endline
    "Enter make to start process your image.\n\
     Enter help for a list of flags, or help <flag> for information \
     about a specific flag. ";
  print_string "> ";
  start ()

(* Execute the program. *)
let () = main ()
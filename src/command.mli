(** This is the parser for user inputs. *)

exception Invalid_flag of string
(** Raised when a wrong flag is encountered. *)

exception Type_mismatch
(** Raised when a wrong flag type is encountered. *)

exception Invalid_command of string
(** Raised when an invalid command is encountered. *)

(** The type [command] represents the parsed, user-inputted command. *)
type command =
  | Info
  | Help of string
  | Clean
  | Quit
  | Make

type flags = {
  style_weight : float;
  learning_rate : float;
  total_steps : int;
  layers_style_loss : int list;
  layers_content_loss : int list;
  k : int;
  sigma : float;
  size : float;
}
(** The type [flags] represents the arguments values. *)

type make_file
(** The type [make_file] includes data about the content image, style
    image, and optional arguments. *)

val all_flags : string list
(** [all_flags] is the list of all possible optional argument names. *)

val parse_input : string -> command
(** [parse_input] parses the input string and returns the corresponding
    command. *)

val parse_make :
  string -> string -> string -> string -> string -> make_file
(** [parse_make c s m f o] parses a user's input into a [make_file]. It
    ignores any unnecessary spaces. Requires: [m] is either ["vgg16"] or
    ["vgg19"]. Raises [Invalid_Flag] when an unexpected flag is
    encountered. Raises [TypeMismatch] if an optional argument has an
    incorrect type. *)

val flag_info : string -> string
(** [flag_info s] returns the description of the flag [s]. Raises
    [Invalid_Flag] when an unexpected flag is encountered. *)

val get_content : make_file -> string
(** [get_content cmd] returns the content image location in [cmd]. *)

val get_style : make_file -> string
(** [get_style cmd] returns the style image location in [cmd]. *)

val get_model : make_file -> string
(** [get_model cmd] returns the pre-trained model location in [cmd]. *)

val get_all_flags : make_file -> flags
(** [get_all_flags cmd] returns a tuple of values of all arguments in
    [cmd]. *)

val get_output : make_file -> string
(** [get_output cmd] returns the user-inputted output file name in
    [cmd]. *)

val get_gif_name : make_file -> string
(** [get_gif_name cmd] returns the user-inputted output gif name in
    [cmd]. *)

val default : flags
(** [default] is the default value of all flags read from the .json
    file. *)

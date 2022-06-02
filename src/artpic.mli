val tmp_file_loc : string -> string

(** [tmp_file_loc name] is the temperatory file name to store
    intermediate figures *)

val artwork : string -> Command.make_file -> unit

(** [artwork model_name cmd] creates the style transferred artwork, in
    the same size as the content image, transformed with the resized
    style image with size = size*[flgs.size]. [flgs.size] should be set
    to much smaller than 1 if the style image is too large*)

val artwork_resize_512 : string -> Command.make_file -> unit
(** [artwork_resize_512 model_name cmd]     creates the style transferred
    artwork, in the with size (512x512), transformed with the resized
    style image with size (512x512) and content image resized to size
    (512x512). This option is to enable getting results faster since the
    inputs will be smaller*)

val picture : string -> Command.make_file -> unit

(** [picture model_name cmd] creates the style transferred picture, in
    the same size as the content image, transformed with the resized
    style image's gradient image with size = size*[flgs.size].
    [flgs.size] should be set to much smaller than 1 if the style image
    is too large*)

val picture_resize_512 : string -> Command.make_file -> unit

(** [picture_resize_512 model_name cmd] creates the style transferred
    picture, in the with size (512x512), transformed with the resized
    style image's gradient image with size (512x512), and content image
    resized to size (512x512). This option is to enable getting results
    faster since the inputs will be smaller*)

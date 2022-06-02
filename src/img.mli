val img_resize : string -> string -> float -> unit
(** [img_resize file_in file_out size] Resize the input img named
    file_in (e.g. "data/cornell1.jpg") by making it 'size' times
    bigger/smaller and output as file_out. The input 'size' should be
    float. *)

val blur_gaussian : string -> string -> int -> float -> unit
(** [blur_gaussian file_in file_out k sigma] Blur input image named
    file_in by applying the k x k gaussian filter with standard
    deviation sigma, and output as file_out. Assume k is odd, sigma is
    float. *)

val gradient_graph : string -> string -> int -> float -> unit
(** [gradient_graph file_in file_out k sigma] Process input image named
    file_in by Computing the gradient magnitude at each pixel. If sigma
    >0, smooth the image with a gaussian first, and output as file_out.
    Assume k is odd, sigma is float. *)

val img_resize_default : string -> string -> unit
(** [img_resize_default file_in file_out] Same as [img_resize] except it
    always resize to 512 * 512 img. This is to convert larger images to
    smaller ones so that NST on them can be much faster.*)

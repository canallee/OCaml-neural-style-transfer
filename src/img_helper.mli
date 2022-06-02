val normalize : Torch.Tensor.t -> Torch.Tensor.t
(** [normalize tensor] is normalized tensor with ImageNet mean and
    standard deviation. Warning: This is copied from OCaml-Torch source
    code, see LOC.txt *)

val unnormalize : Torch.Tensor.t -> Torch.Tensor.t
(** [unnormalize tensor] is unnormalized tensor from the normalized
    tensor, with ImageNet mean and standard deviation. Warning: This is
    copied from OCaml-Torch source code, see LOC.txt *)

val output_tensor : Torch.Tensor.t -> Torch.Tensor.t
(** [output_tensor tensor] is the torch tensor without ok condition.*)

val get_shape_str : Torch.Tensor.t -> string
(** [get_shape_str tensor] is the string representing the shape of the
    tensor.*)

val get_img_size_height : Torch.Tensor.t -> int
(** [get_img_size_height tensor] is the int representing the height of
    an image.*)

val get_img_size_width : Torch.Tensor.t -> int
(** [get_img_size_width tensor] is the int representing the width of an
    image.*)

val print_shape : Torch.Tensor.t -> unit
(** [print_shape tensor] prints the shape of the tensor.*)

val read_img_to_tensor : string -> Torch.Tensor.t
(** load_image_no_resize_and_crop reads the image, store it in a
    <1,channel,width,hight> tensor; [read_img_to_tensor] is the
    <channel,width,hight> tensor, which is the representation for the
    read image. Warning: this doesn't normalize the image !!! *)


val read_img_to_tensor_reshape : string -> int * int -> Torch.Tensor.t
(** Basically the same function as [read_img_to_tensor], but allow
    reshaping into [size] where [size] is (width, height). Warning: this
    doesn't normalize the image !!！ *)

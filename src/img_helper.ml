open Torch
open Torch_vision

let mean_, std_ =
  [ (0.485, 0.229); (0.456, 0.224); (0.406, 0.225) ] |> Base.List.unzip

(* ############## THIS IS COPIED FROM TORCH SOURCE CODE ##############
   Warning: This is copied from OCaml-Torch source code, see LOC.txt!!!
   We need these to have normalize and unnormalize working for torch*)

let mean_ =
  lazy (Tensor.float_vec mean_ |> Tensor.view ~size:[ 3; 1; 1 ])

let std_ = lazy (Tensor.float_vec std_ |> Tensor.view ~size:[ 3; 1; 1 ])

let normalize tensor =
  let mean_ = Lazy.force mean_ in
  let std_ = Lazy.force std_ in
  let tensor = Tensor.to_type tensor ~type_:(T Float) in
  Tensor.(((tensor / f 255.) - mean_) / std_)

let unnormalize tensor =
  let mean_ = Lazy.force mean_ in
  let std_ = Lazy.force std_ in
  Tensor.(((tensor * std_) + mean_) * f 255.)
  |> Tensor.clamp ~min:(Scalar.float 0.) ~max:(Scalar.float 255.)
  |> Tensor.to_type ~type_:(T Uint8)

(* ############# Above IS COPIED FROM TORCH SOURCE CODE ############ *)

let output_tensor tensor = Tensor.to_type tensor ~type_:(T Float)
let get_shape_str tensor = Tensor.shape_str tensor

let get_img_size_height tensor =
  let lst = get_shape_str tensor |> String.split_on_char ',' in
  List.nth lst 1 |> String.trim |> int_of_string

let get_img_size_width tensor =
  let lst = get_shape_str tensor |> String.split_on_char ',' in
  List.nth lst 2 |> String.trim |> int_of_string

let print_shape tensor =
  Stdio.print_endline
    ("The shape of this tensor is: " ^ get_shape_str tensor)

let read_img_to_tensor (filename : string) : Tensor.t =
  Imagenet.load_image_no_resize_and_crop filename
  |> unnormalize |> Tensor.to_list |> List.hd

let read_img_to_tensor_reshape (filename : string) (size : int * int) :
    Tensor.t =
  let load_image filename size =
    Image.load_image filename ~resize:size
    |> Base.Or_error.ok_exn |> output_tensor
  in
  load_image filename size |> Tensor.to_list |> List.hd

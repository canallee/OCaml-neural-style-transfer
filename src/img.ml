open Torch
open Torch_vision
open Img_helper

(* ############## BELOW ARE FILTER FUNCTIONS ############## *)
let reverse a =
  for i = 0 to Array.length a / 2 do
    let temp = a.(i) in
    a.(i) <- a.(Array.length a - i - 1);
    a.(Array.length a - i - 1) <- temp
  done;
  a

let sum a =
  let v = Array.make (Array.length a) 0 in
  for i = 0 to Array.length a - 1 do
    v.(i) <- Array.fold_left ( + ) 0 a.(i)
  done;
  Array.fold_left ( + ) 0 v

let sum_float a =
  let v = Array.make (Array.length a) 0.0 in
  for i = 0 to Array.length a - 1 do
    v.(i) <- Array.fold_left ( +. ) 0.0 a.(i)
  done;
  Array.fold_left ( +. ) 0.0 v

let mult a b =
  let len1, len2 = (Array.length a, Array.length a.(0)) in
  let v = Array.make_matrix len1 len2 0.0 in
  for i = 0 to len1 - 1 do
    for j = 0 to len2 - 1 do
      v.(i).(j) <- a.(i).(j) *. b.(i).(j)
    done
  done;
  v

let mult_const a b =
  let len1, len2 = (Array.length a, Array.length a.(0)) in
  let v = Array.make_matrix len1 len2 0.0 in
  for i = 0 to len1 - 1 do
    for j = 0 to len2 - 1 do
      v.(i).(j) <- a.(i).(j) *. b
    done
  done;
  v

let add a b =
  let len1, len2 = (Array.length a, Array.length a.(0)) in
  let v = Array.make_matrix len1 len2 0.0 in
  for i = 0 to len1 - 1 do
    for j = 0 to len2 - 1 do
      v.(i).(j) <- a.(i).(j) +. b.(i).(j)
    done
  done;
  v

let add_ave a b c =
  let len1, len2 = (Array.length a, Array.length a.(0)) in
  let v = Array.make_matrix len1 len2 0.0 in
  for i = 0 to len1 - 1 do
    for j = 0 to len2 - 1 do
      v.(i).(j) <- (a.(i).(j) +. b.(i).(j) +. c.(i).(j)) /. 3.0
    done
  done;
  v

let part a init_x lenx init_y leny =
  let v = Array.make_matrix lenx leny 0.0 in
  for x = 0 to lenx - 1 do
    for y = 0 to leny - 1 do
      v.(x).(y) <- a.(x + init_x).(y + init_y)
    done
  done;
  v

let pow s a =
  let len1, len2 = (Array.length a, Array.length a.(0)) in
  let v = Array.make_matrix len1 len2 0.0 in
  for i = 0 to len1 - 1 do
    for j = 0 to len2 - 1 do
      v.(i).(j) <- a.(i).(j) ** s
    done
  done;
  v

(* [convolve img filt] Performs a convolution of an image with a filter.
   Assume filter is 2D and has an odd size. Assume image is grayscale.
   The output image should be the same size as the input. *)
let convolve img filt =
  let x = Array.length img in
  let y = Array.length img.(0) in
  let fx = Array.length filt in
  let fy = Array.length filt.(0) in
  let padding_x = (fx - 1) / 2 in
  let padding_y = (fy - 1) / 2 in
  let padded = Array.make_matrix (x + fx - 1) (y + fy - 1) 0.0 in
  let new_padded =
    for i = 0 to x - 1 do
      for j = 0 to y - 1 do
        padded.(i + padding_x).(j + padding_y) <- img.(i).(j)
      done
    done;
    padded
  in
  let result = Array.make_matrix x y 0.0 in
  let new_filt = reverse filt in
  let nn_filt =
    for i = 0 to fx - 1 do
      new_filt.(i) <- reverse new_filt.(i)
    done;
    new_filt
  in
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      result.(i).(j) <-
        part new_padded i fx j fy |> mult nn_filt |> sum_float
    done
  done;
  result

(* [gaussian_filter k sigma] Produces a k x k gaussian filter with
   standard deviation sigma. Assume k is odd, sigma is float. *)
let gaussian_filter k sigma =
  let width = (k - 1) / 2 in
  let func x y =
    Float.exp (~-.((x ** 2.) +. (y ** 2.)) /. (2. *. (sigma ** 2.)))
  in
  let filt = Array.make_matrix k k 0.0 in
  for i = 0 to k - 1 do
    for j = 0 to k - 1 do
      filt.(i).(j) <-
        func (Float.of_int (j - width)) (Float.of_int (i - width))
    done
  done;
  let s = sum_float filt in
  for i = 0 to k - 1 do
    for j = 0 to k - 1 do
      filt.(i).(j) <- filt.(i).(j) /. s
    done
  done;
  filt

(* [dx_filter] Produces a 1 x 3 filter that computes the derivative in x
   direction. *)
let dx_filter =
  let a = Array.make_matrix 1 3 0.0 in
  a.(0).(0) <- 1.0;
  a.(0).(1) <- ~-.1.0;
  a

(* [dy_filter] Produces a 3 x 1 filter that computes the derivative in y
   direction. *)
let dy_filter =
  let a = Array.make_matrix 3 1 0.0 in
  a.(0).(0) <- 1.0;
  a.(1).(0) <- ~-.1.0;
  a

(* [gradient_magnitude img k sigma] Compute the gradient magnitude at
   each pixel. If sigma >0, smooth the image with a gaussian first. *)
let gradient_magnitude img k sigma =
  let new_img =
    if sigma > 0.0 then convolve img (gaussian_filter k sigma) else img
  in
  let dx = convolve new_img dx_filter in
  let dy = convolve new_img dy_filter in
  add (pow 2.0 dx) (pow 2.0 dy) |> pow 0.5

(* [mean_filter k] Produces a k x k mean filter. Assume k is odd. *)
let mean_filter k = Array.make_matrix k k (1. /. (Float.of_int k ** 2.))

(* [threshold thresh img] Produces an image where pixels greater than
   thresh are assigned 1 and those less than thresh are assigned 0. *)
let threshold thresh img =
  let func x = if x > thresh then 1.0 else 0.0 in
  let len1, len2 = (Array.length img, Array.length img.(0)) in
  let v = Array.make_matrix len1 len2 0.0 in
  for i = 0 to len1 - 1 do
    for j = 0 to len2 - 1 do
      v.(i).(j) <- func img.(i).(j)
    done
  done;
  v

let clip img low high =
  let len1, len2 = (Array.length img, Array.length img.(0)) in
  let v = Array.make_matrix len1 len2 0.0 in
  for i = 0 to len1 - 1 do
    for j = 0 to len2 - 1 do
      v.(i).(j) <-
        (let m = if img.(i).(j) > low then img.(i).(j) else low in
         if m < high then m else high)
    done
  done;
  v

let multiply img alpha = clip (mult_const img alpha) 0.0 1.0

let amax a =
  let v = Array.make (Array.length a) 0.0 in
  for i = 0 to Array.length a - 1 do
    v.(i) <-
      Array.fold_left
        (fun a b -> if a > b then a else b)
        a.(i).(0)
        a.(i)
  done;
  Array.fold_left (fun a b -> if a > b then a else b) v.(0) v

let amin a =
  let v = Array.make (Array.length a) 0.0 in
  for i = 0 to Array.length a - 1 do
    v.(i) <-
      Array.fold_left
        (fun a b -> if a < b then a else b)
        a.(i).(0)
        a.(i)
  done;
  Array.fold_left (fun a b -> if a < b then a else b) v.(0) v

(* [fnormalize img] Performs an affine transformation of the intensities
   (i.e., f'(x,y) = af(x,y) + b) with a and b chosen so that the minimum
   value is 0 and the maximum value is 1. If all pixels in the image
   have the same intensity, then return an image that is 0.0
   everywhere. *)
let fnormalize img =
  let len1, len2 = (Array.length img, Array.length img.(0)) in
  let vmax = amax img in
  let vmin = amin img in
  let a = 1.0 /. (vmin -. vmax) in
  let b = vmin /. (vmin -. vmax) in
  let func x = (a *. x) +. b in
  if
    Array.for_all
      (fun x -> Array.for_all (fun y -> y = img.(0).(0)) x)
      img
  then Array.make_matrix len1 len2 0.0
  else
    let v = Array.make_matrix len1 len2 0.0 in
    for i = 0 to len1 - 1 do
      for j = 0 to len2 - 1 do
        v.(i).(j) <- func img.(i).(j)
      done
    done;
    v

(* ############## ABOVE ARE FILTER FUNCTIONS ############## *)

let img_resize file_in file_out size =
  let old_size_w =
    read_img_to_tensor file_in |> get_img_size_width |> Float.of_int
  in
  let old_size_H =
    read_img_to_tensor file_in |> get_img_size_height |> Float.of_int
  in
  let img_tensor =
    read_img_to_tensor_reshape file_in
      ( Float.to_int (old_size_w *. size),
        Float.to_int (old_size_H *. size) )
  in
  (* get the float array array array from the tensor *)
  let img_3d_float = img_tensor |> Tensor.to_float3_exn in
  (* get tensor from the 3d float, tensor_from_3d is <3,256,256> *)
  let tensor_from_3d = img_3d_float |> Tensor.of_float3 in
  (* normalize only works for tensor of 3 channels!!! *)
  let normalized = tensor_from_3d |> normalize in
  (* write to output *)
  Imagenet.write_image ~filename:file_out normalized

let blur_gaussian file_in file_out k sigma =
  let old_size_w =
    read_img_to_tensor file_in |> get_img_size_width |> Float.of_int
  in
  let old_size_H =
    read_img_to_tensor file_in |> get_img_size_height |> Float.of_int
  in
  let img_tensor =
    read_img_to_tensor_reshape file_in
      ( Float.to_int (old_size_w *. 1.0),
        Float.to_int (old_size_H *. 1.0) )
  in
  (* get the float array array array from the tensor *)
  let img_3d_float = img_tensor |> Tensor.to_float3_exn in
  (* [red_channel_2d] gets the first channel of the 3 channel layers *)
  let red_channel_2d = img_3d_float.(0) in
  let green_channel_2d = img_3d_float.(1) in
  let blue_channel_2d = img_3d_float.(2) in
  let filt = gaussian_filter k sigma in
  let gaussian_red_2d = convolve red_channel_2d filt in
  let gaussian_green_2d = convolve green_channel_2d filt in
  let gaussian_blue_2d = convolve blue_channel_2d filt in
  (* [img_3channel_float] is of size 3, 256, 256 *)
  let img_3channel_float =
    [| gaussian_red_2d; gaussian_green_2d; gaussian_blue_2d |]
  in
  (* get tensor from the 3d float, tensor_from_3d is <3,256,256> *)
  let tensor_from_3d = img_3channel_float |> Tensor.of_float3 in
  (* normalize only works for tensor of 3 channels!!! *)
  let normalized = tensor_from_3d |> normalize in
  (* write to output, Imagenet.write_image only takes Tensor<3,wid,high>
     or Tensor<1,wid,high>*)
  Imagenet.write_image ~filename:file_out normalized

let gradient_graph file_in file_out k sigma =
  let old_size_w =
    read_img_to_tensor file_in |> get_img_size_width |> Float.of_int
  in
  let old_size_H =
    read_img_to_tensor file_in |> get_img_size_height |> Float.of_int
  in
  let img_tensor =
    read_img_to_tensor_reshape file_in
      ( Float.to_int (old_size_w *. 1.0),
        Float.to_int (old_size_H *. 1.0) )
  in
  (* normalize only works for tensor of 3 channels!!! *)
  let normalized = img_tensor |> normalize in
  (* get the float array array array from the tensor *)
  let img_3d_float = normalized |> Tensor.to_float3_exn in
  (* [red_channel_2d] gets the first channel of the 3 channel layers *)
  let red_channel_2d = img_3d_float.(0) in
  let green_channel_2d = img_3d_float.(1) in
  let blue_channel_2d = img_3d_float.(2) in
  let grey_2d =
    add_ave red_channel_2d green_channel_2d blue_channel_2d
  in
  let gradient_2d = gradient_magnitude grey_2d k sigma in
  (* [img_channel_float] is of size 3, 256, 256 *)
  let img_channel_float = [| threshold 0.1 gradient_2d |] in
  (* get tensor from the 3d float, tensor_from_3d is <3,256,256> *)
  let tensor_from_3d = img_channel_float |> Tensor.of_float3 in
  (* write to output, Imagenet.write_image only takes Tensor<3,wid,high>
     or Tensor<1,wid,high>*)
  Imagenet.write_image ~filename:file_out tensor_from_3d

let img_resize_default file_in file_out =
  let img_tensor = read_img_to_tensor_reshape file_in (512, 512) in
  (* get the float array array array from the tensor *)
  let img_3d_float = img_tensor |> Tensor.to_float3_exn in
  (* get tensor from the 3d float, tensor_from_3d is <3,256,256> *)
  let tensor_from_3d = img_3d_float |> Tensor.of_float3 in
  (* normalize only works for tensor of 3 channels!!! *)
  let normalized = tensor_from_3d |> normalize in
  (* write to output *)
  Imagenet.write_image ~filename:file_out normalized

(** [get_full_img file_in file_out] reads and stores the exactly same
    image of the input file at [file_out]. This function is only used
    for debugging gradient graph*)
let get_full_img file_in file_out =
  (* get the tensor, for example Tensor<3,1024,1024> from the image *)
  let img_tensor = read_img_to_tensor_reshape file_in (1024, 1024) in
  (* get the float array array array from the tensor *)
  let img_3d_float = img_tensor |> Tensor.to_float3_exn in
  (* get tensor from the 3d float, tensor_from_3d is <3,256,256> *)
  let tensor_from_3d = img_3d_float |> Tensor.of_float3 in
  (* normalize only works for tensor of 3 channels!!! *)
  let normalized = tensor_from_3d |> normalize in
  (* write to output *)
  Imagenet.write_image ~filename:file_out normalized

(** [get_one_channel file_in file_out] reads and stores the Red channel
    of the input file at [file_out]. This function is only used for
    debugging gradient graph*)
let get_one_channel file_in file_out =
  (* get the tensor, for example Tensor<3,256,256> from the image *)
  let img_tensor = read_img_to_tensor_reshape file_in (256, 256) in
  (* get the float array array array from the tensor *)
  let img_3d_float = img_tensor |> Tensor.to_float3_exn in
  (* [red_channel_2d] gets the first channel of the 3 channel layers *)
  let red_channel_2d = img_3d_float.(0) in
  (* [img_red_channel_float] is of size 1, 256, 256 *)
  let img_red_channel_float = [| red_channel_2d |] in
  (* get tensor from the 3d float, tensor_from_3d is <1,256,256> *)
  let tensor_from_3d = img_red_channel_float |> Tensor.of_float3 in
  (* normalize only works for tensor of 3 channels!!! *)
  let normalized = tensor_from_3d |> normalize in
  (* write to output, Imagenet.write_image only takes Tensor<3,wid,high>
     or Tensor<1,wid,high>*)
  Imagenet.write_image ~filename:file_out normalized

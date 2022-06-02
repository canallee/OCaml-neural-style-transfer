open Base
open Torch

(* ############## THIS IS COPIED FROM TORCH SOURCE CODE ##############
   Warning: This is copied from OCaml-Torch source code, see LOC.txt!!!
   To allow us to modify the vgg module to enable using more vgg
   pretrained weights, we copied needed source code from vgg.ml in
   torch.vision *)

let relu = Layer.of_fn_ (fun xs ~is_training:_ -> Tensor.relu xs)
let relu_ = Layer.of_fn_ (fun xs ~is_training:_ -> Tensor.relu_ xs)

type t =
  | C of int
  (* conv2d *)
  | M
(* maxpool2d *)

let layers_cfg = function
  | `A ->
      [
        C 64;
        M;
        C 128;
        M;
        C 256;
        C 256;
        M;
        C 512;
        C 512;
        M;
        C 512;
        C 512;
        M;
      ]
  | `B ->
      [
        C 64;
        C 64;
        M;
        C 128;
        C 128;
        M;
        C 256;
        C 256;
        M;
        C 512;
        C 512;
        M;
        C 512;
        C 512;
        M;
      ]
  | `D ->
      [
        C 64;
        C 64;
        M;
        C 128;
        C 128;
        M;
        C 256;
        C 256;
        C 256;
        M;
        C 512;
        C 512;
        C 512;
        M;
        C 512;
        C 512;
        C 512;
        M;
      ]
  | `E ->
      [
        C 64;
        C 64;
        M;
        C 128;
        C 128;
        M;
        C 256;
        C 256;
        C 256;
        C 256;
        M;
        C 512;
        C 512;
        C 512;
        C 512;
        M;
        C 512;
        C 512;
        C 512;
        C 512;
        M;
      ]

let make_layers vs cfg ~batch_norm ~in_place_relu =
  let relu = if in_place_relu then relu_ else relu in
  let sub_vs index = Var_store.sub vs (Int.to_string index) in
  let (_output_dim, _output_idx), layers =
    List.fold_map (layers_cfg cfg) ~init:(3, 0)
      ~f:(fun (input_dim, idx) v ->
        match v with
        | M ->
            ( (input_dim, idx + 1),
              [
                Layer.of_fn (Tensor.max_pool2d ~ksize:(2, 2))
                |> Layer.with_training;
              ] )
        | C output_dim ->
            let conv2d =
              Layer.conv2d_ (sub_vs idx) ~ksize:3 ~stride:1 ~padding:1
                ~input_dim output_dim
              |> Layer.with_training
            in
            if batch_norm then
              let batch_norm =
                Layer.batch_norm2d (sub_vs (idx + 1)) output_dim
              in
              ((output_dim, idx + 3), [ conv2d; batch_norm; relu ])
            else ((output_dim, idx + 2), [ conv2d; relu ]))
  in
  List.concat layers
(* ####################### END OF COPY ####################### *)

(* This is customized to ensure that we can use different VGG models.
   The Ocaml module only allow using vgg16 layers *)
let vgg_layers ?(max_layer = Int.max_value) vs ~batch_norm vgg_name =
  let layer_config =
    match vgg_name with
    | "vgg11" -> `A
    | "vgg13" -> `B
    | "vgg16" -> `D
    | "vgg19" -> `E
    | _ -> failwith "No such vgg model"
  in
  let layers =
    List.take
      (make_layers
         (Var_store.sub vs "features")
         layer_config ~batch_norm ~in_place_relu:false)
      max_layer
  in
  Stdio.print_endline (">>>> using pretrained weight from " ^ vgg_name);
  (* [Staged.stage] just indicates that the [vs] and [~indexes]
     parameters should only be applied on the first call to this
     function. *)
  Staged.stage (fun xs ->
      List.fold_mapi layers ~init:xs ~f:(fun i xs layer ->
          let xs = Layer.forward_ layer xs ~is_training:false in
          (xs, (i, xs)))
      |> fun (_, indexed_layers) ->
      Map.of_alist_exn (module Int) indexed_layers)
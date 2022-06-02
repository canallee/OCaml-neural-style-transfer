open Torch

let gram_matrix m =
  let a, b, c, d = Tensor.shape4_exn m in
  let m = Tensor.view m ~size:[ a * b; c * d ] in
  let g = Tensor.mm m (Tensor.tr m) in
  Tensor.( / ) g (Float.of_int (a * b * c * d) |> Tensor.f)

let get_style_loss input_layers style_layers layers_for_loss =
  let style_loss m1 m2 =
    Tensor.mse_loss (gram_matrix m1) (gram_matrix m2)
  in
  let loss_new_layers lst =
    style_loss
      (Base.Map.find_exn input_layers lst)
      (Base.Map.find_exn style_layers lst)
  in
  List.(map loss_new_layers layers_for_loss
  |> fold_left Tensor.( + ) (Tensor.of_float0 0.0))

let get_content_loss input_layers content_layers layers_for_loss =
  let loss_new_layers lst =
    Tensor.mse_loss
      (Base.Map.find_exn input_layers lst)
      (Base.Map.find_exn content_layers lst)
  in
  List.(map loss_new_layers layers_for_loss 
  |> fold_left Tensor.( + ) (Tensor.of_float0 0.0))

let get_combined_loss style_loss style_weight content_loss =
  Tensor.((style_loss * f style_weight) + content_loss)

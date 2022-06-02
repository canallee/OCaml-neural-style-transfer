val gram_matrix : Torch.Tensor.t -> Torch.Tensor.t
(** [gram_matrix m] returns the gram matrix tensor for the input tensor
    [m]*)

val get_style_loss :
  ('a, Torch.Tensor.t, 'b) Base.Map.t ->
  ('a, Torch.Tensor.t, 'c) Base.Map.t ->
  'a list ->
  Torch.Tensor.t
(** [get_style_loss input_layers style_layers layers_for_loss] gets the
    style loss base on both the input layers and the style layers. Style
    loss measures how the level of style reconstructed from the style
    image. The smaller the better *)

val get_content_loss :
  ('a, Torch.Tensor.t, 'b) Base.Map.t ->
  ('a, Torch.Tensor.t, 'c) Base.Map.t ->
  'a list ->
  Torch.Tensor.t
(** [get_content_loss input_layers content_layers layers_for_loss] gets
    the content loss base on both the input layers and the content
    layers. Content loss measures how much the original content is lost.
    The smaller the better *)

val get_combined_loss :
  Torch.Tensor.t -> float -> Torch.Tensor.t -> Torch.Tensor.t
(** [style_loss style_weight content_loss] is the combined loss form
    style_loss and content_loss, following the equation combined loss =
    [style_loss * style_weight + content_loss]. Because back prop can
    only act on one returned value, two losses need to be composed. *)

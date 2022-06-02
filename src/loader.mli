val load_vgg_model :
  string ->
  string ->
  int list ->
  int list ->
  Torch_core.Device.t ->
  Torch.Tensor.t ->
  (int, Torch.Tensor.t, Base.Int.comparator_witness) Base.Map.t
(** [load_vgg_model vgg_name load_weight_path style_layers content_layers cpu]
    contructs the map between layers (tensor) of vgg model and number for them.
    The vgg model weights is loaded from [load_weight_path], with model
    specification same as [vgg_name]. [style_layers] and
    [content_layers] are the lists of int specifying which layers from
    the model will be used in the nst model. The model will be stored in
    cpu*)

val load_style_img : string -> Torch_core.Device.t -> Torch.Tensor.t
(** [load_style_img style_img cpu] is the style image in tensor*)

val load_content_img : string -> Torch_core.Device.t -> Torch.Tensor.t
(** [load_content_img content_img cpu] is the content image in tensor*)
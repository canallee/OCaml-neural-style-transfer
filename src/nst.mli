val get_inputs_tensors :
  string ->
  Torch_core.Device.t ->
  string ->
  string ->
  string ->
  (Torch.Tensor.t ->
  (int, Torch.Tensor.t, Base.Int.comparator_witness) Base.Map.t)
  * Torch.Tensor.t
  * Torch.Tensor.t
(** [get_inputs_tensors model_name cpu style_img_path 
    content_img_path weight_path]
    is the tensors represent [(model, style_img, content_img)]. Since
    Ocaml-Torch does not support CUDA, the only device usable is cpu*)

val training_nst :
  (Torch.Tensor.t -> (int, Torch.Tensor.t, 'a) Base.Map.t) ->
  Torch.Tensor.t ->
  Torch.Optimizer.t ->
  (int, Torch.Tensor.t, 'b) Base.Map.t ->
  (int, Torch.Tensor.t, 'c) Base.Map.t ->
  int ->
  unit
(** [training_nst model generated_img optimizer style_layers 
    content_layers total_steps]
    trains the style transfer model. It also saves temperatory output
    files to create gif in ["/GIF_tmp"].*)

val main :
  string ->
  string ->
  string ->
  string ->
  Command.flags ->
  string ->
  unit
(** [main model_name style content model input_flags output] creates the
    neural style transfer image with the specified inputs, and store the
    generated artwork/picture in [output].*)

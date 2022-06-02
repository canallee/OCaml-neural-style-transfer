val vgg_layers :
  ?max_layer:int ->
  Torch.Var_store.t ->
  batch_norm:bool ->
  string ->
  (Torch.Tensor.t ->
  (int, Torch.Tensor.t, Base.Int.comparator_witness) Base.Map.t)
  Base.Staged.t

(** [vgg_layers ?max_layer vs ~batch_norm vgg_name] is the vgg
    convolution layers up till the [max_layer]. [vs] is the model
    parameters and [~batch_norm] is the boolean denoting whether
    BatchNorm is used in the pretrained model. The returned layers are
    following the definition with [vgg_name], the name for the vgg
    model, such as vgg19 or vgg13 *)

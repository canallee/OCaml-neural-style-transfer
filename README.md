# OCaml-neural-style-transfer
This is the final project for Cornell CS3110 - Data Structures and Functional Programming. This repo belongs to Canal Li, Thomas Cui and  Canwen Zhang. This project partially adapts from examples in PyTorch Ocaml binding: https://github.com/LaurentMazare/ocaml-torch 

Step 0: update system
`sudo apt update`
`sudo apt upgrade`

Step 1: install required packages
`sudo apt install pkg-config libffi-dev zlib1g-dev imagemagick`

Step 2: install ocaml pytorch
`opam install torch ANSITerminal`

Step 3: 
For MS3, we now enable to use different VGG models.
Download the pretrained weights from: 
vgg16: https://github.com/LaurentMazare/ocaml-torch/releases/download/v0.1-unstable/vgg16.ot
vgg19: https://github.com/LaurentMazare/ocaml-torch/releases/download/v0.1-unstable/vgg19.ot
and save the file in folder `/resources`

To run the engine,
`make launch`
Content image can be any image (should be smaller than 1k otherwise likely out of memory)
    - Here use `cornell` as an example
Style image should be any image (should be smaller than 1k otherwise likely out of memory)
    - Here use `starry` as an example

Pre-trained model can be any vgg models,
But currently Ocaml Torch only provides `vgg16.ot` and `vgg19.ot` for downloads
## For both VGG19 and VGG16, the default flags that work (for both of them):
`-layers_style_loss [2,10,14,21,28]`
`-layers_content_loss [21]`
`-style_weight 9e5`
`-learning_rate 8e-2`
`-total_steps 80`

## We will update this list as soon as we add more pretrained models
The default flags for filtering/resizing are:
`-k 5`
`-sigma 0.75`
All of the flags are optional. To specify a flag value, use `-flag value`. The flags not specified will use the default values. 
What we generated in MS1 is an "artwork". What we generated in MS2 is a "picture". The differences are explained in our progress report. 

The output artwork is in `/data/output`
The gif for correspond artwork is in `/data/output`
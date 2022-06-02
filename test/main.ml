(** The following features are tested in this file with glass-box
    testing method as well as randomized cases: 1. Default values for
    the flags 2. The information about the flags 3. Parsing valid
    inputted commands 4. Parsing invalid inputted commands 5. For
    functions with img as input and output: check img size.

    By using glass-box testing, we tested the series of expressions that
    is conditionally evaluated based on if-expressions,
    match-expressions, and function applications. In other words, our
    test cases are path-complete.

    For the Ocaml-Torch related testing, it's not possible to create
    testcases specifically for the tensors in the network, because 1)
    the optimzation/backpropo is determined by autogradient calculation,
    and would be extremely difficult to derive manually; 2) the neural
    network is more a black box, the best way to test it is imperatively
    running examples and mointor outputs. We included a bunch of utility
    functions in img.ml and img_helper.ml so we can monitor the shapes
    of different layers by printing the tensors' shapes.

    The following features are not tested in this file: 1. User
    interface, for which we manually tested by play around with our
    engine. *)

open OUnit2
open Project
open Command
open Img
open Img_helper

let rec assert_deep_equal lst1 lst2 printer =
  match (lst1, lst2) with
  | [], [] -> ()
  | _ :: _, [] | [], _ :: _ -> failwith "unequal lengths"
  | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then assert_deep_equal t1 t2 printer
      else failwith (printer h1 ^ " not equal to " ^ printer h2)

(* Test cases for Command. *)
let all_flags_test =
  "all flags according to help.json" >:: fun _ ->
  assert_deep_equal
    [
      "-k : int";
      "-layers_content_loss : int list";
      "-layers_style_loss : int list";
      "-learning_rate : float";
      "-sigma : float";
      "-size : float";
      "-style_weight : float";
      "-total_steps : int";
    ]
    all_flags Fun.id

let remove_tmp () =
  let _ = Sys.command "rm -rf tmp" in
  ()

let make_output () =
  let _ = Sys.command "mkdir data/output" in
  ()

let remove_output () =
  let _ = Sys.command "rm -rf data/output" in
  ()

let remove_GIF_tmp () =
  let _ = Sys.command "rm -rf GIF_tmp" in
  ()

let remove_clean () =
  let _ = remove_tmp () in
  let _ = remove_GIF_tmp () in
  ()

let flag_info_test name flag expected_output =
  name >:: fun _ ->
  assert_equal (flag_info flag) expected_output ~printer:Fun.id

let content_test name cmd expected_output =
  name >:: fun _ ->
  assert_equal (get_content cmd) expected_output ~printer:Fun.id

let style_test name cmd expected_output =
  name >:: fun _ ->
  assert_equal (get_style cmd) expected_output ~printer:Fun.id

let flags_test name cmd expected_output =
  name >:: fun _ -> assert_equal (get_all_flags cmd) expected_output

let output_test name cmd expected_output =
  name >:: fun _ ->
  assert_equal (get_output cmd) expected_output ~printer:Fun.id

let model_test name cmd expected_output =
  name >:: fun _ ->
  assert_equal (get_model cmd) expected_output ~printer:Fun.id

let gif_test name cmd expected_output =
  name >:: fun _ ->
  assert_equal (get_gif_name cmd) expected_output ~printer:Fun.id

let parse_fail_flag name flags inv_flg =
  name >:: fun _ ->
  assert_raises (Invalid_flag inv_flg) (fun () ->
      parse_make "" "" "" flags "")

let parse_fail_type name flags =
  name >:: fun _ ->
  assert_raises Type_mismatch (fun () -> parse_make "" "" "" flags "")

let default_style_weight_test =
  "default style weight" >:: fun _ ->
  assert_equal default.style_weight 8e7

let default_learning_rate_test =
  "default learning rate" >:: fun _ ->
  assert_equal default.learning_rate 8e-2

let default_total_steps_test =
  "default total steps" >:: fun _ -> assert_equal default.total_steps 30

let default_layers_style_loss_test =
  "default layers style loss" >:: fun _ ->
  assert_equal default.layers_style_loss [ 2; 10; 14; 21; 28 ]

let default_layers_content_loss_test =
  "default layers content loss" >:: fun _ ->
  assert_equal default.layers_content_loss [ 21 ]

let default_k_test = "default k" >:: fun _ -> assert_equal default.k 5

let default_sigma_test =
  "default sigma" >:: fun _ -> assert_equal default.sigma 1.2

let cmd1 = parse_make "default" "default" "vgg16" "" ""

let test_cmd1 =
  [
    content_test "testing for cmd: content cmd1" cmd1 "data/default.jpg";
    style_test "testing for cmd: style cmd1" cmd1 "data/default.jpg";
    flags_test "testing for cmd: flags cmd1" cmd1 default;
    output_test "testing for cmd: output cmd1" cmd1
      "data/output/art.png";
    model_test "testing for cmd: model cmd1" cmd1 "resources/vgg16.ot";
    gif_test "testing for cmd: gif cmd1" cmd1 "data/output/art";
  ]

let cmd2 =
  parse_make "default" "default" "vgg16" "-learning_rate 2.0" "cmd"

let test_cmd2 =
  [
    content_test "testing for cmd: content cmd2" cmd2 "data/default.jpg";
    style_test "testing for cmd: style cmd2" cmd2 "data/default.jpg";
    flags_test "testing for cmd: flags cmd2" cmd2
      { default with learning_rate = 2.0 };
    output_test "testing for cmd: output cmd2" cmd2
      "data/output/cmd2.png";
    model_test "testing for cmd: model cmd2" cmd2 "resources/vgg16.ot";
    gif_test "testing for cmd: gif cmd2" cmd2 "data/output/cmd2";
  ]

let cmd3 =
  parse_make "default" "default" "vgg19"
    "-layers_style_loss [1, 2, 3, 5, 8] -layers_content_loss [2,3] \
     -style_weight 1e7 -learning_rate 10 -total_steps 100"
    "cmd3"

let test_cmd3 =
  [
    content_test "testing for cmd: content cmd3, expected default.jpg"
      cmd3 "data/default.jpg";
    style_test "testing for cmd:  style cmd3" cmd3 "data/default.jpg";
    flags_test "testing for cmd: flags cmd3" cmd3
      {
        default with
        layers_style_loss = [ 1; 2; 3; 5; 8 ];
        layers_content_loss = [ 2; 3 ];
        style_weight = 1e7;
        learning_rate = 10.;
        total_steps = 100;
      };
    output_test "testing for cmd: output cmd3" cmd3
      "data/output/cmd3.png";
    model_test "testing for cmd: model cmd3" cmd3 "resources/vgg19.ot";
    gif_test "testing for cmd: gif cmd3" cmd3 "data/output/cmd3";
  ]

let cmd4 =
  parse_make "default" "default" "vgg19" "-learning_rate 0.5" "cmd4"

let test_cmd4 =
  [
    content_test "content cmd4" cmd4 "data/default.jpg";
    style_test "style cmd4" cmd4 "data/default.jpg";
    output_test "output cmd4" cmd4 "data/output/cmd4.png";
    model_test "model cmd4" cmd4 "resources/vgg19.ot";
    gif_test "gif cmd4" cmd4 "data/output/cmd4";
  ]

let cmd5 =
  parse_make "default" "default" "vgg19"
    "-layers_style_loss [2, 10, 14] -layers_content_loss [2] \
     -style_weight 2e7 -learning_rate 0.1 -total_steps 150"
    "cmd5"

let test_cmd5 =
  [
    content_test "testing for cmd: content cmd5, expected default.jpg"
      cmd5 "data/default.jpg";
    style_test "testing for cmd: style cmd5, expected default.jpg" cmd5
      "data/default.jpg";
    flags_test "testing for cmd: flags cmd5" cmd5
      {
        default with
        layers_style_loss = [ 2; 10; 14 ];
        layers_content_loss = [ 2 ];
        style_weight = 2e7;
        learning_rate = 0.1;
        total_steps = 150;
      };
    output_test "testing for cmd: output cmd5" cmd5
      "data/output/cmd5.png";
    model_test "testing for cmd: model cmd5, expected vgg19.ot" cmd5
      "resources/vgg19.ot";
    gif_test "testing for cmd: gif cmd5, expected cmd5" cmd5
      "data/output/cmd5";
  ]

let cmd6 = parse_make "default" "default" "vgg19" "" ""

let test_cmd6 =
  [
    content_test "testing for cmd: content cmd6, expected default.jpg"
      cmd6 "data/default.jpg";
    style_test "testing for cmd: style cmd6, expected default.jpg" cmd6
      "data/default.jpg";
    flags_test "testing for cmd: flags cmd6" cmd6 default;
    output_test "testing for cmd: output cmd6, expected art.png" cmd6
      "data/output/art.png";
    model_test "testing for cmd: model cmd6, expected vgg19.ot" cmd6
      "resources/vgg19.ot";
    gif_test "testing for cmd: gif cmd6, expected art" cmd6
      "data/output/art";
  ]

let file_in = "data/cornell.jpg"
let file_out = "data/output/file_out.jpg"

let img_resize_default_height_test =
  "resize cornell.jpg with img_resize_default" >:: fun _ ->
  let _ = img_resize_default file_in file_out in
  assert_equal (read_img_to_tensor file_out |> get_img_size_height) 512

let img_resize_default_width_test =
  "resize cornell.jpg with img_resize_default" >:: fun _ ->
  let _ = img_resize_default file_in file_out in
  assert_equal (read_img_to_tensor file_out |> get_img_size_width) 512

let img_resize_height_test =
  "resize cornell.jpg with img_resize by double" >:: fun _ ->
  let _ = img_resize file_in file_out 2.0 in
  assert_equal (read_img_to_tensor file_out |> get_img_size_height) 1126

let img_resize_width_test =
  "resize cornell.jpg with img_resize by double" >:: fun _ ->
  let _ = img_resize file_in file_out 2.0 in
  assert_equal (read_img_to_tensor file_out |> get_img_size_width) 2000

let blur_gaussian_width_test =
  "blur_gaussian should not change img width" >:: fun _ ->
  let _ = blur_gaussian file_in file_out 5 0.75 in
  assert_equal (read_img_to_tensor file_out |> get_img_size_width) 1000

let blur_gaussian_height_test =
  "blur_gaussian should not change img width" >:: fun _ ->
  let _ = blur_gaussian file_in file_out 5 0.75 in
  assert_equal (read_img_to_tensor file_out |> get_img_size_height) 563

let gradient_graph_height_test =
  "gradient_graph should not change img height" >:: fun _ ->
  let _ = gradient_graph file_in file_out 5 0.75 in
  assert_equal (read_img_to_tensor file_out |> get_img_size_height) 563

let gradient_graph_width_test =
  "gradient_graph should not change img width" >:: fun _ ->
  let _ = gradient_graph file_in file_out 5 0.75 in
  assert_equal (read_img_to_tensor file_out |> get_img_size_width) 1000

(* ######################################################################### *)
let command_tests1 =
  test_cmd1 @ test_cmd2 @ test_cmd3 @ test_cmd4 @ test_cmd5 @ test_cmd6
  @ [
      all_flags_test;
      flag_info_test "#### Testing:  info of style_weight"
        "style_weight" "style weight";
      flag_info_test "#### Testing:  info of learning_rate"
        "learning_rate" "learning rate";
      flag_info_test "#### Testing:  info of total_steps" "total_steps"
        "total_steps";
      flag_info_test "#### Testing:  info of layer_style_loss"
        "layers_style_loss" "layers_style_loss";
      flag_info_test "#### Testing:  info of layer_content_loss"
        "layers_content_loss" "layers_content_loss";
      flag_info_test "#### Testing:  info of k" "k" "k";
      flag_info_test "#### Testing:  info of sigma" "sigma" "sigma";
      flag_info_test "#### Testing:  info of size" "size" "size";
      parse_fail_flag
        "#### Testing:  invalid flag causes parsing to fail" "-foo 10"
        "foo";
      parse_fail_flag
        "#### Testing:  invalid flag causes parsing to fail"
        "-learning_rate 2.0 -foo 10" "foo";
      parse_fail_flag
        "#### Testing:  invalid flag causes parsing to fail"
        "-size 2.0 -sss 10" "sss";
      parse_fail_flag
        "#### Testing:  invalid flag causes parsing to fail"
        "-size 2.0 -test lll" "test";
      parse_fail_flag
        "#### Testing:  invalid flag causes parsing to fail"
        "-bar 10      learning_rate 2.0 " "bar";
      parse_fail_type
        "#### Testing:  wrong type causes parsing to fail 1"
        "-learning_rate \"123\"";
      parse_fail_type
        "#### Testing:  wrong type causes parsing to fail 2"
        "-learning_rate [1,2,3]";
      parse_fail_type
        "#### Testing:  wrong type causes parsing to fail 3"
        "-learning_rate [1.0]";
      (* default_style_weight_test; *)
      default_learning_rate_test;
      default_total_steps_test;
      default_layers_style_loss_test;
      default_layers_content_loss_test;
      default_k_test;
      default_sigma_test;
    ]

let resize_test =
  [
    img_resize_default_height_test;
    img_resize_default_width_test;
    img_resize_height_test;
    img_resize_width_test;
    blur_gaussian_height_test;
    blur_gaussian_width_test;
    gradient_graph_height_test;
    gradient_graph_width_test;
  ]

let suite =
  "test suite for project"
  >::: List.flatten [ command_tests1; resize_test ]

let _ =
  remove_clean ();
  run_test_tt_main suite;
  remove_clean ();
 
 

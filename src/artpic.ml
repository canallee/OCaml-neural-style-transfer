open Command
open Img

let tmp_file_loc name = "tmp" ^ Filename.dir_sep ^ name ^ ".jpg"

let artwork model_name cmd =
  let _ = Sys.command "mkdir GIF_tmp" in
  let _ = Sys.command "mkdir tmp" in
  let flgs = get_all_flags cmd in
  let res_style = tmp_file_loc "resize_style" in
  print_endline "Resizing... ";
  img_resize (get_style cmd) res_style flgs.size;
  Nst.main model_name res_style (get_content cmd) (get_model cmd) flgs
    (get_output cmd)

let artwork_resize_512 model_name cmd =
  let res_cont = tmp_file_loc "resize_content" in
  let flgs = get_all_flags cmd in
  let res_style = tmp_file_loc "resize_style" in
  let _ = Sys.command "mkdir tmp" in
  let _ = Sys.command "mkdir GIF_tmp" in
  print_endline "Resizing... ";
  img_resize_default (get_content cmd) res_cont;
  img_resize_default (get_style cmd) res_style;
  print_endline "Blurring... ";
  Nst.main model_name res_style res_cont (get_model cmd) flgs
    (get_output cmd)

let picture model_name cmd =
  let flgs = get_all_flags cmd in
  let gaus_cont = tmp_file_loc "gaussian_content" in
  let grad = tmp_file_loc "gradient" in
  let _ = Sys.command "mkdir tmp" in
  let _ = Sys.command "mkdir GIF_tmp" in
  print_endline "Generating gradient... ";
  gradient_graph (get_style cmd) grad flgs.k flgs.sigma;
  print_endline "Blurring... ";
  blur_gaussian (get_content cmd) gaus_cont flgs.k flgs.sigma;
  Nst.main model_name grad gaus_cont (get_model cmd) flgs
    (get_output cmd)

let picture_resize_512 model_name cmd =
  let res_cont = tmp_file_loc "resize_content" in
  let flgs = get_all_flags cmd in
  let res_style = tmp_file_loc "resize_style" in
  let gaus_cont = tmp_file_loc "gaussian_content" in
  let grad = tmp_file_loc "gradient" in
  let _ = Sys.command "mkdir tmp" in
  let _ = Sys.command "mkdir GIF_tmp" in
  print_endline "Resizing... ";
  img_resize_default (get_content cmd) res_cont;
  img_resize_default (get_style cmd) res_style;
  print_endline "Generating gradient... ";
  gradient_graph res_style grad flgs.k flgs.sigma;
  print_endline "Blurring... ";
  blur_gaussian res_cont gaus_cont flgs.k flgs.sigma;
  Nst.main model_name grad gaus_cont (get_model cmd) flgs
    (get_output cmd)
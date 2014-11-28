.align 8
.globl min_caml_n_objects
min_caml_n_objects:
	.space	8
.align 8
.globl min_caml_objects
min_caml_objects:
	.space	480
.align 8
.globl min_caml_screen
min_caml_screen:
	.space	24
.align 8
.globl min_caml_viewpoint
min_caml_viewpoint:
	.space	24
.align 8
.globl min_caml_light
min_caml_light:
	.space	24
.align 8
.globl min_caml_beam
min_caml_beam:
	.space	8
.align 8
.globl min_caml_and_net
min_caml_and_net:
	.space	200
.align 8
.globl min_caml_or_net
min_caml_or_net:
	.space	4
.align 8
.globl min_caml_solver_dist
min_caml_solver_dist:
	.space	8
.align 8
.globl min_caml_intsec_rectside
min_caml_intsec_rectside:
	.space	4
.align 8
.globl min_caml_tmin
min_caml_tmin:
	.space	8
.align 8
.globl min_caml_intersection_point
min_caml_intersection_point:
	.space	24
.align 8
.globl min_caml_intersected_object_id
min_caml_intersected_object_id:
	.space	24
.align 8
.globl min_caml_nvector
min_caml_nvector:
	.space	24
.align 8
.globl min_caml_texture_color
min_caml_texture_color:
	.space	24
.align 8
.globl min_caml_diffuse_ray
min_caml_diffuse_ray:
	.space	24
.globl min_caml_rgb
min_caml_rgb:
	.space	24
.align 8
.globl min_caml_image_size
min_caml_image_size:
	.space	8
.globl min_caml_image_center
min_caml_image_center:
	.space	8
.align 8
.globl min_caml_scan_pitch
min_caml_scan_pitch:
	.space	8
.align 8
.globl min_caml_startp
min_caml_startp:
	.space	24
.align 8
.globl min_caml_startp_fast
min_caml_startp_fast:
	.space	24
.align 8
.globl min_caml_screenx_dir
min_caml_screenx_dir:
	.space	24
.align 8
.globl min_caml_screeny_dir
min_caml_screeny_dir:
	.space	24
.align 8
.globl min_caml_screenz_dir
min_caml_screenz_dir:
	.space	24
.align 8
.globl min_caml_ptrace_dirvec
min_caml_ptrace_dirvec:
	.space	24
.align 8
.globl min_caml_dirvecs
min_caml_dirvecs:
	.space	40
.align 8
.globl min_caml_light_dirvec_v3
min_caml_light_dirvec_v3:
	.space	24
.align 8
.globl min_caml_light_dirvec_consts
min_caml_light_dirvec_consts:
	.space	480
.align 8
.globl min_caml_light_dirvec
min_caml_light_dirvec2:
min_caml_light_dirvec:
	.long min_caml_light_dirvec_v3
	.long min_caml_light_dirvec_consts
	# .space	16
.globl min_caml_reflections
min_caml_reflections:
	.space	1440
.globl min_caml_n_reflections
min_caml_n_reflections:
	.space	8

open OUnit2
open Canvas
open Color

let tests = "Test Suite for Canvas" >::: [
    "Creating a canvas" >::
    (fun _ ->
        let c = createCanvas 10 20 in
        let all_black_1d = Array.fold_left 
                            (fun ok pixel -> ok && equalColor pixel black)
                            true in 
        let all_black_2d = Array.fold_left
                            (fun ok array1d -> ok && all_black_1d array1d)
                            true in
        assert_equal c.width 10;
        assert_equal c.height 20;
        assert_bool "all pixels are not black" 
            (all_black_2d c.pixels);
    );
    "Writing pixels to a canvas" >::
    (fun _ ->
        let c = createCanvas 10 20 in
        write_pixel c 2 3 red;
        assert_bool "writing pixel failed" 
            (equalColor (pixel_at c 2 3) red);
    );
    "Constructing the PPM header" >::
    (fun _ ->
        let c = createCanvas 5 3 in 
        let ppm = canvasToPPM c in 
        let lines = String.split_on_char '\n' ppm in
        match lines with 
        | l1 :: l2 :: l3 :: _ -> 
            assert_bool "line 1 failed" (l1 = "P3");
            assert_bool "line 2 failed" (l2 = "5 3");
            assert_bool "line 3 failed" (l3 = "255");
        | _ -> assert_bool "failed" false;
    );
    "Constructing the PPM pixel data" >::
    (fun _ ->
        let c = 
        (
            let c53 = createCanvas 5 3 in 
            let c1 = color 1.5 0. 0. in 
            let c2 = color 0. 0.5 0. in
            let c3 = color (-0.5) 0. 1. in
            write_pixel c53 0 0 c1;
            write_pixel c53 2 1 c2;
            write_pixel c53 4 2 c3;
            c53
        ) in

        let ppm = canvasToPPM c in 
        let lines = String.split_on_char '\n' ppm in
        
        match lines with 
        | _ :: _ :: _ :: l4 :: l5 :: l6 :: _ -> 
            assert_bool "line 4 failed" 
                (l4 = "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
            assert_bool "line 5 failed" 
                (l5 = "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0");
            assert_bool "line 6 failed"
                (l6 = "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255");
        | _ -> assert_bool "failed" false;
    );
    "Splitting long lines in PPM files" >::
    (fun _ ->
        let col = color 1. 0.8 0.6 in
        let wideCanvas = createCanvasColor 10 2 col in 
        let ppm = canvasToPPM wideCanvas in 
        let lines = String.split_on_char '\n' ppm in
        
        match lines with 
        | _ :: _ :: _ :: l4 :: l5 :: l6 :: l7 :: _ -> 
            assert_bool "line 4 failed" 
                (l4 = "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153");
            assert_bool "line 5 failed" 
                (l5 = "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153");
            assert_bool "line 6 failed" 
                (l6 = "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153");
            assert_bool "line 7 failed" 
                (l7 = "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153");
        | _ -> assert_bool "failed" false;
    );
    "PPM files are terminated by a newline" >::
    (fun _ ->
        let c = createCanvas 5 3 in
        let ppm = canvasToPPM c in 
        assert_equal (ppm.[String.length ppm - 1]) '\n';
    );
]

let _ = run_test_tt_main tests

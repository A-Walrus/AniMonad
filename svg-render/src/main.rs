use std::{
    io::Write,
    process::{Command, Stdio},
    sync::mpsc::channel,
    thread,
};

use rayon::prelude::*;
fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 6 {
        println!("Usage:\n\tminimal <dir> <fps> <num-frames> <width> <height>");
        return;
    }

    let dir = args[1].clone();
    let fps = args[2].clone();
    let num_frames: usize = args[3].parse().unwrap();
    let width: usize = args[4].parse().unwrap();
    let height: usize = args[5].parse().unwrap();

    let mut opt = usvg::Options::default();
    opt.resources_dir = None;
    opt.fontdb_mut().load_system_fonts();

    let (tx, rx) = channel();
    let handle = thread::spawn(move || {
        let mut ffmpeg = Command::new("ffmpeg")
            .args(&[
                "-y",
                "-f",
                "rawvideo",
                "-pix_fmt",
                "rgba",
                "-s",
                &format!("{}x{}", width, height),
                "-r",
                &fps,
                "-i",
                "-",
                "-c:v",
                "libx264",
                "-pix_fmt",
                "yuv420p",
                "output.mp4",
            ])
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("Failed to start ffmpeg");
        let stdin = ffmpeg.stdin.as_mut().expect("Failed to open stdin");
        let mut frame_cache: Vec<(usize, Vec<u8>)> = Vec::new();
        let mut frame = 0;
        while frame < num_frames {
            let a: (usize, Vec<u8>) = rx.recv().unwrap();

            // Insert the element at the correct position
            frame_cache.insert(
                frame_cache
                    .binary_search_by(|&(x, _)| x.cmp(&a.0).reverse())
                    .unwrap_or_else(|e| e),
                a,
            );

            while frame_cache.last().is_some_and(|x| x.0 == frame) {
                let current_frame = frame_cache.pop().unwrap();
                stdin
                    .write_all(&current_frame.1)
                    .expect("Failed to write data");
                frame += 1;
            }
        }
    });
    (0..num_frames).into_par_iter().for_each(|i| {
        let svg_data = std::fs::read(format!("{dir}/{i}.svg")).unwrap();
        let tree = usvg::Tree::from_data(&svg_data, &opt).unwrap();

        let pixmap_size = tree.size().to_int_size();
        let mut pixmap = tiny_skia::Pixmap::new(pixmap_size.width(), pixmap_size.height()).unwrap();
        resvg::render(&tree, tiny_skia::Transform::default(), &mut pixmap.as_mut());
        tx.send((i, pixmap.take())).unwrap();
    });
    handle.join().expect("Failed to join");
}

use std::{
    io::{Read, Write},
    process::{Command, Stdio},
    sync::mpsc::channel,
    thread,
};

use rayon::prelude::*;
fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 5 {
        println!("Usage:\n\tsvg-render <fps> <num-frames> <width> <height>");
        return;
    }

    let fps = args[1].clone();
    let num_frames: usize = args[2].parse().unwrap();
    let width: usize = args[3].parse().unwrap();
    let height: usize = args[4].parse().unwrap();

    let mut opt = usvg::Options::default();
    opt.resources_dir = None;
    opt.fontdb_mut().load_system_fonts();

    let (tx, rx) = channel();
    let ffmpeg_handle = thread::spawn(move || {
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
        let ffmpeg_stdin = ffmpeg.stdin.as_mut().expect("Failed to open stdin");
        let mut frame_cache: Vec<Option<Vec<u8>>> = vec![None; num_frames];
        let mut frame = 0;
        while frame < num_frames {
            let (index, data): (usize, Vec<u8>) = rx.recv().unwrap();
            frame_cache[index] = Some(data);
            while frame < num_frames && frame_cache[frame].is_some() {
                let current_frame = frame_cache[frame].take().unwrap();
                ffmpeg_stdin
                    .write_all(&current_frame)
                    .expect("Failed to write data");
                frame += 1;
            }
        }
    });

    let mut buffer = String::new();
    let stdin = std::io::stdin();
    let mut stdin_handle = stdin.lock();

    stdin_handle
        .read_to_string(&mut buffer)
        .expect("Failed to read from STDIN");

    buffer
        .trim()
        .split("\n\n")
        .enumerate()
        .par_bridge()
        .into_par_iter()
        .for_each(|(i, svg)| {
            let tree = usvg::Tree::from_data(svg.as_bytes(), &opt).expect("Failed to parse SVG");

            let pixmap_size = tree.size().to_int_size();
            let mut pixmap =
                tiny_skia::Pixmap::new(pixmap_size.width(), pixmap_size.height()).unwrap();
            resvg::render(&tree, tiny_skia::Transform::default(), &mut pixmap.as_mut());
            tx.send((i, pixmap.take())).unwrap();
        });
    ffmpeg_handle.join().expect("Failed to join");
}

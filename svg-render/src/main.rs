use rayon::prelude::*;
fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage:\n\tminimal <dir>");
        return;
    }

    let paths: Vec<_> = std::fs::read_dir(&args[1]).unwrap().collect();

    let mut opt = usvg::Options::default();
    opt.resources_dir = None;
    opt.fontdb_mut().load_system_fonts();

    paths.into_par_iter().for_each(|path| {
        let mut path = path.unwrap().path();
        let svg_data = std::fs::read(&path).unwrap();
        let tree = usvg::Tree::from_data(&svg_data, &opt).unwrap();

        let pixmap_size = tree.size().to_int_size();
        let mut pixmap = tiny_skia::Pixmap::new(pixmap_size.width(), pixmap_size.height()).unwrap();
        resvg::render(&tree, tiny_skia::Transform::default(), &mut pixmap.as_mut());
        path.set_extension("png");
        pixmap.save_png(path).unwrap();
    });
}
